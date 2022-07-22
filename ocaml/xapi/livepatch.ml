(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = "livepatch" end)

open D

exception Invalid_component

type component = Xen | Kernel

let component_of_string x =
  match String.lowercase_ascii x with
  | "xen" ->
      Xen
  | "kernel" ->
      Kernel
  | _ ->
      raise Invalid_component

let string_of_component = function Xen -> "xen" | Kernel -> "kernel"

(** [to_version] and [to_release] are the RPM version and release of the component reached
 * after applying the live patch.
 *)
type t = {
    component: component
  ; base_build_id: string
  ; to_version: string option
  ; to_release: string option
}

let to_json lp =
  let option_to_json o =
    Option.(value (map (fun x -> `String x) o) ~default:`Null)
  in
  `Assoc
    [
      ("component", `String (string_of_component lp.component))
    ; ("base_build_id", `String lp.base_build_id)
    ; ("to_version", option_to_json lp.to_version)
    ; ("to_release", option_to_json lp.to_release)
    ]

let of_json js =
  try
    let open Yojson.Basic.Util in
    {
      component= member "component" js |> to_string |> component_of_string
    ; base_build_id= member "base_build_id" js |> to_string
    ; to_version= member "to_version" js |> to_string_option
    ; to_release= member "to_release" js |> to_string_option
    }
  with e ->
    let msg = "Can't construct a livepatch from json" in
    error "%s - %s: %s"
      (ExnHelper.string_of_exn e)
      msg
      (Yojson.Basic.pretty_to_string js) ;
    raise Api_errors.(Server_error (internal_error, [msg]))

let get_latest_livepatch lps =
  List.map (fun (_, _, t_v, t_r) -> (t_v, t_r)) lps
  |> Rpm.get_latest_version_release
  |> function
  | Some (v, r) ->
      List.find_opt (fun (_, _, t_v, t_r) -> t_v = v && t_r = r) lps
  | None ->
      None

module BuildId = struct
  let one_byte =
    let open Angstrom in
    any_uint8 >>= fun c -> return (Printf.sprintf "%02x" c)

  let all_bytes = Angstrom.many one_byte
end

module KernelLivePatch = struct
  let get_running_livepatch' s =
    let r =
      (* This expects the kernel livepatch module name should be built as:
       * lp_<base_version>__<base_release>__<to_version>__<to_releaes>
       * The "." in <base_version>, <base_release>, <to_version>, and <to_releaes>
       * should be replaced with "_".
       * For example:
       * lp_4_19_19__8_0_20__4_19_19__8_0_22
       *)
      Re.Posix.compile_pat
        {|^[ ]*lp_([^ \[]+)__([^ \[]+)__([^ \[]+)__([^ \[]+).*$|}
    in
    Astring.String.cuts ~sep:"\n" s
    |> List.fold_left
         (fun (acc, acc_flag) line ->
           match
             ( acc_flag
             , Astring.String.is_prefix ~affix:"Installed patch modules:" line
             )
           with
           | true, _ ->
               (acc, true)
           | false, true ->
               (acc, true)
           | false, false ->
               (line :: acc, false)
         )
         ([], false)
    |> (fun (x, _) -> x)
    |> List.filter_map (fun line ->
           let replace s =
             Astring.String.cuts ~sep:"_" s |> Astring.String.concat ~sep:"."
           in
           match Re.exec_opt r line with
           | Some groups ->
               Some
                 ( Re.Group.get groups 1 |> replace
                 , Re.Group.get groups 2 |> replace
                 , Re.Group.get groups 3 |> replace
                 , Re.Group.get groups 4 |> replace
                 )
           | None ->
               None
       )
    |> get_latest_livepatch

  let get_running_livepatch () =
    Helpers.call_script !Xapi_globs.kpatch_cmd ["list"]
    |> get_running_livepatch'

  (* The actual size is aligned with 4. *)
  let align4 n = (Int32.to_int n + 3) / 4 * 4

  let rec section () =
    let open Angstrom in
    at_end_of_input >>= function
    | true ->
        fail "Not found build id"
    | false -> (
        LE.any_int32 >>= fun name_size ->
        LE.any_int32 >>= fun desc_size ->
        LE.any_int32 >>= fun note_type ->
        take (align4 name_size) >>= fun name ->
        take (align4 desc_size) >>= fun desc ->
        let name' = Astring.(String.trim ~drop:Char.Ascii.is_control name) in
        match (note_type, desc_size, name') with
        | 3l, 20l, "GNU" ->
            return desc
        | _ ->
            section ()
      )

  let get_base_build_id () =
    let open Rresult.R.Infix in
    let consume = Angstrom.Consume.Prefix in
    let path = "/sys/kernel/notes" in
    let ic = open_in path in
    Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
    let size = in_channel_length ic in
    let r =
      Angstrom.(parse_string ~consume (section ())) (really_input_string ic size)
      >>= fun build_id_bs ->
      Angstrom.(parse_string ~consume BuildId.all_bytes build_id_bs)
    in
    match r with
    | Ok ss ->
        let build_id = Astring.String.concat ~sep:"" ss in
        debug "Kernel build id: %s" build_id ;
        Some build_id
    | Error msg ->
        warn "Can't get kernel build id: %s" msg ;
        None

  let apply ~livepatch_file =
    Helpers.call_script !Xapi_globs.kpatch_cmd ["load"; livepatch_file]
    |> ignore
end

module XenLivePatch = struct
  let get_pattern status =
    Re.Posix.compile_pat
      (Printf.sprintf {|^[ ]*lp_([^- ]+)-([^- ]+)-([^- ]+)-([^- ]+).+%s.*$|}
         status
      )

  let get_livepatches pattern s =
    Astring.String.cuts ~sep:"\n" s
    |> List.filter_map (fun line ->
           match Re.exec_opt pattern line with
           | Some groups ->
               let base_version = Re.Group.get groups 1 in
               let base_release = Re.Group.get groups 2 in
               let to_version = Re.Group.get groups 3 in
               let to_release = Re.Group.get groups 4 in
               Some (base_version, base_release, to_version, to_release)
           | None ->
               None
       )

  let get_running_livepatch' s =
    let r = get_pattern "APPLIED" in
    get_livepatches r s |> get_latest_livepatch

  let get_running_livepatch () =
    Helpers.call_script !Xapi_globs.xen_livepatch_cmd ["list"]
    |> get_running_livepatch'

  let get_checked_livepatches () =
    Helpers.call_script !Xapi_globs.xen_livepatch_cmd ["list"]
    |> get_livepatches (get_pattern "CHECKED")

  let get_base_build_id () =
    let drop x =
      let open Astring.Char.Ascii in
      is_control x || is_blank x || is_white x
    in
    Helpers.call_script !Xapi_globs.xl_cmd ["info"; "build_id"]
    |> Astring.String.trim ~drop
    |> function
    | build_id when not (Astring.String.is_empty build_id) ->
        debug "Xen build id: %s" build_id ;
        Some build_id
    | _ ->
        warn "Can't get Xen build id" ;
        None

  let apply ~livepatch_file ~base_version ~base_release ~to_version ~to_release
      =
    let name =
      (* Example lp_4.13.4-10.20.xs8-4.13.4-10.21.xs8 *)
      Printf.sprintf "lp_%s-%s-%s-%s" base_version base_release to_version
        to_release
    in
    (* try to unload anyway *)
    ( try
        ignore
          (Helpers.call_script !Xapi_globs.xen_livepatch_cmd ["unload"; name])
      with _ -> ()
    ) ;
    Helpers.call_script
      !Xapi_globs.xen_livepatch_cmd
      ["upload"; name; livepatch_file]
    |> ignore ;
    Helpers.call_script !Xapi_globs.xen_livepatch_cmd ["replace"; name]
    |> ignore ;
    (* Unload obsolete livepatches.
     * The status of these livepatches is changed to 'CHECKED' from 'APPLIED' when
     * the latest livepatch is being applied.
     *)
    get_checked_livepatches ()
    |> List.iter (fun (base_version, base_release, to_version, to_release) ->
           let name =
             Printf.sprintf "lp_%s-%s-%s-%s" base_version base_release
               to_version to_release
           in
           Helpers.call_script !Xapi_globs.xen_livepatch_cmd ["unload"; name]
           |> ignore
       )
end

let get_applied_livepatch ~component ~base_build_id ~running_livepatch =
  ( match (base_build_id, running_livepatch) with
  | Some base_build_id, Some (_, _, to_ver, to_rel) ->
      (* livepatch listing reports a livepatch. *)
      Some
        {
          component
        ; base_build_id
        ; to_version= Some to_ver
        ; to_release= Some to_rel
        }
  | Some base_build_id, None ->
      Some {component; base_build_id; to_version= None; to_release= None}
  | None, _ ->
      (* Can do nothing without base build id *)
      None
  )
  |> function
  | Some lp ->
      debug "Got %s livepatch: %s"
        (string_of_component component)
        (Yojson.Basic.pretty_to_string (to_json lp)) ;
      Some lp
  | None ->
      None

let get_applied_livepatches () =
  [
    get_applied_livepatch ~component:Xen
      ~base_build_id:(XenLivePatch.get_base_build_id ())
      ~running_livepatch:(XenLivePatch.get_running_livepatch ())
  ; get_applied_livepatch ~component:Kernel
      ~base_build_id:(KernelLivePatch.get_base_build_id ())
      ~running_livepatch:(KernelLivePatch.get_running_livepatch ())
  ]
  |> List.filter_map (fun x -> x)

let get_livepatch_file_path ~component ~base_build_id ~base_version
    ~base_release ~to_version ~to_release =
  let lp_dir =
    Printf.sprintf "/usr/lib/%s-livepatch/%s"
      (string_of_component component)
      base_build_id
  in
  let suffix = match component with Xen -> "livepatch" | Kernel -> "ko" in
  let installed_symlink = Filename.concat lp_dir ("livepatch." ^ suffix) in
  match Unix.lstat installed_symlink with
  | Unix.{st_kind= S_LNK; _} -> (
      let installed_filename = Unix.readlink installed_symlink in
      let replace s = Astring.String.(cuts ~sep:"." s |> concat ~sep:"_") in
      let expected_filename =
        Printf.sprintf "lp_%s__%s__%s__%s.%s" (replace base_version)
          (replace base_release) (replace to_version) (replace to_release)
          suffix
      in
      match installed_filename = expected_filename with
      | true ->
          Some (Filename.concat lp_dir expected_filename)
      | false ->
          error "The installed livepatch file is '%s' but expected '%s'"
            installed_filename expected_filename ;
          None
    )
  | _ | (exception _) ->
      error "Invalid installed livepatch file" ;
      None

let apply ~component ~livepatch_file ~base_build_id ~base_version ~base_release
    ~to_version ~to_release =
  let assert_same_build_id ~expected ~real =
    let component_str = string_of_component component in
    match (expected, real) with
    | expected_id, Some real_id when expected_id = real_id ->
        ()
    | _ ->
        let msg =
          Printf.sprintf
            "The livepatch is against build ID %s, but the build ID of the \
             running %s is %s"
            expected component_str
            (Option.value real ~default:"None")
        in
        raise Api_errors.(Server_error (internal_error, [msg]))
  in
  match component with
  | Xen ->
      let build_id = XenLivePatch.get_base_build_id () in
      assert_same_build_id ~expected:base_build_id ~real:build_id ;
      XenLivePatch.apply ~livepatch_file ~base_version ~base_release ~to_version
        ~to_release
  | Kernel ->
      let build_id = KernelLivePatch.get_base_build_id () in
      assert_same_build_id ~expected:base_build_id ~real:build_id ;
      KernelLivePatch.apply ~livepatch_file
