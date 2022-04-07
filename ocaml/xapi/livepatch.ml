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

let pkg_name_of_component = function
  | Xen ->
      "xen-hypervisor"
  | Kernel ->
      "kernel"

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
    any_uint8 >>= fun c -> return (Printf.sprintf "%0x" c)

  let all_bytes = Angstrom.many one_byte
end

module KernelLivePatch = struct
  let get_running_livepatch () =
    let r =
      (* This expects the kernel livepatch module name should be built as:
       * livepatch_<base_version>__<base_release>__<to_version>__<to_releaes>
       * The "." in <base_version>, <base_release>, <to_version>, and <to_releaes>
       * should be replaced with "_".
       * For example:
       * livepatch_4_19_19__8_0_20__4_19_19__8_0_22
       *)
      Re.Posix.compile_pat
        {|^[ ]*livepatch_([^ \[]+)__([^ \[]+)__([^ \[]+)__([^ \[]+).*$|}
    in
    Helpers.call_script !Xapi_globs.kpatch_cmd ["list"]
    |> Astring.String.cuts ~sep:"\n"
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
end

module XenLivePatch = struct
  let get_running_livepatch () =
    let r = Re.Posix.compile_pat {|^[ ]*lp_([^_ ]+)_([^_ ]+).+APPLIED.*$|} in
    Helpers.call_script !Xapi_globs.xen_livepatch_cmd ["list"]
    |> Astring.String.cuts ~sep:"\n"
    |> List.filter_map (fun line ->
           match Re.exec_opt r line with
           | Some groups -> (
               let base_vr = Re.Group.get groups 1 in
               let to_vr = Re.Group.get groups 2 in
               match
                 Astring.String.(cuts ~sep:"-" base_vr, cuts ~sep:"-" to_vr)
               with
               | [b_v; b_r], [t_v; t_r] ->
                   Some (b_v, b_r, t_v, t_r)
               | _ ->
                   None
             )
           | None ->
               None
       )
    |> get_latest_livepatch

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
end

let get_livepatch_dir ~component ~base_build_id =
  let comp_str = string_of_component component in
  Printf.sprintf "/usr/lib/%s-livepatch/%s" comp_str base_build_id

let get_installed_livepatch_file ~component ~base_build_id =
  let lp_dir = get_livepatch_dir ~component ~base_build_id in
  let installed_symlink = Filename.concat lp_dir "livepatch.livepatch" in
  match Unix.lstat installed_symlink with
  | Unix.{st_kind= S_LNK; _} ->
      Unix.readlink installed_symlink |> Filename.concat lp_dir |> Option.some
  | _ | (exception _) ->
      None

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
