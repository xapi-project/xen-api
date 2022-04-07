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

let component_of_string = function
  | "Xen" | "xen" ->
      Xen
  | "Kernel" | "kernel" ->
      Kernel
  | _ ->
      raise Invalid_component

let pkg_name_of_component = function
  | Xen ->
      "xen-hypervisor"
  | Kernel ->
      "kernel"

let string_of_component = function Xen -> "xen" | Kernel -> "kernel"

type t = {
    component: component
  ; base_build_id: string
  ; base_version: string
  ; base_release: string
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
    ; ("base_version", `String lp.base_version)
    ; ("base_release", `String lp.base_release)
    ; ("to_version", option_to_json lp.to_version)
    ; ("to_release", option_to_json lp.to_release)
    ]

let of_json js =
  try
    let open Yojson.Basic.Util in
    {
      component= member "component" js |> to_string |> component_of_string
    ; base_build_id= member "base_build_id" js |> to_string
    ; base_version= member "base_version" js |> to_string
    ; base_release= member "base_release" js |> to_string
    ; to_version= member "to_version" js |> to_string_option
    ; to_release= member "to_release" js |> to_string_option
    }
  with e ->
    let msg = "Can't construct a livepatch from json" in
    error "%s: %s" msg (ExnHelper.string_of_exn e) ;
    raise Api_errors.(Server_error (internal_error, [msg]))

let get_latest_livepatch lps =
  List.map (fun (_, _, t_v, t_r) -> (t_v, t_r)) lps
  |> Rpm.get_latest_version_release
  |> function
  | Some (v, r) ->
      List.find_opt (fun (_, _, t_v, t_r) -> t_v = v && t_r = r) lps
  | None ->
      None

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
           match acc_flag with
           | true ->
               (acc, true)
           | false ->
               if
                 Astring.String.is_prefix ~affix:"Installed patch modules:" line
               then
                 (acc, true)
               else
                 (line :: acc, false)
         )
         ([], false)
    |> (fun (x, _) -> x)
    |> List.filter_map (fun line ->
           let replace s =
             Astring.String.cuts ~sep:"-" s |> Astring.String.concat ~sep:"."
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

  let get_base_build_id () =
    let build_id_len = 20 in
    let build_id_note_id = 3 in
    let path = "/sys/kernel/notes" in
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    let len = Unix.lseek fd 0 Unix.SEEK_END + 1 in
    let bytes_buff = Bytes.make len '\000' in
    ignore (Unix.lseek fd 0 Unix.SEEK_SET) ;
    ignore (Unix.read fd bytes_buff 0 (Bytes.length bytes_buff)) ;
    Unix.close fd ;
    let buff = Cstruct.of_bytes bytes_buff in
    let align x =
      (* The actual size is aligned with 4. *)
      let open Int32 in
      if rem x 4l = 0l then to_int x else to_int (add x (sub 4l (rem x 4l)))
    in
    let getters = [(4, Cstruct.LE.get_uint32)] in
    let headers_struct =
      [("name_size", 4, true); ("desc_size", 4, true); ("note_type", 4, false)]
    in
    let rec read_build_id buff =
      let rec read_headers acc buff hds =
        match (Cstruct.length buff, hds) with
        | buff_len, (hd_name, hd_len, aligned) :: remaining
          when buff_len >= hd_len ->
            let v = (List.assoc hd_len getters) buff 0 in
            let hd = (hd_name, if aligned then align v else Int32.to_int v) in
            read_headers (hd :: acc) (Cstruct.shift buff hd_len) remaining
        | _ ->
            (acc, buff)
      in
      try
        let headers, buff = read_headers [] buff headers_struct in
        let name =
          Cstruct.to_string ~len:(List.assoc "name_size" headers) buff
          |> Astring.String.trim ~drop:Astring.Char.Ascii.is_control
        in
        let buff = Cstruct.shift buff (List.assoc "name_size" headers) in
        match
          (name, List.assoc "note_type" headers, List.assoc "desc_size" headers)
        with
        | "GNU", tid, len when tid = build_id_note_id && len = build_id_len ->
            Some (Cstruct.to_bytes ~len:build_id_len buff)
        | _ ->
            read_build_id (Cstruct.shift buff (List.assoc "desc_size" headers))
      with Not_found | Invalid_argument _ -> None
    in
    match read_build_id buff with
    | Some build_id_bytes ->
        (* For each byte (b7 b6 b5 b4 b4 b3 b1 b0), split it into two bytes:
         * [0 0 0 0 b7 b6 b5 b4], and [0 0 0 0 b3 b2 b1 b0].
         * Then change these 2 bytes to strings.
         *)
        let byte_to_string x =
          if x >= 10 then
            Char.escaped (Char.chr (x - 10 + Char.code 'a'))
          else
            Char.escaped (Char.chr (x + Char.code '0'))
        in
        Bytes.fold_left
          (fun acc c ->
            let code = Char.code c in
            let hi = Int.shift_right code 4 |> byte_to_string in
            let lo = Int.logand code 15 |> byte_to_string in
            acc @ [hi; lo]
          )
          [] build_id_bytes
        |> Astring.String.concat ~sep:""
        |> fun x ->
        debug "Kernel build id: %s" x ;
        x |> Option.some
    | None ->
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
    let r = Re.Posix.compile_pat {|^[ ]*build_id[ ]*:[ ]*([a-z0-9]+).*$|} in
    Helpers.call_script !Xapi_globs.xl_cmd ["info"]
    |> Astring.String.cuts ~sep:"\n"
    |> List.filter_map (fun line ->
           match Re.exec_opt r line with
           | Some groups ->
               Some (Re.Group.get groups 1)
           | None ->
               None
       )
    |> function
    | [build_id] ->
        debug "Xen build id: %s" build_id ;
        Some build_id
    | _ ->
        None
end

let get_installed_version_release ~component =
  let repoquery_sep = ":|" in
  let fmt =
    ["version"; "release"]
    |> List.map (fun field -> "%{" ^ field ^ "}")
    |> Astring.String.concat ~sep:repoquery_sep
  in
  let pkg_name = pkg_name_of_component component in
  let parse_line line =
    match Astring.String.cuts ~sep:repoquery_sep line with
    | [version; release] ->
        debug "Installed %s: %s-%s" pkg_name version release ;
        Some (version, release)
    | _ ->
        warn "Can't parse line of repoquery '%s'" line ;
        None
  in
  let params = ["--pkgnarrow=installed"; "--qf"; fmt; pkg_name] in
  Helpers.call_script !Xapi_globs.repoquery_cmd params
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map parse_line
  |> function
  | [(version, release)] ->
      Some (version, release)
  | _ ->
      None

let get_livepatch_dir ~component ~base_build_id =
  let comp_str = string_of_component component in
  Printf.sprintf "/usr/lib/%s-livepatch/%s" comp_str base_build_id

let get_installed_livepatch_file ~component ~base_build_id =
  let lp_dir = get_livepatch_dir ~component ~base_build_id in
  let installed_symlink = Filename.concat lp_dir "livepatch.livepatch" in
  if not (Sys.file_exists installed_symlink) then
    None
  else if not ((Unix.lstat installed_symlink).Unix.st_kind = Unix.S_LNK) then
    None
  else
    Unix.readlink installed_symlink |> Filename.concat lp_dir |> Option.some

let get_applied_livepatch ~component ~base_build_id ~running_livepatch =
  ( match (base_build_id, running_livepatch) with
  | Some base_build_id, Some (base_ver, base_rel, to_ver, to_rel) ->
      (* livepatch listing reports a livepatch. *)
      Some
        {
          component
        ; base_build_id
        ; base_version= base_ver
        ; base_release= base_rel
        ; to_version= Some to_ver
        ; to_release= Some to_rel
        }
  | Some base_build_id, None -> (
    match get_installed_version_release ~component with
    | Some (installed_version, installed_release) ->
        (* No running livepatch can be found from listing. *)
        Some
          {
            component
          ; base_build_id
          ; base_version= installed_version
          ; base_release= installed_release
          ; to_version= None
          ; to_release= None
          }
    | None ->
        None
  )
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
