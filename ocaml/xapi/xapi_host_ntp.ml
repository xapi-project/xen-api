(*
 * Copyright (c) Cloud Software Group, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = "xapi_host_ntp" end)

open D

let ( // ) = Filename.concat

let ntp_dhcp_server_path interface =
  Printf.sprintf "%s/%s.sources" !Xapi_globs.ntp_dhcp_dir interface

let add_exec_permission fname =
  let st = Unix.stat fname in
  let perm = st.Unix.st_perm lor 0o111 in
  Unix.chmod fname perm

let remove_exec_permission fname =
  let st = Unix.stat fname in
  let perm = st.Unix.st_perm land lnot 0o111 in
  Unix.chmod fname perm

let get_dhclient_interfaces () =
  let extract_interface_name filename =
    try Scanf.sscanf filename "dhclient-%[^.].leases" (fun x -> Some x)
    with _ -> None
  in
  Sys.readdir "/var/lib/xcp"
  |> Array.to_list
  |> List.filter_map extract_interface_name

let get_dhcp_ntp_server interface =
  let fname = Printf.sprintf "/var/lib/xcp/dhclient-%s.leases" interface in
  Xapi_stdext_unix.Unixext.read_lines ~path:fname
  |> List.rev (* search from the last lease entry *)
  |> List.find_map (fun line ->
         let line = String.trim line in
         try Scanf.sscanf line "option ntp-servers %[^;];" (fun x -> Some x)
         with _ -> None
     )

let add_dhcp_ntp_servers () =
  let ntp_dhcp_dir = !Xapi_globs.ntp_dhcp_dir in
  if not (Sys.file_exists ntp_dhcp_dir && Sys.is_directory ntp_dhcp_dir) then
    Xapi_stdext_unix.Unixext.mkdir_rec ntp_dhcp_dir 0o755 ;
  get_dhclient_interfaces ()
  |> List.iter (fun interface ->
         match get_dhcp_ntp_server interface with
         | Some server ->
             let line = Printf.sprintf "server %s iburst prefer\n" server in
             Xapi_stdext_unix.Unixext.write_string_to_file
               (ntp_dhcp_server_path interface)
               line
         | None ->
             ()
     ) ;
  add_exec_permission !Xapi_globs.ntp_dhcp_script

let remove_dhcp_ntp_servers () =
  remove_exec_permission !Xapi_globs.ntp_dhcp_script ;
  Sys.readdir !Xapi_globs.ntp_dhcp_dir
  |> Array.iter (fun fname ->
         if String.ends_with ~suffix:".sources" fname then
           Sys.remove (!Xapi_globs.ntp_dhcp_dir // fname)
     )

let enable_ntp_service () =
  Helpers.call_script !Xapi_globs.timedatectl ["set-ntp"; "true"] |> ignore

let disable_ntp_service () =
  Helpers.call_script !Xapi_globs.timedatectl ["set-ntp"; "false"] |> ignore

let restart_ntp_service () =
  (* Make sure the NTP service is enabled before restarting, and the timedatectl
     ntp enabled status is consistent with the service state *)
  enable_ntp_service () ;
  Xapi_systemctl.restart ~wait_until_success:false !Xapi_globs.ntp_service

let is_ntp_service_active () =
  Fe_systemctl.is_active ~service:!Xapi_globs.ntp_service

let parse_ntp_conf () =
  try
    Xapi_stdext_unix.Unixext.read_lines ~path:!Xapi_globs.ntp_conf
    |> List.partition (String.starts_with ~prefix:"server ")
  with Sys_error _ -> ([], [])

let write_ntp_conf other servers =
  let lines = List.map (fun s -> Printf.sprintf "server %s iburst" s) servers in
  let all_lines = other @ lines in
  let write_lines fname lines =
    Xapi_stdext_unix.Unixext.write_string_to_file fname
      (String.concat "\n" lines ^ "\n")
  in
  write_lines !Xapi_globs.ntp_conf all_lines

let set_servers_in_conf servers =
  let _, other = parse_ntp_conf () in
  write_ntp_conf other servers

let clear_servers_in_conf () = set_servers_in_conf []

let get_servers_from_conf () =
  let servers, _ = parse_ntp_conf () in
  List.filter_map
    (fun line ->
      try Scanf.sscanf line "server %s@ iburst%!" Option.some with _ -> None
    )
    servers

let is_ntp_dhcp_enabled () =
  (* check ntp_dhcp_script exec permission *)
  try
    let stat = Unix.stat !Xapi_globs.ntp_dhcp_script in
    stat.Unix.st_perm land 0o100 <> 0
  with _ -> false

(* chronyc -c sources output the ntp servers status in csv format. Example:
    ^,-,10.62.16.11,5,6,377,54,0.000496471,0.000496471,0.071449950
    ^,?,17.253.14.123,0,8,0,4294967295,0.000000000,0.000000000,0.000000000
    ^,?,104.40.149.189,0,8,0,4294967295,0.000000000,0.000000000,0.000000000
    ^,*,10.71.56.11,5,6,377,57,-0.000006851,-0.000118707,0.082518920
   Source mode: '^' = server, '=' = peer, '#' = local clock.
   Source state: '*' = current synced, '+' = combined, '-' = not combined,
        '?' = unreachable, 'x' = time may be in error, '~' = time too variable
*)
let get_servers_status () =
  let convert = function
    | "*" ->
        "synced"
    | "+" ->
        "combined"
    | "-" ->
        "uncombined"
    | "x" ->
        "error"
    | "~" ->
        "variable"
    | "?" ->
        "unreachable"
    | _ ->
        "unknown"
  in
  let r = Helpers.call_script !Xapi_globs.ntp_client_path ["-c"; "sources"] in
  let lines = String.split_on_char '\n' r in
  List.filter_map
    (fun line ->
      line |> String.trim |> String.split_on_char ',' |> function
      | "^" :: status :: server :: _ ->
          Some (server, convert status)
      | _ ->
          None
    )
    lines

let promote_legacy_default_servers () =
  let servers = get_servers_from_conf () in
  let legacy = !Xapi_globs.legacy_factory_ntp_servers in
  let defaults = !Xapi_globs.factory_ntp_servers in
  if
    legacy <> []
    && defaults <> []
    && Xapi_stdext_std.Listext.List.set_equiv servers legacy
  then (
    info "Promoting legacy default NTP servers (%s) to new default servers (%s)"
      (String.concat "," servers)
      (String.concat "," defaults) ;
    set_servers_in_conf defaults ;
    restart_ntp_service ()
  )

let is_synchronized () =
  let patterns = ["System clock synchronized: yes"; "NTP synchronized: yes"] in
  try
    Helpers.call_script !Xapi_globs.timedatectl ["status"]
    |> String.split_on_char '\n'
    |> List.exists ((Fun.flip List.mem) patterns)
    |> Result.ok
  with e -> Error (ExnHelper.string_of_exn e)
