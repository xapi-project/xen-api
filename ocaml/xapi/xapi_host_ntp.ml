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

let dhcp_line () = Format.sprintf "sourcedir %s" !Xapi_globs.ntp_dhcp_dir

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
  let all_lines =
    try Xapi_stdext_unix.Unixext.read_lines ~path:!Xapi_globs.ntp_conf
    with Sys_error _ -> []
  in
  let server_lines, other_lines =
    all_lines |> List.partition (String.starts_with ~prefix:"server ")
  in
  let dhcp_lines, other_lines =
    other_lines |> List.partition (String.starts_with ~prefix:(dhcp_line ()))
  in
  (server_lines, dhcp_lines, other_lines)

let write_ntp_conf other dhcp servers =
  let lines = List.map (fun s -> Printf.sprintf "server %s iburst" s) servers in
  let all_lines = other @ dhcp @ lines in
  let write_lines fname lines =
    Xapi_stdext_unix.Unixext.write_string_to_file fname
      (String.concat "\n" lines ^ "\n")
  in
  write_lines !Xapi_globs.ntp_conf all_lines

let add_dhcp_ntp_servers () =
  let servers, _, other = parse_ntp_conf () in
  write_ntp_conf other [dhcp_line ()] servers

let remove_dhcp_ntp_servers () =
  let servers, _, other = parse_ntp_conf () in
  write_ntp_conf other [] servers

let set_servers_in_conf servers =
  let _, dhcp, other = parse_ntp_conf () in
  write_ntp_conf other dhcp servers

let clear_servers_in_conf () = set_servers_in_conf []

let get_servers_from_conf () =
  let servers, _, _ = parse_ntp_conf () in
  List.filter_map
    (fun line ->
      try Scanf.sscanf line "server %s@ iburst%!" Option.some with _ -> None
    )
    servers

let is_ntp_dhcp_enabled () =
  let _, dhcp, _ = parse_ntp_conf () in
  dhcp <> []

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

let update_system_layout ~__context ~host =
  let ntp_layout_version =
    Localdb.get_int Constants.ntp_system_layout_version
    |> Option.value ~default:1
  in
  debug "Current NTP system layout: %d" ntp_layout_version ;
  if ntp_layout_version < 2 then (
    (*
     * in previous layout:
     * - ntp_dhcp_script filemode was +x only when ntp-mode=DHCP (and -x else)
     * - possible presence of `sourcedir` lines in ntp_conf (even if not in ntp-mode=DHCP)
     *)
    info "Updating NTP system layout to 2" ;

    (* update ntp_dhcp_script filemode : always +x *)
    let ntp_dhcp_script = !Xapi_globs.ntp_dhcp_script in
    let hook_st = Unix.stat ntp_dhcp_script in
    let hook_perm = hook_st.Unix.st_perm in
    if hook_perm land 0o100 = 0 then
      Unix.chmod ntp_dhcp_script (hook_perm lor 0o111) ;

    (* chrony.conf rewrite to take care of `sourcedir` *)
    let expected_mode = Db.Host.get_ntp_mode ~__context ~self:host in
    if expected_mode <> `DHCP then
      let servers, _, others = parse_ntp_conf () in
      write_ntp_conf others [] servers
    else
      add_dhcp_ntp_servers () ;

    Localdb.put Constants.ntp_system_layout_version (string_of_int 2)
  )
