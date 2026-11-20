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

type parsed_ntp = {
    server_lines: string list
  ; dhcp_lines: string list
  ; other_lines: string list
}

let dhcp_line () = Printf.sprintf "sourcedir %s" !Xapi_globs.ntp_dhcp_dir

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
  let server_lines, other_lines =
    try
      Xapi_stdext_unix.Unixext.read_lines ~path:!Xapi_globs.ntp_conf
      |> List.partition (String.starts_with ~prefix:"server ")
    with Sys_error _ -> ([], [])
  in
  let dhcp_lines, other_lines =
    other_lines |> List.partition (String.starts_with ~prefix:(dhcp_line ()))
  in
  {server_lines; dhcp_lines; other_lines}

let write_ntp_conf {server_lines; dhcp_lines; other_lines} =
  let all_lines = other_lines @ dhcp_lines @ server_lines in
  let write_lines fname lines =
    Xapi_stdext_unix.Unixext.write_string_to_file fname
      (String.concat "\n" lines ^ "\n")
  in
  write_lines !Xapi_globs.ntp_conf all_lines

let add_dhcp_ntp_servers () =
  let parsed = parse_ntp_conf () in
  write_ntp_conf {parsed with dhcp_lines= [dhcp_line ()]}

let remove_dhcp_ntp_servers () =
  let parsed = parse_ntp_conf () in
  write_ntp_conf {parsed with dhcp_lines= []}

let set_servers_in_conf servers =
  let server_lines =
    List.map (fun s -> Printf.sprintf "server %s iburst" s) servers
  in
  let parsed = parse_ntp_conf () in
  write_ntp_conf {parsed with server_lines}

let clear_servers_in_conf () = set_servers_in_conf []

let get_servers_from_conf () =
  let {server_lines; _} = parse_ntp_conf () in
  List.filter_map
    (fun line ->
      try Scanf.sscanf line "server %s@ iburst%!" Option.some with _ -> None
    )
    server_lines

let is_ntp_dhcp_enabled () =
  let {dhcp_lines; _} = parse_ntp_conf () in
  dhcp_lines <> []

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

let update_system_layout () =
  (*
   * In the previous layout:
   * - ntp_dhcp_script filemode was +x only when ntp-mode=DHCP (and -x else)
   * - possible presence of `sourcedir` lines in ntp_conf (even if not in ntp-mode=DHCP)
   *
   * In the new layout:
   * - ntp_dhcp_script is always +x
   * - the `sourcedir` line in ntp_conf is the only marker of ntp-mode=DHCP
   *
   * This migration runs at every xapi startup and is idempotent:
   * it only changes state on a system with the previous layout
   * (-x ntp_dhcp_script) and is a no-op on fresh install or on an
   * already migrated system. If an action fails, an error is logged
   * and the migration is retried at the next startup.
   *
   * The no-op on an executable ntp_dhcp_script is intentional: an
   * executable script with no `sourcedir` line could be a previous-layout
   * system in DHCP mode where the line was manually removed, or a new-layout
   * system with ntp-mode <> DHCP. It is treated as non-DHCP, so that a fresh
   * non-DHCP install is never converted to DHCP. On such a previous-layout
   * system, the user must re-set ntp-mode=DHCP to restore the `sourcedir` line.
   *
   * Another migration edge case: if the host was booted with IP-mode=DHCP and
   * ntp-mode <> DHCP before updating, the /run/chrony-dhcp directory will only
   * be populated with the next lease renewal. Setting ntp-mode=DHCP during this
   * window will not work as expected (the NTP servers list from DHCP will be
   * empty, and only updated at the next lease).
   *)
  let hook = !Xapi_globs.ntp_dhcp_script in
  match Unix.stat hook with
  | {Unix.st_perm; _} when st_perm land 0o100 = 0 -> (
      (* Old layout: non-DHCP mode. Drop the `sourcedir` line and set +x. *)
      info "Migrating NTP layout: removing sourcedir, making %s executable" hook ;
      try
        remove_dhcp_ntp_servers () ;
        Unix.chmod hook (st_perm lor 0o111)
      with e -> error "NTP layout migration failed: %s" (Printexc.to_string e)
    )
  | _ ->
      (* New layout (or previous layout in DHCP mode): leave everything as is. *)
      ()
  | exception e ->
      (* e.g. ntp_dhcp_script is absent: nothing can be concluded, skip. *)
      warn "NTP layout migration skipped: %s" (Printexc.to_string e)
