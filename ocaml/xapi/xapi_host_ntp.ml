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

let chrony_dhcp_dir = "/run/chrony-dhcp"

let chrony_dhcp_server_path interface =
  Printf.sprintf "%s/%s.sources" chrony_dhcp_dir interface

let chrony_conf = "/etc/chrony.conf"

let chrony_script = "/etc/dhcp/dhclient.d/chrony.sh"

let ( // ) = Filename.concat

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
  get_dhclient_interfaces ()
  |> List.iter (fun interface ->
         match get_dhcp_ntp_server interface with
         | Some server ->
             let line = Printf.sprintf "server %s iburst prefer\n" server in
             Xapi_stdext_unix.Unixext.write_string_to_file
               (chrony_dhcp_server_path interface)
               line
         | None ->
             ()
     ) ;
  add_exec_permission chrony_script

let remove_dhcp_ntp_servers () =
  remove_exec_permission chrony_script ;
  Sys.readdir chrony_dhcp_dir
  |> Array.iter (fun fname ->
         if String.ends_with ~suffix:".sources" fname then
           Sys.remove (chrony_dhcp_dir // fname)
     )

let restart_ntp_service () =
  Xapi_systemctl.restart ~wait_until_success:false "chronyd"

let parse_chrony_conf () =
  try
    Xapi_stdext_unix.Unixext.read_lines ~path:chrony_conf
    |> List.partition (String.starts_with ~prefix:"server ")
  with Sys_error _ -> ([], [])

let write_chrony_conf other servers =
  let lines = List.map (fun s -> Printf.sprintf "server %s iburst" s) servers in
  let all_lines = other @ lines in
  let write_lines fname lines =
    Xapi_stdext_unix.Unixext.write_string_to_file fname
      (String.concat "\n" lines ^ "\n")
  in
  write_lines chrony_conf all_lines

let set_servers_in_conf servers =
  let _, other = parse_chrony_conf () in
  write_chrony_conf other servers

let clear_servers_in_conf () = set_servers_in_conf []
