(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

open Xen_api_lwt_unix

let uri = ref "http://127.0.0.1/jsonrpc"
let username = ref "root"
let password = ref "password"
let json = ref false

let exn_to_string = function
  | Api_errors.Server_error(code, params) ->
    Printf.sprintf "%s %s" code (String.concat " " params)
  | e -> Printexc.to_string e

let main () =
  let rpc = if !json then make_json !uri else make !uri in
  Session.login_with_password ~rpc ~uname:!username ~pwd:!password ~version:"1.0" ~originator:"list_vms"
  >>= fun session_id ->
  Lwt.finalize
    (fun () ->
       VM.get_all_records ~rpc ~session_id >>= fun vms ->
       List.iter
         (fun (_vm, vm_rec) ->
            Printf.printf "VM %s\n" vm_rec.API.vM_name_label
         ) vms;
       return ())
    (fun () -> Session.logout ~rpc ~session_id)

let _ =
  Arg.parse [
    "-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
    "-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
    "-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
    "-j", Arg.Set json, (Printf.sprintf "Use jsonrpc rather than xmlrpc (default %b)" !json);
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
    "Simple example which lists VMs found on a pool";

  Lwt_main.run (main ())
