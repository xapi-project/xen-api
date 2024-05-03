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

open Xen_api_client_lwt.Xen_api_lwt_unix
open Lwt.Syntax

let uri = ref "http://127.0.0.1/jsonrpc"

let username = ref "root"

let password = ref "password"

let json = ref false

let exn_to_string = function
  | Api_errors.Server_error (code, params) ->
      Printf.sprintf "%s %s" code (String.concat " " params)
  | e ->
      Printexc.to_string e

let main () =
  Lwt_switch.with_switch @@ fun switch ->
  let t =
    SessionCache.create_uri ~switch ~target:(Uri.of_string !uri)
      ~uname:!username ~pwd:!password ~version:"1.0" ~originator:"list_vms" ()
  in
  let* vms = SessionCache.with_session t @@ VM.get_all_records in
  List.iter
    (fun (_vm, vm_rec) ->
      let kind = if vm_rec.API.vM_is_a_template then "Template" else "VM" in
      Printf.printf "%s %s\n" kind vm_rec.API.vM_name_label
    )
    vms ;
  Lwt.return_unit

let _ =
  Arg.parse
    [
      ( "-uri"
      , Arg.Set_string uri
      , Printf.sprintf "URI of server to connect to (default %s)" !uri
      )
    ; ( "-u"
      , Arg.Set_string username
      , Printf.sprintf "Username to log in with (default %s)" !username
      )
    ; ( "-pw"
      , Arg.Set_string password
      , Printf.sprintf "Password to log in with (default %s)" !password
      )
    ]
    (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
    "Simple example which lists VMs found on a pool" ;

  Lwt_main.run (main ())
