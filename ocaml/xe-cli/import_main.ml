(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(*
 * import of VMs: driver which allows VMs to be imported from the local filesystem
 * (no network transport)
 *)

open Debug
open Import 

let username = ref "root"
let password = ref ""
let host = ref "127.0.0.1"
let port = ref 8086
let srid = ref ""

open Client

let rpc xml =
	let open Xmlrpcclient in
	let http = xmlrpc ~version:"1.0" "/" in
	XML_protocol ~transport:(TCP(!host, !port)) ~http xml

let _ = 
  let path = ref "" in
  Arg.parse [
    "-username", Arg.Set_string username,
    Printf.sprintf "Login username (default %s)" !username;    
    "-password", Arg.Set_string password,
    "Login password";
    "-host", Arg.Set_string host,
    Printf.sprintf "Server hostname (default %s)" !host;
    "-port", Arg.Set_int port,
    Printf.sprintf "Server port (default %d)" !port;
    "-srid", Arg.Set_string srid,
    "Import VDIs into this SR (default autodetect)";
    "-debug", Arg.Unit (fun () -> Debug.enable_all ()), "enable debugging";
  ] (fun x -> if !path = "" then path := x else Printf.printf "Warning, ignoring unknown argument: %s" x)
    "Import a VM from the local filesystem";
  let path = !path in
  if path = "" then failwith "Must supply an XVA path (file or directory) as an argument";

  let session_id: API.ref_session = Client.Session.login_with_password ~rpc ~uname:!username ~pwd:!password ~version:"1.2" ~originator:"cli" in

  let send_fn = match Import.classify path with
    | Zurich -> stream_from_xva_dir path
    | TarXVA -> stream_from_xva_file path
    | Unknown -> failwith "Failed to detect XVA type" in

  let writer (response, sock) = 
    begin match response.Http.Response.task with Some task_id -> debug(Printf.sprintf "Got task id: %s" task_id) | None -> () end;
    let oc = Unix.out_channel_of_descr sock in
    send_fn oc;
    flush oc in

  let path = Constants.import_xva_uri in
  let open Xmlrpcclient in
  let request = connect ~session_id:(Ref.string_of session_id) !host Constants.import_xva_uri in
  let transport = TCP(!host, !port) in
  with_transport transport (with_http request writer);
  debug "XVA import successful"
  

