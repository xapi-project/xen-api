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
 * HTTP handler for connecting to a VM's VNC console.
 * Handler should be passed a reference to either a VM (in which case the 'default' VNC
 * console will be chosen) or a console object.
 *)

open Http
open Stdext

module D = Debug.Make(struct let name="console" end)
open D

exception Failure

type address =
  | Port of int (* console is listening on localhost:port *)
  | Path of string (* console is listening on a Unix domain socket *)

let string_of_address = function
  | Port x -> "localhost:" ^ (string_of_int x)
  | Path x -> "unix:" ^ x

let address_of_console __context console : address option =
  let vm = Db.Console.get_VM __context console in
  let address_option =
    if Db.VM.get_is_control_domain ~__context ~self:vm
    then Some (Port (Db.Console.get_port ~__context ~self:console |> Int64.to_int))
    else begin
      try
        let open Xenops_interface in
        let id = Xapi_xenops.id_of_vm ~__context ~self:vm in
        let dbg = Context.string_of_task __context in
        let open Xapi_xenops_queue in
        let module Client = (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS) in
        let _, s = Client.VM.stat dbg id in

        let proto = match Db.Console.get_protocol __context console with
          | `rfb -> Vm.Rfb
          | `vt100 -> Vm.Vt100
          | `rdp -> failwith "No support for tunnelling RDP" in
        let console = List.find (fun x -> x.Vm.protocol = proto) s.Vm.consoles in
        Some (if console.Vm.path = "" then Port console.Vm.port else Path console.Vm.path)
      with e ->
        debug "%s" (Printexc.to_string e);
        None
    end
  in
  debug "VM %s console port: %s" (Ref.string_of vm) (Opt.default "None" (Opt.map (fun x -> "Some " ^ (string_of_address x)) address_option));
  address_option

let real_proxy __context _ _ vnc_port s =
  try
    Http_svr.headers s (Http.http_200_ok ());
    let vnc_sock = match vnc_port with
      | Port x -> Unixext.open_connection_fd "127.0.0.1" x
      | Path x -> Unixext.open_connection_unix_fd x in

    (* Unixext.proxy closes fds itself so we must dup here *)
    let s' = Unix.dup s in
    debug "Connected; running proxy (between fds: %d and %d)" (Unixext.int_of_file_descr vnc_sock) (Unixext.int_of_file_descr s');
    Unixext.proxy vnc_sock s';
    debug "Proxy exited"
  with
    exn -> debug "error: %s" (ExnHelper.string_of_exn exn)

let check_wsproxy () =
  try
    let pid = int_of_string (Unixext.string_of_file "/var/run/wsproxy.pid") in
    Unix.kill pid 0;
    true
  with _ -> false

let ensure_proxy_running () =
  if check_wsproxy () then () else begin
    ignore(Forkhelpers.execute_command_get_output "/opt/xensource/libexec/wsproxy" []);
    Thread.delay 1.0;
  end

let ws_proxy __context req protocol address s =
  let port = match address with
    | Port p -> p
    | Path _ ->
      error "No implementation for web-sockets console proxy to a Unix domain socket";
      Http_svr.headers s (Http.http_501_method_not_implemented ());
      failwith "ws_proxy: not implemented" in

  ensure_proxy_running ();
  let protocol = match protocol with
    | `rfb -> "rfb"
    | `vt100 -> "vt100"
    | `rdp -> "rdp"
  in

  let real_path = Filename.concat "/var/lib/xcp" "wsproxy" in
  let sock =
    try
      Some (Fecomms.open_unix_domain_sock_client real_path)
    with e ->
      debug "Error connecting to wsproxy (%s)" (Printexc.to_string e);
      Http_svr.headers s (Http.http_501_method_not_implemented ());
      None
  in

  (* Ensure we always close the socket *)
  Pervasiveext.finally (fun () ->
      let upgrade_successful = Opt.map (fun sock ->
          try
            let result = (sock,Some (Ws_helpers.upgrade req s)) in
            result
          with _ ->
            (sock,None)) sock
      in

      Opt.iter (function
          | (sock,Some ty) -> begin
              let wsprotocol = match ty with
                | Ws_helpers.Hixie76 -> "hixie76"
                | Ws_helpers.Hybi10 -> "hybi10" in
              let message = Printf.sprintf "%s:%s:%d" wsprotocol protocol port in
              let len = String.length message in
              ignore(Unixext.send_fd_substring sock message 0 len [] s)
            end
          | (sock,None) -> begin
              Http_svr.headers s (Http.http_501_method_not_implemented ())
            end) upgrade_successful)
    (fun () ->
       Opt.iter (fun sock -> Unix.close sock) sock)



let default_console_of_vm ~__context ~self =
  try
    let consoles = Db.VM.get_consoles ~__context ~self in
    let protocols = List.map (fun self -> Db.Console.get_protocol ~__context ~self) consoles in
    fst (List.find (fun (_, p) -> p = `rfb) (List.combine consoles protocols))
  with _ ->
    error "Failed to find default VNC console for VM";
    raise Failure

let console_of_request __context req =
  (* First check the request looks valid *)
  if not(List.mem_assoc "ref" req.Http.Request.query) && not(List.mem_assoc "uuid" req.Http.Request.query) then begin
    error "HTTP request for console forwarding lacked 'ref' or 'uuid' parameter";
    raise Failure
  end;
  let _ref =
    if List.mem_assoc "uuid" req.Http.Request.query
    then
      let uuid = List.assoc "uuid" req.Http.Request.query in
      (try Ref.string_of(Db.VM.get_by_uuid ~__context ~uuid)
       with _ -> Ref.string_of(Db.Console.get_by_uuid ~__context ~uuid))
    else List.assoc "ref" req.Http.Request.query in

  (* The _ref may be either a VM ref in which case we look for a
     default VNC console or it may be a console ref in which case we
     go for that. *)
  let db = Context.database_of __context in
  let is_vm, is_console =
    let module DB = (val (Db_cache.get db) : Db_interface.DB_ACCESS) in
    match DB.get_table_from_ref db _ref with
    | Some c when c = Db_names.vm -> true, false
    | Some c when c = Db_names.console -> false, true
    | _ ->
      error "%s is neither a VM ref or a console ref" _ref;
      raise Failure in

  if is_vm then default_console_of_vm ~__context ~self:(Ref.of_string _ref) else (Ref.of_string _ref)


let rbac_check_for_control_domain __context (req:Request.t) console_id permission =
  let is_control_domain =
    let vm_id = Db.Console.get_VM ~__context ~self:console_id in
    Db.VM.get_is_control_domain ~__context ~self:vm_id
  in
  if is_control_domain then
    let extra_dmsg = Printf.sprintf "for host console %s" (Ref.string_of console_id) in
    let session_id = Xapi_http.get_session_id req in
    Rbac.check_with_new_task ~extra_dmsg session_id permission ~fn:Rbac.nofn
      ~args:(Xapi_http.rbac_audit_params_of req)

let check_vm_is_running_here __context console =
  let vm = Db.Console.get_VM ~__context ~self:console in
  if Db.VM.get_power_state ~__context ~self:vm <> `Running then begin
    error "VM %s (Console %s) has power_state <> Running" (Ref.string_of vm) (Ref.string_of console);
    raise Failure
  end;
  let localhost = Helpers.get_localhost ~__context in
  let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
  if resident_on <> localhost then begin
    error "VM %s (Console %s) has resident_on = %s <> localhost" (Ref.string_of vm) (Ref.string_of console) (Ref.string_of resident_on);
    raise Failure
  end

(* GET /console_uri?ref=.....
   Cookie: <session id> *)
let handler proxy_fn (req: Request.t) s _ =
  req.Request.close <- true;
  Xapi_http.with_context "Connection to VM console" req s
    (fun __context ->
       let console = console_of_request __context req in
       (* only sessions with 'http/connect_console/host_console' permission *)
       let protocol = Db.Console.get_protocol ~__context ~self:console in
       (* can access dom0 host consoles *)
       rbac_check_for_control_domain __context req console
         Rbac_static.permission_http_connect_console_host_console.Db_actions.role_name_label;

       (* Check VM is actually running locally *)
       check_vm_is_running_here __context console;

       match address_of_console __context console with
       | Some vnc_port ->
         proxy_fn __context req protocol vnc_port s
       | None ->
         Http_svr.headers s (Http.http_404_missing ())
    )
