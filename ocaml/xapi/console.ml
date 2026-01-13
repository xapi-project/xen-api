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
module Unixext = Xapi_stdext_unix.Unixext

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

module D = Debug.Make (struct let name = "console" end)

open D

exception Failure

type address =
  | Port of int (* console is listening on localhost:port *)
  | Path of string

(* console is listening on a Unix domain socket *)

(* This module limits VNC console sessions to at most one per VM/host.
   Depending on configuration, either unlimited connections are allowed,
   or only a single active connection per VM/host is allowed. *)
module Connection_limit = struct
  module VMMap = Map.Make (String)

  type connection_info = {user_name: string; connection_id: string}

  (* VMMap maps VM IDs to a list of connection_info records representing active connections *)
  let active_connections : connection_info list VMMap.t ref = ref VMMap.empty

  let mutex = Mutex.create ()

  let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

  let generate_connection_id () = Uuidx.to_string (Uuidx.make ())

  let drop vm_id connection_id =
    with_lock mutex (fun () ->
        let f conns =
          Option.bind conns (fun conns ->
              match
                List.filter
                  (fun conn -> conn.connection_id <> connection_id)
                  conns
              with
              | [] ->
                  None
              | l ->
                  Some l
          )
        in
        active_connections := VMMap.update vm_id f !active_connections
    )

  (* When the limit is disabled (false), we must still track the connections for each vm_id.
     This ensures that if the limit is later enabled (set to true), any existing connections are accounted for,
     and the limit can be correctly enforced for subsequent connection attempts. *)
  let try_add vm_id user_name is_limit_enabled =
    with_lock mutex (fun () ->
        let connections =
          VMMap.find_opt vm_id !active_connections |> Option.value ~default:[]
        in
        match (is_limit_enabled, connections) with
        | true, _ :: _ ->
            let connected_users =
              List.map (fun conn -> conn.user_name) connections
            in
            debug
              "limit_console_sessions is true. Console connection is rejected \
               for VM %s, active connections: %d"
              vm_id (List.length connections) ;
            Error connected_users
        | _ ->
            let connection_id = generate_connection_id () in
            let updated_connections =
              {user_name; connection_id} :: connections
            in
            active_connections :=
              VMMap.add vm_id updated_connections !active_connections ;
            Ok connection_id
    )
end

let string_of_address = function
  | Port x ->
      "localhost:" ^ string_of_int x
  | Path x ->
      "unix:" ^ x

let address_of_console __context console : address option =
  let vm = Db.Console.get_VM ~__context ~self:console in
  let address_option =
    if Db.VM.get_is_control_domain ~__context ~self:vm then
      Some (Port (Db.Console.get_port ~__context ~self:console |> Int64.to_int))
    else
      try
        let open Xenops_interface in
        let id = Xapi_xenops.id_of_vm ~__context ~self:vm in
        let dbg = Context.string_of_task __context in
        let open Xapi_xenops_queue in
        let module Client =
          (val make_client (queue_of_vm ~__context ~self:vm) : XENOPS)
        in
        let _, s = Client.VM.stat dbg id in
        let proto =
          match Db.Console.get_protocol ~__context ~self:console with
          | `rfb ->
              Vm.Rfb
          | `vt100 ->
              Vm.Vt100
          | `rdp ->
              failwith "No support for tunnelling RDP"
        in
        let console =
          List.find (fun x -> x.Vm.protocol = proto) s.Vm.consoles
        in
        Some
          ( if console.Vm.path = "" then
              Port console.Vm.port
            else
              Path console.Vm.path
          )
      with e ->
        debug "%s" (Printexc.to_string e) ;
        None
  in
  debug "VM %s console port: %s" (Ref.string_of vm)
    (Option.fold ~none:"None"
       ~some:(fun x -> "Some " ^ string_of_address x)
       address_option
    ) ;
  address_option

module Console_idle_monitor = struct
  let get_idle_timeout_config ~__context ~vm =
    try
      let idle_timeout =
        if Db.VM.get_is_control_domain ~__context ~self:vm then
          let host = Helpers.get_localhost ~__context in
          Db.Host.get_console_idle_timeout ~__context ~self:host
        else
          let pool = Helpers.get_pool ~__context in
          Db.Pool.get_vm_console_idle_timeout ~__context ~self:pool
      in
      if idle_timeout > 0L then
        Some (Int64.to_float idle_timeout)
      else
        None
    with _ -> None

  let is_active messages =
    List.exists
      (function
        | Rfb_client_msgtype_parser.KeyEvent
        | Rfb_client_msgtype_parser.PointerEvent
        | Rfb_client_msgtype_parser.QEMUClientMessage ->
            true
        | _ ->
            false
        )
      messages

  let timed_out ~idle_timeout_seconds ~last_activity =
    let elapsed = Mtime_clock.count last_activity in
    Mtime.Span.to_float_ns elapsed /. 1e9 > idle_timeout_seconds

  (* Create an idle timeout callback for the console,
     if idle timeout, then close the proxy *)
  let create_idle_timeout_callback ~__context ~vm =
    match get_idle_timeout_config ~__context ~vm with
    | Some idle_timeout_seconds -> (
        let module P = Rfb_client_msgtype_parser in
        let state = ref (Some (P.create (), Mtime_clock.counter ())) in
        (* Return true for idle timeout to close the proxy,
            otherwise return false to keep the proxy open *)
        fun (buf, read_len, offset) ->
          match !state with
          | None ->
              false
          | Some (rfb_parser, last_activity) ->
              let ok msgs =
                if is_active msgs then (
                  state := Some (rfb_parser, Mtime_clock.counter ()) ;
                  false
                ) else
                  let timeout_result =
                    timed_out ~idle_timeout_seconds ~last_activity
                  in
                  if timeout_result then
                    debug
                      "Console connection idle timeout exceeded for VM %s \
                       (timeout: %.1fs)"
                      (Ref.string_of vm) idle_timeout_seconds ;
                  timeout_result
              in
              let error msg =
                debug "RFB parse error: %s" msg ;
                state := None ;
                false
              in
              Bytes.sub_string buf offset read_len
              |> rfb_parser
              |> Result.fold ~ok ~error
      )
    | None ->
        Fun.const false
end

let get_poll_timeout =
  let poll_period_timeout = !Xapi_globs.proxy_poll_period_timeout in
  if poll_period_timeout < 0. then
    -1
  else
    Float.to_int (poll_period_timeout *. 1000.)
(* convert to milliseconds *)

let real_proxy' ~__context ~vm vnc_port s =
  try
    Http_svr.headers s (Http.http_200_ok ()) ;
    let vnc_sock =
      match vnc_port with
      | Port x ->
          Unixext.open_connection_fd "127.0.0.1" x
      | Path x ->
          Unixext.open_connection_unix_fd x
    in
    (* Unixext.proxy closes fds itself so we must dup here *)
    let s' = Unix.dup s in
    debug "Connected; running proxy (between fds: %d and %d)"
      (Unixext.int_of_file_descr vnc_sock)
      (Unixext.int_of_file_descr s') ;

    let poll_timeout = get_poll_timeout in
    let should_close =
      Console_idle_monitor.create_idle_timeout_callback ~__context ~vm
    in
    Unixext.proxy ~should_close ~poll_timeout vnc_sock s' ;
    debug "Proxy exited"
  with exn -> debug "error: %s" (ExnHelper.string_of_exn exn)

let respond_console_limit_exceeded req s vm_id connected_users =
  let body =
    let users_text =
      match
        (!Xapi_globs.include_console_username_in_error, connected_users)
      with
      | true, [] ->
          error "The connected user list should not be empty." ;
          raise Failure
      | true, [user] ->
          Printf.sprintf "User '%s' is" (Http_svr.escape user)
      | true, users ->
          let escaped_users = List.map Http_svr.escape users in
          Printf.sprintf "Users '%s' are" (String.concat ", " escaped_users)
      | false, _ ->
          "There're users"
    in
    Printf.sprintf
      "<html><body><h1>Connection Limit Exceeded</h1><p>%s currently connected \
       to this console (VM %s). No more connections are allowed when \
       limit_console_sessions is enabled.</p></body></html>"
      users_text vm_id
  in
  Http_svr.response_custom_error ~req s "503" "Connection Limit Exceeded" body

let real_proxy __context vm req _ vnc_port s =
  let vm_id = Ref.string_of vm in
  let pool = Helpers.get_pool ~__context in
  let is_limit_enabled =
    Db.Pool.get_limit_console_sessions ~__context ~self:pool
  in
  let session_id = Xapi_http.get_session_id req in
  let user = Db.Session.get_auth_user_name ~__context ~self:session_id in
  match Connection_limit.try_add vm_id user is_limit_enabled with
  | Ok connection_id ->
      finally (* Ensure we drop the vm connection count if exceptions occur *)
        (fun () -> real_proxy' ~__context ~vm vnc_port s)
        (fun () -> Connection_limit.drop vm_id connection_id)
  | Error connected_users ->
      respond_console_limit_exceeded req s vm_id connected_users

let go_if_no_limit __context s f =
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_limit_console_sessions ~__context ~self:pool then
    Http_svr.headers s (Http.http_503_service_unavailable ())
  else
    f ()

let ws_proxy __context _ req protocol address s =
  go_if_no_limit __context s @@ fun () ->
  let addr = match address with Port p -> string_of_int p | Path p -> p in
  let protocol =
    match protocol with `rfb -> "rfb" | `vt100 -> "vt100" | `rdp -> "rdp"
  in
  let real_path = Filename.concat "/var/lib/xcp" "websockproxy" in
  let sock =
    try Some (Fecomms.open_unix_domain_sock_client real_path)
    with e ->
      debug "Error connecting to wsproxy (%s)" (Printexc.to_string e) ;
      Http_svr.headers s (Http.http_501_method_not_implemented ()) ;
      None
  in
  (* Ensure we always close the socket *)
  finally
    (fun () ->
      let upgrade_successful =
        Option.map
          (fun sock ->
            try
              let result = (sock, Some (Ws_helpers.upgrade req s)) in
              result
            with _ -> (sock, None)
          )
          sock
      in
      Option.iter
        (function
          | sock, Some ty ->
              let wsprotocol =
                match ty with
                | Ws_helpers.Hixie76 ->
                    "hixie76"
                | Ws_helpers.Hybi10 ->
                    "hybi10"
              in
              let message =
                Printf.sprintf "%s:%s:%s" wsprotocol protocol addr
              in
              let len = String.length message in
              ignore (Unixext.send_fd_substring sock message 0 len [] s)
          | _, None ->
              Http_svr.headers s (Http.http_501_method_not_implemented ())
          )
        upgrade_successful
    )
    (fun () -> Option.iter (fun sock -> Unix.close sock) sock)

let default_console_of_vm ~__context ~self =
  try
    let consoles = Db.VM.get_consoles ~__context ~self in
    let protocols =
      List.map (fun self -> Db.Console.get_protocol ~__context ~self) consoles
    in
    fst (List.find (fun (_, p) -> p = `rfb) (List.combine consoles protocols))
  with _ ->
    error "Failed to find default VNC console for VM" ;
    raise Failure

let console_of_request __context req =
  (* First check the request looks valid *)
  if
    (not (List.mem_assoc "ref" req.Http.Request.query))
    && not (List.mem_assoc "uuid" req.Http.Request.query)
  then (
    error "HTTP request for console forwarding lacked 'ref' or 'uuid' parameter" ;
    raise Failure
  ) ;
  let _ref =
    if List.mem_assoc "uuid" req.Http.Request.query then
      let uuid = List.assoc "uuid" req.Http.Request.query in
      try Ref.string_of (Db.VM.get_by_uuid ~__context ~uuid)
      with _ -> Ref.string_of (Db.Console.get_by_uuid ~__context ~uuid)
    else
      List.assoc "ref" req.Http.Request.query
  in
  (* The _ref may be either a VM ref in which case we look for a
     default VNC console or it may be a console ref in which case we
     go for that. *)
  let db = Context.database_of __context in
  let is_vm, _ =
    let module DB =
      (val Xapi_database.Db_cache.get db : Xapi_database.Db_interface.DB_ACCESS2)
    in
    match DB.get_table_from_ref db _ref with
    | Some c when c = Db_names.vm ->
        (true, false)
    | Some c when c = Db_names.console ->
        (false, true)
    | _ ->
        error "%s is neither a VM ref or a console ref" _ref ;
        raise Failure
  in
  if is_vm then
    default_console_of_vm ~__context ~self:(Ref.of_string _ref)
  else
    Ref.of_string _ref

let rbac_check_for_control_domain __context (req : Request.t) console_id
    permission =
  let is_control_domain =
    let vm_id = Db.Console.get_VM ~__context ~self:console_id in
    Db.VM.get_is_control_domain ~__context ~self:vm_id
  in
  if is_control_domain then
    let extra_dmsg =
      Printf.sprintf "for host console %s" (Ref.string_of console_id)
    in
    let session_id = Xapi_http.get_session_id req in
    Rbac.check_with_new_task ~extra_dmsg session_id permission ~fn:Rbac.nofn
      ~args:(Xapi_http.rbac_audit_params_of req)

let check_vm_is_running_here __context console =
  let vm = Db.Console.get_VM ~__context ~self:console in
  if Db.VM.get_power_state ~__context ~self:vm <> `Running then (
    error "VM %s (Console %s) has power_state <> Running" (Ref.string_of vm)
      (Ref.string_of console) ;
    raise Failure
  ) ;
  let localhost = Helpers.get_localhost ~__context in
  let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
  if resident_on <> localhost then (
    error "VM %s (Console %s) has resident_on = %s <> localhost"
      (Ref.string_of vm) (Ref.string_of console)
      (Ref.string_of resident_on) ;
    raise Failure
  )

(* GET /console_uri?ref=.....
   Cookie: <session id> *)
let handler proxy_fn (req : Request.t) s _ =
  req.Request.close <- true ;
  Xapi_http.with_context "Connection to VM console" req s (fun __context ->
      let console = console_of_request __context req in
      (* only sessions with 'http/connect_console/host_console' permission *)
      let protocol = Db.Console.get_protocol ~__context ~self:console in
      (* can access dom0 host consoles *)
      rbac_check_for_control_domain __context req console
        Rbac_static.permission_http_connect_console_host_console
          .Db_actions.role_name_label ;
      (* Check VM is actually running locally *)
      check_vm_is_running_here __context console ;
      match address_of_console __context console with
      | Some vnc_port ->
          let vm = Db.Console.get_VM ~__context ~self:console in
          proxy_fn __context vm req protocol vnc_port s
      | None ->
          Http_svr.headers s (Http.http_404_missing ())
  )
