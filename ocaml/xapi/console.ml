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
module Timer = Clock.Timer

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

  let active_connections : int VMMap.t ref = ref VMMap.empty

  let mutex = Mutex.create ()

  let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

  let drop vm_id =
    with_lock mutex (fun () ->
        match VMMap.find_opt vm_id !active_connections with
        | Some n when n > 1 ->
            active_connections := VMMap.add vm_id (n - 1) !active_connections
        | Some _ | None ->
            active_connections := VMMap.remove vm_id !active_connections
    )

  (* When the limit is disabled (false), we must still track the connection count for each vm_id.
     This ensures that if the limit is later enabled (set to true), any existing connections are accounted for,
     and the limit can be correctly enforced for subsequent connection attempts. *)
  let try_add vm_id is_limit_enabled =
    with_lock mutex (fun () ->
        let count =
          VMMap.find_opt vm_id !active_connections |> Option.value ~default:0
        in
        if is_limit_enabled && count > 0 then (
          debug
            "limit_console_sessions is true. Console connection is rejected \
             for VM %s, active connections: %d"
            vm_id count ;
          false
        ) else (
          active_connections := VMMap.add vm_id (count + 1) !active_connections ;
          true
        )
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

let log_in_file ?(file = "/var/log/idle_timeout.txt") msg =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o644 file in
  let time_str =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.tm_year + 1900)
      (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
  in
  output_string oc (Printf.sprintf "[%s] %s\n" time_str msg) ;
  close_out oc

(* https://github.com/rfbproto/rfbproto/blob/master/rfbproto.rst *)
module RFB_msg_type_parser = struct
  type state = {
      mutable rfb_phase: [`Handshake | `Security | `ClientInit | `Messages]
    ; mutable incomplete_msg_remaining: int
          (* bytes needed to complete current message *)
    ; mutable received_msg_types: int list
          (* List of message types parsed from input data *)
  }

  let create () =
    {rfb_phase= `Handshake; incomplete_msg_remaining= 0; received_msg_types= []}

  (* https://github.com/rfbproto/rfbproto/blob/master/rfbproto.rst#client-to-server-messages *)
  let message_length msg_type =
    match msg_type with
    | 0 ->
        20 (* SetPixelFormat *)
    | 2 ->
        -1 (* SetEncodings - variable, need to read count *)
    | 3 ->
        10 (* FramebufferUpdateRequest *)
    | 4 ->
        8 (* KeyEvent *)
    | 5 ->
        6 (* PointerEvent *)
    | 6 ->
        -1 (* ClientCutText - variable, need to read length *)
    | _ ->
        -2 (* Unknown - cannot determine length, stop parsing *)

  (* Handle variable length messages. For now, only SetEncodings(2) and ClientCutText(6) *)
  let handle_variable_message data offset msg_type available =
    match msg_type with
    | 2 when available >= 4 ->
        (* SetEncodings *)
        let count =
          ((Bytes.get data (offset + 2) |> int_of_char) lsl 8)
          lor (Bytes.get data (offset + 3) |> int_of_char)
        in
        let total_len = 4 + (count * 4) in
        log_in_file
          (Printf.sprintf "[rfb] SetEncodings total length: %d" total_len) ;
        if available >= total_len then
          `Complete total_len
        else
          `Incomplete (total_len - available)
    | 6 when available >= 8 ->
        (* ClientCutText *)
        let text_len =
          ((Bytes.get data (offset + 4) |> int_of_char) lsl 24)
          lor ((Bytes.get data (offset + 5) |> int_of_char) lsl 16)
          lor ((Bytes.get data (offset + 6) |> int_of_char) lsl 8)
          lor (Bytes.get data (offset + 7) |> int_of_char)
        in
        let total_len = 8 + text_len in
        log_in_file
          (Printf.sprintf "[rfb] ClientCutText total length: %d" total_len) ;
        if available >= total_len then
          `Complete total_len
        else
          `Incomplete (total_len - available)
    (* Incomplete header cases - need more data to determine message length *)
    | 2 ->
        `Incomplete (4 - available) (* Need more data for SetEncodings header *)
    | 6 ->
        `Incomplete (8 - available)
        (* Need more data for ClientCutText header *)
    | _ ->
        `Skip_All

  (* Process a single message in Messages phase *)
  let process_single_message state data offset available =
    let msg_type = Bytes.get data offset |> int_of_char in
    state.received_msg_types <- state.received_msg_types @ [msg_type] ;
    let msg_len = message_length msg_type in
    log_in_file
      (Printf.sprintf "[rfb] Message length for type %d: %d" msg_type msg_len) ;

    match msg_len with
    | -1 ->
        handle_variable_message data offset msg_type available
    | len when len > 0 ->
        if available >= len then
          `Complete len
        else
          `Incomplete (len - available)
    | _ ->
        log_in_file
          (Printf.sprintf
             "[rfb] Unknown RFB message type: %d (0x%02X), skipping remaining \
              data"
             msg_type msg_type
          ) ;
        `Skip_All

  (* Different RFB versions have variations in their handshake protocols,
     so we implement a simplified handshake parser that handles the most common cases.
     If handshake parsing encounters unexpected data, we gracefully transition to
     the Messages phase where unknown message types can be handled appropriately.
     This prevents the parser from getting stuck on protocol variations while
     ensuring we can still achieve our primary goal: detecting KeyEvent and
     PointerEvent messages for activity monitoring. *)
  let progress_handshake state data_len offset =
    match state.rfb_phase with
    | `Handshake when data_len - offset >= 12 ->
        log_in_file (Printf.sprintf "[rfb] Handshake 12 bytes") ;
        state.rfb_phase <- `ClientInit ;
        Some 12
    | `ClientInit when data_len - offset >= 1 ->
        log_in_file (Printf.sprintf "[rfb] ClientInit 1 byte") ;
        state.rfb_phase <- `Messages ;
        Some 1
    | _ ->
        state.rfb_phase <- `Messages ;
        None

  (* Handle incomplete message continuation *)
  let continue_incomplete_message state data_len offset =
    let available = data_len - offset in
    if available >= state.incomplete_msg_remaining then (
      let consumed = state.incomplete_msg_remaining in
      state.incomplete_msg_remaining <- 0 ;
      Some (offset + consumed)
    ) else (
      state.incomplete_msg_remaining <-
        state.incomplete_msg_remaining - available ;
      None
    )

  let rec process_data state data offset =
    let data_len = Bytes.length data in

    if offset >= data_len then
      ()
    else if state.incomplete_msg_remaining > 0 then
      (* Continue processing incomplete message *)
      match continue_incomplete_message state data_len offset with
      | Some new_offset ->
          process_data state data new_offset
      | None ->
          ()
    else (* Start new message or handle handshake *)
      match state.rfb_phase with
      | `Messages -> (
          let available = data_len - offset in
          match process_single_message state data offset available with
          | `Complete msg_len ->
              process_data state data (offset + msg_len)
          | `Incomplete remaining ->
              state.incomplete_msg_remaining <- remaining
          | `Skip_All ->
              (* Stop processing the current buffer and treat the next incoming data as fresh.
                 Since each network read likely starts with a message header, this allows
                 graceful recovery from parsing errors without getting permanently out of sync
                 with the RFB protocol stream. *)
              state.incomplete_msg_remaining <- 0
        )
      | _ -> (
        (* Handle handshake phases *)
        match progress_handshake state data_len offset with
        | Some consumed_bytes ->
            process_data state data (offset + consumed_bytes)
        | None ->
            ()
      )
end

module Console_idle_monitor = struct
  let read_console_config ~__context ~vm =
    try
      let idle_timeout =
        if Db.VM.get_is_control_domain ~__context ~self:vm then
          let host = Helpers.get_localhost ~__context in
          Db.Host.get_console_idle_timeout ~__context ~self:host
        else
          let pool = Helpers.get_pool ~__context in
          Db.Pool.get_vm_console_idle_timeout ~__context ~self:pool
      in
      let poll_event_timeout_ms =
        if idle_timeout > 0L then
          !Xapi_globs.proxy_poll_event_timeout_ms
        else
          -1 (* -1 means block indefinitely *)
      in
      (idle_timeout > 0L, poll_event_timeout_ms, idle_timeout)
    with _ -> (false, -1, 0L)
  (* Default: no timeout, block indefinitely *)

  (* Returns the initial vnc_console_state for a console *)
  let get_init_console_state ~__context ~vm =
    let is_idle_timeout_set, poll_event_timeout_ms, _ =
      read_console_config ~__context ~vm
    in
    let initial_state =
      {
        Unixext.is_idle_timeout_set
      ; Unixext.is_idle= false
      ; Unixext.poll_event_timeout_ms
      }
    in
    log_in_file
      (Printf.sprintf
         "[get_init_console_state] initial_state: is_idle_timeout_set=%b, \
          is_idle=%b, poll_event_timeout_ms=%d"
         initial_state.is_idle_timeout_set initial_state.is_idle
         initial_state.poll_event_timeout_ms
      ) ;
    initial_state

  (* Creates a VNC console idle timeout callback closure to determine is_idle *)
  let create_vnc_console_idle_timeout_callback ~__context ~vm
      initial_console_state =
    let simple_rfb_parser = RFB_msg_type_parser.create () in
    let current_console_state = ref initial_console_state in
    let idle_timer = ref None in

    (* Helper function to start/restart the idle timer *)
    let start_idle_timer reason =
      let _, _, idle_timeout_seconds = read_console_config ~__context ~vm in
      let timeout_duration =
        Timer.s_to_span (Int64.to_float idle_timeout_seconds) |> Option.get
      in
      idle_timer := Some (Timer.start ~duration:timeout_duration) ;
      log_in_file
        (Printf.sprintf "[idle_timeout] Starting/ restarting idle timer %s"
           reason
        )
    in

    (* Start the timer immediately if idle timeout is enabled *)
    if initial_console_state.Unixext.is_idle_timeout_set then
      start_idle_timer "on callback creation" ;
    fun received_data_opt ->
      (* First update any configuration that might have changed *)
      let is_idle_timeout_set, poll_event_timeout_ms, _ =
        read_console_config ~__context ~vm
      in
      current_console_state :=
        {
          !current_console_state with
          Unixext.is_idle_timeout_set
        ; Unixext.poll_event_timeout_ms
        } ;

      if !current_console_state.Unixext.is_idle_timeout_set then
        (* Though the callback is only invoked when the timeout is set,
           we still check here in case the setting changed from true to false.
           The case from false to true would never reach here *)
        match !idle_timer with
        (* Check if idle timer has expired first, regardless of received data *)
        | Some timer when Timer.has_expired timer ->
            log_in_file
              "[idle_timeout] Idle timeout reached - session should be \
               terminated" ;
            {!current_console_state with Unixext.is_idle= true}
        | _ -> (
          (* Timer not expired or not started yet, continue processing *)
          match received_data_opt with
          | None ->
              !current_console_state
          | Some data ->
              RFB_msg_type_parser.process_data simple_rfb_parser data 0 ;

              (* Check if we received KeyEvent (4) or PointerEvent (5) messages *)
              let has_activity =
                List.exists
                  (fun msg_type -> msg_type = 4 || msg_type = 5)
                  simple_rfb_parser.received_msg_types
              in

              if has_activity then (
                log_in_file
                  "[idle_timeout] User activity detected - resetting idle timer" ;
                (* Clear the received messages for next iteration *)
                simple_rfb_parser.received_msg_types <- [] ;
                (* Start/restart the idle timer *)
                start_idle_timer "after user activity" ;
                {!current_console_state with Unixext.is_idle= false}
              ) else (
                log_in_file
                  "[idle_timeout] No user activity detected in current data" ;
                !current_console_state
              )
        )
      else (
        (* Idle timeout not enabled, clear the existing timer *)
        idle_timer := None ;
        !current_console_state
      )
end

let real_proxy' ~__context ~vm vnc_port s console_state =
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

    let idle_callback =
      Console_idle_monitor.create_vnc_console_idle_timeout_callback ~__context
        ~vm console_state
    in
    Unixext.proxy ~console_init_state:console_state
      ~vnc_console_idle_timeout_callback:idle_callback vnc_sock s' ;
    debug "Proxy exited"
  with exn -> debug "error: %s" (ExnHelper.string_of_exn exn)

let real_proxy __context vm _ _ vnc_port s =
  let vm_id = Ref.string_of vm in
  let pool = Helpers.get_pool ~__context in
  let is_limit_enabled =
    Db.Pool.get_limit_console_sessions ~__context ~self:pool
  in
  if Connection_limit.try_add vm_id is_limit_enabled then
    let console_state =
      Console_idle_monitor.get_init_console_state ~__context ~vm
    in
    finally (* Ensure we drop the vm connection count if exceptions occur *)
      (fun () -> real_proxy' ~__context ~vm vnc_port s console_state)
      (fun () -> Connection_limit.drop vm_id)
  else
    Http_svr.headers s (Http.http_503_service_unavailable ())

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
