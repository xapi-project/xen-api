(*
 * Copyright (C) Citrix Systems Inc.
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

open Xapi_stdext_monadic
open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_threads.Threadext

module D = Debug.Make(struct let name = "xmlrpc_client" end)
open D

module E = Debug.Make(struct let name = "mscgen" end)

module Internal = struct
  let set_stunnelpid_callback : (string option -> int -> unit) option ref = ref None
  let unset_stunnelpid_callback : (string option -> int -> unit) option ref = ref None
end

let user_agent = "xen-api-libs/1.0"

let connect ?session_id ?task_id ?subtask_of path =
  let arg str x = Opt.default [] (Opt.map (fun x -> [ str, x ]) x) in
  let cookie = arg "session_id" session_id @ (arg "task_id" task_id) @ (arg "subtask_of" subtask_of) in
  Http.Request.make ~user_agent ~version:"1.0" ~keep_alive:true ~cookie ?subtask_of
    Http.Connect path

let xmlrpc ?frame ?version ?keep_alive ?task_id ?cookie ?length ?auth ?subtask_of ?query ?body path =
  let headers = Opt.map (fun x -> [ Http.Hdr.task_id, x ]) task_id in
  Http.Request.make ~user_agent ?frame ?version ?keep_alive ?cookie ?headers ?length ?auth ?subtask_of ?query ?body
    Http.Post path

(** Thrown when ECONNRESET is caught which suggests the remote crashed or restarted *)
exception Connection_reset

module StunnelDebug=Debug.Make(struct let name="stunnel" end)
let write_to_log x = StunnelDebug.debug "%s" (Astring.String.trim x)

(** Return true if this fd is connected to an HTTP server by sending an XMLRPC request
    for an unknown method and checking we get a matching MESSAGE_METHOD_UNKNOWN.
    This is used to prevent us accidentally trying to reuse a connection which has been 
    closed or left in some other inconsistent state. *)
let check_reusable (x: Unix.file_descr) =
  let msg_name = "system.isAlive" in
  let msg_uuid = Uuidm.to_string (Uuidm.create `V4) in
  (* This is for backward compatability *)
  let msg_func = Printf.sprintf "%s:%s" msg_name msg_uuid in
  let msg_param = [ XMLRPC.To.string msg_uuid ] in
  let xml = Xml.to_string (XMLRPC.To.methodCall msg_func msg_param) in
  let http = xmlrpc ~version:"1.1" ~keep_alive:true ~body:xml "/" in
  try
    Http_client.rpc x http
      (fun response _ ->
         match response.Http.Response.content_length with
         | Some len ->
           let len = Int64.to_int len in
           let tmp = String.make len 'X' in
           let buf = Buf_io.of_fd x in
           Buf_io.really_input buf tmp 0 len;
           begin match XMLRPC.From.methodResponse (Xml.parse_string tmp) with
             | XMLRPC.Failure("MESSAGE_METHOD_UNKNOWN", [ param ])
               when param = msg_func ->
               (* This must be the server pre-dates system.isAlive *)
               true
             | XMLRPC.Success param when param = msg_param ->
               (* This must be the new server withs system.isAlive *)
               true
             | _ ->
               StunnelDebug.debug "check_reusable: unexpected response: connection not reusable: %s" tmp;
               false
           end
         | None ->
           StunnelDebug.debug "check_reusable: no content-length from known-invalid URI: connection not reusable";
           false
      )
  with exn ->
    StunnelDebug.debug "check_reusable: caught exception %s; assuming not reusable" (Printexc.to_string exn);
    false

(** Thrown when repeated attempts to connect an stunnel to a remote host and check
    the connection works fail. *)
exception Stunnel_connection_failed

let get_new_stunnel_id =
  let counter = ref 0 in
  let m = Mutex.create () in
  fun () -> Mutex.execute m (fun () -> incr counter; !counter)

(** Returns an stunnel, either from the persistent cache or a fresh one which
    has been checked out and guaranteed to work. *)
let get_reusable_stunnel ?use_fork_exec_helper ?write_to_log host port ?verify_cert =
  let found = ref None in
  (* 1. First check if there is a suitable stunnel in the cache. *)
  let verify_cert = Stunnel.must_verify_cert verify_cert in
  begin
    try
      while !found = None do
        let (x: Stunnel.t) = Stunnel_cache.remove host port verify_cert in
        if check_reusable x.Stunnel.fd
        then found := Some x
        else begin
          StunnelDebug.debug "get_reusable_stunnel: Found non-reusable stunnel in the cache. disconnecting from %s:%d" host port;
          Stunnel.disconnect x
        end
      done
    with Not_found -> ()
  end;
  match !found with
  | Some x ->
    x
  | None ->
    StunnelDebug.debug "get_reusable_stunnel: stunnel cache is empty; creating a fresh connection to %s:%d" host port;
    (* 2. Create a fresh connection and make sure it works *)
    begin
      let max_attempts = 10 in
      let attempt_number = ref 0 in
      let delay = 10. in (* seconds *)
      while !found = None && (!attempt_number < max_attempts) do
        incr attempt_number;
        try
          let unique_id = get_new_stunnel_id () in
          let (x: Stunnel.t) = Stunnel.connect ~unique_id ?use_fork_exec_helper ?write_to_log ~verify_cert host port in
          if check_reusable x.Stunnel.fd
          then found := Some x
          else begin
            StunnelDebug.error "get_reusable_stunnel: fresh stunnel failed reusable check; delaying %.2f seconds before reconnecting to %s:%d (attempt %d / %d)" delay host port !attempt_number max_attempts;
            Thread.delay delay;
            Stunnel.disconnect x
          end
        with e ->
          StunnelDebug.error "get_reusable_stunnel: fresh stunnel connection failed with exception: %s: delaying %.2f seconds before reconnecting to %s:%d (attempt %d / %d)" (Printexc.to_string e) delay host port !attempt_number max_attempts;
          Thread.delay delay;
      done
    end;
    begin match !found with
      | Some x ->
        x
      | None ->
        StunnelDebug.error "get_reusable_stunnel: failed to acquire a working stunnel to connect to %s:%d" host port;
        raise Stunnel_connection_failed
    end

module SSL = struct
  type t = {
    use_fork_exec_helper: bool;
    use_stunnel_cache: bool;
    verify_cert: bool option;
    task_id: string option
  }
  let make ?(use_fork_exec_helper=true) ?(use_stunnel_cache=false) ?(verify_cert) ?task_id () = {
    use_fork_exec_helper = use_fork_exec_helper;
    use_stunnel_cache = use_stunnel_cache;
    verify_cert = verify_cert;
    task_id = task_id
  }
  let to_string (x: t) =
    Printf.sprintf "{ use_fork_exec_helper = %b; use_stunnel_cache = %b; verify_cert = %s; task_id = %s }"
      x.use_fork_exec_helper x.use_stunnel_cache (Opt.default "None" (Opt.map (fun x -> string_of_bool x) x.verify_cert))
      (Opt.default "None" (Opt.map (fun x -> "Some " ^ x) x.task_id))
end

type transport =
  | Unix of string
  | TCP of string * int
  | SSL of SSL.t * string * int

let string_of_transport = function
  | Unix x -> Printf.sprintf "Unix %s" x
  | TCP (host, port) -> Printf.sprintf "TCP %s:%d" host port
  | SSL (ssl, host, port) -> Printf.sprintf "SSL %s:%d %s" host port (SSL.to_string ssl)

let transport_of_url (scheme, _) =
  let open Http.Url in
  match scheme with
  | File { path = path } -> Unix path
  | Http ({ ssl = false } as h) ->
    let port = Opt.default 80 h.port in
    TCP(h.host, port)
  | Http ({ ssl = true } as h) ->
    let port = Opt.default 443 h.port in
    SSL(SSL.make (), h.host, port)

let with_transport transport f = 
  let open Xapi_stdext_unix in
  match transport with
  | Unix path ->
    let fd = Unixext.open_connection_unix_fd path in
    finally
      (fun () -> f fd)
      (fun () -> Unix.close fd)
  | TCP (host, port) ->
    let fd = Unixext.open_connection_fd host port in
    finally
      (fun () ->
         Unixext.set_tcp_nodelay fd true;
         f fd)
      (fun () -> Unix.close fd)
  | SSL ({
      SSL.use_fork_exec_helper = use_fork_exec_helper;
      use_stunnel_cache = use_stunnel_cache;
      verify_cert = verify_cert;
      task_id = task_id}, host, port) ->
    let st_proc =
      if use_stunnel_cache
      then get_reusable_stunnel ~use_fork_exec_helper ~write_to_log host port ?verify_cert
      else
        let unique_id = get_new_stunnel_id () in
        Stunnel.connect ~use_fork_exec_helper ~write_to_log ~unique_id ?verify_cert ~extended_diagnosis:true host port in
    let s = st_proc.Stunnel.fd in
    let s_pid = Stunnel.getpid st_proc.Stunnel.pid in
    debug "stunnel pid: %d (cached = %b) connected to %s:%d" s_pid use_stunnel_cache host port;

    (* Call the {,un}set_stunnelpid_callback hooks around the remote call *)
    let with_recorded_stunnelpid task_opt s_pid f =
      debug "with_recorded_stunnelpid task_opt=%s s_pid=%d" (Opt.default "None" task_opt) s_pid;
      begin
        match !Internal.set_stunnelpid_callback with
        | Some f -> f task_id s_pid
        | _ -> ()
      end;
      finally f
        (fun () ->
           match !Internal.unset_stunnelpid_callback with
           | Some f -> f task_id s_pid
           | _ -> ()
        ) in

    with_recorded_stunnelpid task_id s_pid
      (fun () ->
         finally
           (fun () ->
              try
                f s
              with e ->
                warn "stunnel pid: %d caught %s" s_pid (Printexc.to_string e);
                if e = Connection_reset && not use_stunnel_cache
                then Stunnel.diagnose_failure st_proc;
                raise e)
           (fun () ->
              if use_stunnel_cache
              then begin
                Stunnel_cache.add st_proc;
                debug "stunnel pid: %d (cached = %b) returned stunnel to cache" s_pid use_stunnel_cache;
              end else
                begin
                  Unix.unlink st_proc.Stunnel.logfile;
                  Stunnel.disconnect st_proc
                end
           )
      )

let with_http request f s =
  try
    Http_client.rpc s request (fun response s -> f (response, s))
  with Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset

let curry2 f (a, b) = f a b

module type FORMAT = sig
  type response
  val response_of_string: string -> response
  val response_of_file_descr: Unix.file_descr -> response
  type request
  val request_to_string: request -> string
  val request_to_short_string: request -> string
end

module XML = struct
  type response = Xml.xml
  let response_of_string = Xml.parse_string
  let response_of_file_descr fd = Xml.parse_in (Unix.in_channel_of_descr fd)
  type request = Xml.xml
  let request_to_string = Xml.to_string
  let request_to_short_string = fun _ -> "(XML)"
end

module XMLRPC = struct
  type response = Rpc.response
  let response_of_string x = Xmlrpc.response_of_string x
  let response_of_file_descr fd = Xmlrpc.response_of_in_channel (Unix.in_channel_of_descr fd)
  type request = Rpc.call
  let request_to_string x = Xmlrpc.string_of_call x
  let request_to_short_string x = x.Rpc.name
end

module Protocol = functor(F: FORMAT) -> struct
  (** Take an optional content_length and task_id together with a socket
      		and return the XMLRPC response as an XML document *)
  let read_response r s =
    try
      match r.Http.Response.content_length with
      | Some l when (Int64.to_int l) <= Sys.max_string_length ->
        F.response_of_string (Xapi_stdext_unix.Unixext.really_read_string s (Int64.to_int l))
      | Some _ | None -> F.response_of_file_descr s
    with
    | Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset

  let rpc ?(srcstr="unset") ?(dststr="unset") ~transport ~http req =
    (* Caution: req can contain sensitive information such as passwords in its parameters,
       		 * so we should not log the parameters or a string representation of the whole thing.
       		 * The name should be safe though, e.g. req.Rpc.name when F is XMLRPC. *)
    E.debug "%s=>%s [label=\"%s\"];" srcstr dststr (F.request_to_short_string req) ;
    let body = F.request_to_string req in
    let http = { http with Http.Request.body = Some body } in
    with_transport transport (with_http http (curry2 read_response))
end

module XML_protocol = Protocol(XML)
module XMLRPC_protocol = Protocol(XMLRPC)




