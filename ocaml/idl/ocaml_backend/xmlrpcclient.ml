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
open Stringext
open Pervasiveext
open Threadext

module D = Debug.Debugger(struct let name = "http" end)
open D

let set_stunnelpid_callback : (string option -> int -> unit) option ref = ref None
let unset_stunnelpid_callback : (string option -> int -> unit) option ref = ref None

(* Headers for an HTTP CONNECT operation *)
let connect_headers ?session_id ?task_id ?subtask_of host path = 
  let session_id = default [] (may (fun x -> [ "session_id", x ]) session_id)
  and task_id = default [] (may (fun x -> [ "task_id", x ]) task_id) 
  and subtask_of = default [] (may (fun x -> [ "subtask_of", x ]) subtask_of) in
  let all = session_id @ task_id @ subtask_of in
  let cookie = String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) all) in
  [
    Printf.sprintf "CONNECT %s HTTP/1.0" path;
    Printf.sprintf "Host: %s" host;
    Printf.sprintf "Cookie: %s" cookie;
    "Connection: keep-alive"; 
  ]

(** Example XMLRPC headers from http://www.xmlrpc.com/spec *)
let xmlrpc_headers ?task_id ?subtask_of ~version host path content_length = [
  Printf.sprintf "POST %s HTTP/%s" path version;
  (* User-Agent: Frontier/5.1.2 (WinNT) *)
  Printf.sprintf "User-Agent: xapi/%s" Xapi_globs.api_version_string;
  Printf.sprintf "Host: %s" host;
  "Content-Type: text/xml";
  Printf.sprintf "Content-length: %d" content_length;
] @ (default [] (may (fun task -> [ Printf.sprintf "%s: %s" Http.task_id_hdr task ]) task_id))
  @ (default [] (may (fun task -> [ Printf.sprintf "%s: %s" Http.subtask_of_hdr task ]) subtask_of))

(** Thrown when an explicit HTTP rejection is received *)
exception Http_request_rejected of string

let http_body_max = 1 lsl 20

(** Thrown when ECONNRESET is caught which suggests the remote crashed or restarted *)
exception Connection_reset

(* Internal exception thrown when reading a newline-terminated HTTP header when the 
   connection is closed *)
exception Http_header_truncated of string

(** Thrown when no data is received from the remote HTTP server. This could happen if
    (eg) an stunnel accepted the connection but xapi refused the forward causing stunnel
    to immediately close. *)
exception Empty_response_from_server

(** Thrown when we get a specific HTTP error, e.g. 
		401 (unauthorized) if we supply the wrong credentials
		403 (forbidden)    if RBAC denied access
		500 (internal server error) if XAPI failed with an INTERNAL_ERROR,
		      Api_server error, XMLRPC_UNMARSHAL_FAILURE error etc.
 *)
exception Http_error of string*string

let input_line_fd (fd: Unix.file_descr) = 
  let buf = Buffer.create 20 in
  let finished = ref false in
  try
    while not(!finished) do
      let buffer = " " in
      let read = Unix.read fd buffer 0 1 in
      if read < 1 then raise (Http_header_truncated (Buffer.contents buf));
      if buffer = "\n" then finished := true
      else Buffer.add_char buf buffer.[0]
    done;
    Buffer.contents buf
  with
  | Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset


let http_rpc_send_query fd headers body =
  try
    let output_string str =
      ignore (Unix.write fd str 0 (String.length str)) in
    let writeln x = 
      output_string x; output_string "\r\n" in
    
    List.iter writeln headers;
    writeln "";
    if body <> "" then
      output_string body
  with
  | Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset

let http_rpc_send_big_query fd headers body =
  try
    let output_string str =
      ignore (Unix.write fd str 0 (String.length str)) in
    let writeln x = 
      output_string x; output_string "\r\n" in
    
    List.iter writeln headers;
    writeln "";
    if Bigbuffer.length body > 0L then
      Bigbuffer.to_fct body (fun s -> output_string s)
  with
  | Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset

(** Read the HTTP response from the fd *)
let http_rpc_recv_response error_msg fd =
  let ok = ref false in
  let task_id = ref None in
  let content_length = ref (-1) in
  (try
     (* Initial line has the response code on it *)
     let line = 
       try input_line_fd fd 
       with 
       | Http_header_truncated "" ->
	   (* Special case the error when no data is received at all *)
	   warn "HTTP connection closed immediately with before any data received";
	   raise Empty_response_from_server	   
     in
     match String.split_f String.isspace line with
     | _ :: "200" :: _ ->
	 ok := true;
	 (* Skip the rest of the headers *)
	 while true do
	   let line = input_line_fd fd in

	   (* NB input_line removes the final '\n'.
              RFC1945 says to expect a '\r\n' (- '\n' = '\r') *)
	   match line with	 
	   | "" | "\r" -> raise Not_found
	   | x -> 
	       begin
		 let (k,t) = 
		   match String.split ':' x with
		   | k :: rst -> (k, String.concat ":" rst) 
		   | _ -> ("","") in
		 let k' = String.lowercase k in
		 if k' = String.lowercase Http.task_id_hdr then begin
		   (* debug(Printf.sprintf "k = [%s] v = [%s]" k t); *)
		     let t = String.strip String.isspace t in
		     debug "Got a task-id: [%s]" t;
		     task_id := Some t
		 end else if k' = "content-length" then begin
		     let t = String.strip String.isspace t in
		     content_length := int_of_string t
		 end 
	       end
	 done
     | _ :: (("401"|"403"|"500") as http_code) :: _ ->
       raise (Http_error (http_code,error_msg))
     | _ -> 
	 debug "Read unknown response response: %s" line;
	 raise Not_found
   with Not_found -> ());
  if not(!ok) 
  then raise (Http_request_rejected error_msg)
  else (!content_length, !task_id)

(** XXX: merge with previous function *)
let read_http_headers ?timeout fd = 
  let headers = ref [] in
  let finished = ref false in
  while not(!finished) do
    let line = Buf_io.input_line ?timeout fd in
    match line with
    | "" | "\r" -> finished := true
    | x ->
	begin
	  let (k,t) = 
	    match String.split ':' x with
	    | k :: rst -> (k, String.concat ":" rst) 
	    | _ -> ("","") in
	  let k' = String.lowercase k in
	  let t' = String.strip String.isspace t in
	  headers := (k', t') :: !headers
	end	
  done;
  List.rev !headers

(** XXX: merge with previous function. *)
let http_rpc_recv_response_timeout error_msg ?(timeout=Some Buf_io.infinite_timeout) buf = 
  let line = Buf_io.input_line ?timeout buf in
  match String.split_f String.isspace line with
  | _ :: "200" :: _ -> read_http_headers ?timeout buf 
  | _ :: (("401"|"403"|"500") as http_code) :: _ ->
      raise (Http_error (http_code,error_msg))
  | _ ->
      warn "http_rpc_recv_response_timeout: unknown response: %s" line;
      raise (Http_request_rejected error_msg)


(** Send an HTTP request on a connected fd with (headers) and optional (body).
    Parses the result code and either throws an error exception or returns
    (content_length, task_id option). The file descriptor stays connected
    throughout. *)
let http_rpc_fd (fd: Unix.file_descr) headers body = 
  http_rpc_send_query fd headers body;
  http_rpc_recv_response (List.hd headers) fd

let http_rpc_fd_big_query fd headers body =
  http_rpc_send_big_query fd headers body;
  http_rpc_recv_response (List.hd headers) fd

module StunnelDebug=Debug.Debugger(struct let name="stunnel" end)
let write_to_log x = StunnelDebug.debug "%s" (String.strip String.isspace x) 

(** Return true if this fd is connected to an HTTP server by sending an XMLRPC request
    for an unknown method and checking we get a matching MESSAGE_METHOD_UNKNOWN.
    This is used to prevent us accidentally trying to reuse a connection which has been 
    closed or left in some other inconsistent state. *)
let check_reusable (x: Unix.file_descr) = 
  let unknown_msg = Printf.sprintf "unknown-message-%s" (Uuid.string_of_uuid (Uuid.make_uuid ())) in
  let xml = Xml.to_string (XMLRPC.To.methodCall unknown_msg []) in
  let lines = [ "POST / HTTP/1.1";
		"Connection: keep-alive";
		Printf.sprintf "Content-length: %d" (String.length xml);
	      ] in
  try
    http_rpc_send_query x lines xml;
    let buf = Buf_io.of_fd x in
    let headers = http_rpc_recv_response_timeout "XMLRPC probe" buf in
    if List.mem_assoc "content-length" headers then begin
      let len = int_of_string (List.assoc "content-length" headers) in
      let tmp = String.make len 'X' in
      Buf_io.really_input buf tmp 0 len;
      match XMLRPC.From.methodResponse (Xml.parse_string tmp) with
      | XMLRPC.Failure(code, [ param ]) 
	  when code = Api_errors.message_method_unknown && param = unknown_msg ->
	  true
      | _ ->
	  StunnelDebug.debug "check_reusable: unexpected response: connection not reusable: %s" tmp;
	  false
    end else begin
      StunnelDebug.debug "check_reusable: no content-length from known-invalid URI: connection not reusable";
      false
    end
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
let get_reusable_stunnel ?use_external_fd_wrapper ?write_to_log host port = 
  let found = ref None in
  (* 1. First check if there is a suitable stunnel in the cache. *)
  begin
    try
      while !found = None do
	let (x: Stunnel.t) = Stunnel_cache.remove host port in
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
  | Some x -> x
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
	    let (x: Stunnel.t) = Stunnel.connect ~unique_id ?use_external_fd_wrapper ?write_to_log host port in
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
      | Some x -> x
      | None ->
	  StunnelDebug.error "get_reusable_stunnel: failed to acquire a working stunnel to connect to %s:%d" host port;
	  raise Stunnel_connection_failed
      end
  

(** Sends an HTTP request to (host):(port) with (headers) and optional (body)
    Parses the result code and, if successful, calls 'f' with the
    socket still connected. Returns the result of
    f after closing the socket. *)
let do_http_rpc host port ?(unixsock=None) headers body f =
  let s = 
    match unixsock with
	None -> let s = Unixext.open_connection_fd host port in Unixext.set_tcp_nodelay s true; s
      | Some path -> Unixext.open_connection_unix_fd path in
  try
    let content_length, task_id = http_rpc_fd s headers body in
    let result = f content_length task_id s in
    Unix.close s;
    result
  with e ->
    Unix.close s;
    raise e

let do_secure_http_rpc ?(use_external_fd_wrapper=true) ?(use_stunnel_cache=false) ?(verify_cert=false) ?task_id ~host ~port ?(unixsock=None) ~headers ~body f =
  assert (not (verify_cert && use_stunnel_cache)); 
  let st_proc = 
    if use_stunnel_cache 
    then get_reusable_stunnel ~use_external_fd_wrapper ~write_to_log host port
    else 
      let unique_id = get_new_stunnel_id () in
      Stunnel.connect ~use_external_fd_wrapper ~write_to_log ~unique_id ~verify_cert ~extended_diagnosis:true host port in
  let s = st_proc.Stunnel.fd in
  let s_pid = Stunnel.getpid st_proc.Stunnel.pid in
  info "stunnel pid: %d (cached = %b) connected to %s:%d" s_pid use_stunnel_cache host port;

  (* Call the {,un}set_stunnelpid_callback hooks around the remote call *)
  let with_recorded_stunnelpid task_opt s_pid f =
	  info "with_recorded_stunnelpid task_opt=%s s_pid=%d" (Opt.default "None" task_opt) s_pid;
	begin
	  match !set_stunnelpid_callback with
	  | Some f -> f task_id s_pid
	  | _ -> ()
	end;
	finally f
		(fun () ->
			 match !unset_stunnelpid_callback with
			 | Some f -> f task_id s_pid
			 | _ -> ()
		) in

  with_recorded_stunnelpid task_id s_pid
	(fun () ->
  finally
    (fun () ->
      try
        let content_length, task_id = http_rpc_fd s headers body in
        f content_length task_id s
      with e ->
		  warn "stunnel pid: %d caught %s" s_pid (Printexc.to_string e);
		  if e = Connection_reset && not use_stunnel_cache
		  then Stunnel.diagnose_failure st_proc;
          raise e)
    (fun () ->
      if use_stunnel_cache
      then begin
          Stunnel_cache.add st_proc;
		  info "stunnel pid: %d (cached = %b) returned stunnel to cache" s_pid use_stunnel_cache;
      end else
        begin
          Unix.unlink st_proc.Stunnel.logfile;
          Stunnel.disconnect st_proc
        end
    )
     ) 

(** Take an optional content_length and task_id together with a socket
    and return the XMLRPC response as an XML document *)
let read_xml_rpc_response content_length task_id s = 
  try
    if content_length > -1 
    then
      let buffer = String.make content_length '\000' in
      let rec really_read off n =
	if n=0 then () else 
	let m = Unix.read s buffer off n in
	really_read (off+m) (n-m)
      in
      really_read 0 content_length;
      Xml.parse_string buffer
    else
      Xml.parse_in (Unix.in_channel_of_descr s)
  with
  | Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset

let generic_rpc ?task_id ?subtask_of req version host path =
  let req_string = Xml.to_string_fmt req in
  let headers = xmlrpc_headers ?task_id ?subtask_of ~version host path (String.length req_string) in
    req_string, headers

(** Perform a complete XMLRPC over a fresh connection. *)
let do_xml_rpc ?task_id ?subtask_of ~version ~host ~port ~path (req: Xml.xml) : Xml.xml =
  let req_string, headers = generic_rpc ?task_id ?subtask_of req version host path in
  do_http_rpc host port headers req_string read_xml_rpc_response

let do_secure_xml_rpc ?task_id ?subtask_of ?use_external_fd_wrapper ?use_stunnel_cache ~version ~host ~port ~path (req: Xml.xml) : Xml.xml =
  let req_string, headers = generic_rpc ?task_id ?subtask_of req version host path in
  do_secure_http_rpc ?use_external_fd_wrapper ?use_stunnel_cache ?task_id ~host ~port ~headers ~body:req_string read_xml_rpc_response

let do_xml_rpc_unix ?task_id ?subtask_of ~version ~filename ~path (req: Xml.xml) : Xml.xml =
  let req_string, headers = generic_rpc ?task_id ?subtask_of req version "localhost" path in  
  do_http_rpc "" 0 headers ~unixsock:(Some filename) req_string read_xml_rpc_response

exception Content_length_required

(** Perform an HTTP/1.1 XMLRPC request over an existing file_descriptor.
    Returns the XML response and leaves the connection is a state where
    the next request can be accessed. *)
let do_xml_rpc_persistent ~host ~path (s: Unix.file_descr) (req: Xml.xml) : Xml.xml = 
  let req_string = Xml.to_string_fmt req in
  (* Persistent connections requires HTTP 1.1 *)
  let headers = xmlrpc_headers ~version:"1.1" host path (String.length req_string + 0) in
  let content_length, task_id = http_rpc_fd s headers req_string in
  (* XML responses must have a content-length because we cannot use the Xml.parse_in
     in_channel function: the input channel will buffer an arbitrary amount of stuff
     and we'll be out of sync with the next request. *)
  if content_length < 0 then raise Content_length_required;
  read_xml_rpc_response content_length task_id s
