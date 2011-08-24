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
(* A very very simple HTTP server! *)

(*
 * Notes:
 *
 * HTTP CONNECT requests are not handled in the standard way! Normally, one
 * would issue a connect request like this:
 * 
 *    CONNECT host.domain:port HTTP/1.0
 * 
 * But we've got different proxies for different things, so we use the syntax
 *
 *    CONNECT /console?session_id=... HTTP/1.0
 * 
 * So we're not exactly standards compliant :)
 *
 *)

open Http
open Stringext
open Pervasiveext

module D = Debug.Debugger(struct let name="http" end)
module DCritical = Debug.Debugger(struct let name="http_critical" end)
open D


type uri_path = string 
let make_uri_path x = x

(** Type of a function which can handle a Request.t *)
type bufio_req_handler = Request.t -> Buf_io.t -> unit
type fdio_req_handler = Request.t -> Unix.file_descr -> unit
type req_handler =
	| BufIO of bufio_req_handler
	| FdIO of fdio_req_handler

(* One table per HTTP method *)
let connect_handler_table : (uri_path, req_handler) Hashtbl.t = Hashtbl.create 3
let get_handler_table : (uri_path, req_handler) Hashtbl.t = Hashtbl.create 3
let post_handler_table : (uri_path, req_handler) Hashtbl.t = Hashtbl.create 3
let put_handler_table : (uri_path, req_handler) Hashtbl.t = Hashtbl.create 3
    
let handler_table = function
    | Get -> get_handler_table 
    | Post -> post_handler_table
    | Put -> put_handler_table
    | Connect -> connect_handler_table
    | x -> failwith (Printf.sprintf "No handler table for HTTP method %s" (Http.string_of_method_t x))

let add_handler ty uri handler = 
  let table = handler_table ty in
  if Hashtbl.mem table uri 
  then failwith (Printf.sprintf "Handler for URI: %s already registered" uri)
  else Hashtbl.add table uri handler

(* try and do f (unit -> unit), ignore exceptions *)
let best_effort f =
  try f() with _ -> ()

let headers s headers = 
  output_http s headers;
  output_http s [""]

let response s hdrs length f =
	output_http s hdrs;
	output_http s [ Printf.sprintf "Content-Length: %Ld" length ];
	output_http s [ "" ];
	f s

(* If http/1.0 was requested, return that, else return http/1.1 *)
let get_return_version req =
  try
    let (maj,min) = Scanf.sscanf (Request.get_version req) "HTTP/%d.%d" (fun a b -> (a,b)) in
    match (maj,min) with
	(1,0) -> "1.0"
      | _ -> "1.1"
  with _ -> "1.1"
    
let response_fct req ?(hdrs=[]) s (response_length: int64) (write_response_to_fd_fn: Unix.file_descr -> unit) = 
	let version = get_return_version req in
	let keep_alive = if req.Request.close then false else true in
	response s ((http_200_ok ~version ~keep_alive ()) @ hdrs) response_length write_response_to_fd_fn

let response_str req ?hdrs s body =
	let length = String.length body in
	response_fct req ?hdrs s (Int64.of_int length) (fun s -> Unixext.really_write_string s body)

let response_missing ?(hdrs=[]) s body =
	response s (http_404_missing () @ hdrs) (Int64.of_int (String.length body)) (fun s -> Unixext.really_write_string s body)

let response_error_html s hdrs body =
	let hdrs = hdrs @ [ "Content-Type: text/html" ] in
	response s hdrs (Int64.of_int (String.length body)) (fun x -> Unixext.really_write_string s body)

let response_unauthorised ?req label s =
	let body = "<html><body><h1>HTTP 401 unauthorised</h1>Please check your credentials and retry.</body></html>" in
	response_error_html s (http_401_unauthorised ~realm:label ()) body

let response_forbidden ?req s =
	let version = Opt.map get_return_version req in
	let body = "<html><body><h1>HTTP 403 forbidden</h1>Access to the requested resource is forbidden.</body></html>" in	
	response_error_html s (http_403_forbidden ?version ()) body

let response_badrequest ?req s =
	let version = Opt.map get_return_version req in
	let body = "<html><body><h1>HTTP 400 bad request</h1>The HTTP request was malformed. Please correct and retry.</body></html>" in
	response_error_html s (http_400_badrequest ?version ()) body

let response_internal_error ?req ?extra s =
	let version = Opt.map get_return_version req in
	let extra = Opt.default "" (Opt.map (fun x -> "<h1> Additional information </h1>" ^ x) extra) in
	let body = "<html><body><h1>HTTP 500 internal server error</h1>An unexpected error occurred; please wait a while and try again. If the problem persists, please contact your support representative." ^ extra ^ "</body></html>" in
	response_error_html s (http_500_internal_error ?version ()) body

let response_file ?(hdrs=[]) ?mime_content_type s file =
	let size = (Unix.LargeFile.stat file).Unix.LargeFile.st_size in
	let mime_header = Opt.default [] (Opt.map (fun ty -> [ "Content-Type: " ^ ty ]) mime_content_type) in
	headers s (http_200_ok_with_content size ~version:"1.1" ~keep_alive:true () @ mime_header);
	Unixext.with_file file [ Unix.O_RDONLY ] 0
		(fun f ->
			let (_: int64) = Unixext.copy_file f s in
			()
		)

(** If no handler matches the request then call this callback *)
let default_callback req bio = 
  response_forbidden (Buf_io.fd_of bio);
  req.Request.close <- true
    
let escape uri =
       String.escaped ~rules:[ '<', "&lt;"; '>', "&gt;"; '\'', "&apos;"; '"', "&quot;"; '&', "&amp;" ] uri

exception Too_many_headers
exception Generic_error of string

(** [request_of_bio_exn ic] reads a single Http.req from [ic] and returns it. On error
	it simply throws an exception and doesn't touch the output stream. *)
let request_of_bio_exn ic =
    (* Try to keep the connection open for a while to prevent spurious End_of_file type 
	   problems under load *)
	let initial_timeout = 5. *. 60. in
  
	let content_length = ref (-1L) in
	let cookie = ref "" in
	let transfer_encoding = ref None in
	let auth = ref None in
	let task = ref None in
	let subtask_of = ref None in
	let content_type = ref None in
	let user_agent = ref None in

	content_length := -1L;
	cookie := "";

	let request_line = Buf_io.input_line ~timeout:initial_timeout ic in
	let req = Request.of_request_line request_line in

	(* Default for HTTP/1.1 is persistent connections. Anything else closes *)
	(* the channel as soon as the request is processed *)
	if req.Request.version <> "HTTP/1.1" then req.Request.close <- true;

	let rec read_rest_of_headers left =
		let cl_hdr = String.lowercase Http.Hdr.content_length in
		let cookie_hdr = String.lowercase Http.Hdr.cookie in
		let connection_hdr = String.lowercase Http.Hdr.connection in
		let transfer_encoding_hdr = String.lowercase Http.Hdr.transfer_encoding in
		let auth_hdr = String.lowercase Http.Hdr.authorization in
		let task_hdr = String.lowercase Http.Hdr.task_id in
		let subtask_of_hdr = String.lowercase Http.Hdr.subtask_of in
		let content_type_hdr = String.lowercase Http.Hdr.content_type in
		let user_agent_hdr = String.lowercase Http.Hdr.user_agent in
		let r = Buf_io.input_line ~timeout:Buf_io.infinite_timeout ic in
		match String.split ~limit:2 ':' r with
			| [ k; v ] ->
				let k = String.lowercase k in
				let v = String.strip String.isspace v in
				let absorbed = match k with
					| k when k = cl_hdr -> content_length := Int64.of_string v; true
					| k when k = cookie_hdr -> cookie := v; true
					| k when k = transfer_encoding_hdr -> transfer_encoding := Some v; true
					| k when k = auth_hdr -> auth := Some(authorization_of_string v); true
					| k when k = task_hdr -> task := Some v; true
					| k when k = subtask_of_hdr -> subtask_of := Some v; true
					| k when k = content_type_hdr -> content_type := Some v; true
					| k when k = user_agent_hdr -> user_agent := Some v; true
                    | k when k = connection_hdr ->
                        req.Request.close <- String.lowercase v = "close";
                        true
					| _ -> false in
				if not absorbed && left <= 0 then raise Too_many_headers;
				if absorbed
				then read_rest_of_headers (left - 1)
				else (k, v) :: (read_rest_of_headers (left - 1))
			| _ -> [] in
	let headers = read_rest_of_headers 242 in
	{ req with
		Request.cookie = (Http.parse_keyvalpairs !cookie);
		content_length = if !content_length = -1L then None else Some(!content_length);
		auth = !auth;
		task = !task;
		subtask_of = !subtask_of;
		content_type = !content_type;
		user_agent = !user_agent;
		additional_headers = headers;
	}

(** [request_of_bio ic] returns [Some req] read from [ic], or [None]. If [None] it will have
	already sent back a suitable error code and response to the client. *)
let request_of_bio ic =
	try
		Some (request_of_bio_exn ic)
	with e ->
		best_effort (fun () ->
			let ss = Buf_io.fd_of ic in
			match e with
				(* Specific errors thrown during parsing *)
			| Http.Http_parse_failure ->
				response_internal_error ss ~extra:"The HTTP headers could not be parsed.";
				debug "Error parsing HTTP headers";
			| Too_many_headers ->
				(* don't log anything, since it could fill the log *)
				response_internal_error ss ~extra:"Too many HTTP headers were received.";
			| Buf_io.Timeout ->
				DCritical.debug "Idle connection closed" (* NB infinite timeout used when headers are being read *)
			| Buf_io.Eof ->
				DCritical.debug "Connection terminated";
			| Buf_io.Line x ->
				begin
					match x with
							Buf_io.Too_long ->
								DCritical.debug "Line too long!"
						| Buf_io.No_newline ->
							DCritical.debug "Newline not found!"
				end;
				DCritical.debug "Buffer contains: '%s'" (Buf_io.get_data ic);
				response_internal_error ss ~extra:"One of the header lines was too long.";
				(* Generic errors thrown during parsing *)
			| End_of_file ->
				DCritical.debug "Premature termination of connection!";
			| Unix.Unix_error (a,b,c) ->
				response_internal_error ss ~extra:(Printf.sprintf "Got UNIX error: %s %s %s" (Unix.error_message a) b c);
				DCritical.debug "Unhandled unix exception: %s %s %s" (Unix.error_message a) b c;
			| exc ->
				response_internal_error ss ~extra:(escape (Printexc.to_string exc));
				DCritical.debug "Unhandled exception: %s" (Printexc.to_string exc);
				log_backtrace ();
		);
		None

let handle_connection _ ss =
	let ic = Buf_io.of_fd ss in
	let finished = ref false in
	let reqno = ref 0 in

	while not !finished do
		(* 1. we must successfully parse a request *)
		let req = request_of_bio ic in
		Opt.iter (fun _ -> reqno := !reqno + 1) req;
		if req = None then finished := true;

		(* 2. now we attempt to process the request *)
		Opt.iter
			(fun req ->
				try
					D.debug "Request %s" (Http.Request.to_string req);
					let table = handler_table req.Request.m in
					(* Find a specific handler: the last one whose URI is a prefix of the received
					   URI defaulting to 'default_callback' if none can be found *)
					let uris = Hashtbl.fold (fun k v a -> k::a) table [] in
					let uris = List.sort (fun a b -> compare (String.length b) (String.length a)) uris in
					let handlerfn =
						try
							let longest_match = List.find (fun uri -> String.startswith uri req.Request.uri) uris in
							Hashtbl.find table longest_match
						with
								_ -> BufIO (default_callback)
					in
					(match handlerfn with
						| BufIO handlerfn -> handlerfn req ic
						| FdIO handlerfn ->
							let fd = Buf_io.fd_of ic in
							Buf_io.assert_buffer_empty ic;
							handlerfn req fd
					);
					finished := (req.Request.close)
				with e ->
					finished := true;
					best_effort (fun () -> match e with
							(* Specific errors thrown by handlers *)
						| Generic_error s ->
							response_internal_error ~req ss ~extra:s
						| Http.Unauthorised realm ->
							response_unauthorised ~req realm ss
						| Http.Forbidden ->
							response_forbidden ~req ss
							(* Generic errors thrown by handlers *)
						| End_of_file ->
							DCritical.debug "Premature termination of connection!";
						| Unix.Unix_error (a,b,c) ->
							response_internal_error ~req ss ~extra:(Printf.sprintf "Got UNIX error: %s %s %s" (Unix.error_message a) b c);
							DCritical.debug "Unhandled unix exception: %s %s %s" (Unix.error_message a) b c;

						| exc ->
							response_internal_error ~req ss ~extra:(escape (Printexc.to_string exc));
							DCritical.debug "Unhandled exception: %s" (Printexc.to_string exc);
							log_backtrace ()

					)
			) req
	done;
	Unix.close ss

let bind ?(listen_backlog=128) sockaddr = 
  let domain = match sockaddr with
    | Unix.ADDR_UNIX path ->
		debug "Establishing Unix domain server on path: %s" path;
	Unix.PF_UNIX
    | Unix.ADDR_INET(_,_) ->
		debug "Establishing inet domain server";
	Unix.PF_INET in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  (* Make sure exceptions cause the socket to be closed *)
  try
    Unix.set_close_on_exec sock;
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    (match sockaddr with Unix.ADDR_INET _ -> Unixext.set_tcp_nodelay sock true | _ -> ());
    Unix.bind sock sockaddr;
    Unix.listen sock listen_backlog;
    sock 
  with e ->
    debug "Caught exception in Http_svr.bind (closing socket): %s" (Printexc.to_string e);
    Unix.close sock;
    raise e

let http_svr sockets = 
  (* Fork a thread for each socket, return the list of threads so they may be
     joined later (potentially). *)
  List.map
    (fun (sock,name) ->
       Thread.create 
	 (fun () ->
	    let handler = { Server_io.name = name;
			    body = handle_connection } in
	    Server_io.server handler sock) ()) sockets

(* Maps sockets to Server_io.server records *)
let server_table = Hashtbl.create 10

type server = Unix.file_descr

(* Start an HTTP server on a new socket *)
let start (socket, name) : server  = 
  let handler = { Server_io.name = name;
		  body = handle_connection } in
  let server = Server_io.server handler socket in
  Hashtbl.add server_table socket server;
  socket

exception Server_not_found
(* Stop an HTTP server running on a socket *)
let stop (key: server) = 
  let server = try Hashtbl.find server_table key with Not_found -> raise Server_not_found in
  Hashtbl.remove server_table key;
  server.Server_io.shutdown ()

exception Client_requested_size_over_limit

(** Read the body of an HTTP request (requires a content-length: header). *)
let read_body ?limit req bio =
	match req.Request.content_length with
	| None -> failwith "We require a content-length: HTTP header"
	| Some length ->
		let length = Int64.to_int length in
		maybe (fun l -> if length > l then raise Client_requested_size_over_limit) limit;
		Buf_io.really_input_buf ~timeout:Buf_io.infinite_timeout bio length

module Chunked = struct
    type t = { mutable current_size : int; mutable current_offset : int;
               mutable read_headers : bool; bufio : Buf_io.t }

    let of_bufio bufio =
        { current_size = 0; current_offset = 0; bufio = bufio; 
          read_headers = true }

    let rec read chunk size =
        if chunk.read_headers = true then begin
            (* first get the size, then get the data requested *)
            let size = Buf_io.input_line chunk.bufio in
            let size = String.strip String.isspace size in
            let size = int_of_string ("0x" ^ size) in
            chunk.current_size <- size;
            chunk.current_offset <- 0;
            chunk.read_headers <- false;
        end ;
        
        (* read as many bytes from this chunk as possible *)
        if chunk.current_size = 0 then ""
	else begin
            let bytes_to_read = min size (chunk.current_size - chunk.current_offset) in
            if bytes_to_read = 0 then ""
            else begin
                let data = String.make bytes_to_read '\000' in
                Buf_io.really_input chunk.bufio data 0 bytes_to_read;

                (* now update the data structure: *)
                if (chunk.current_offset + bytes_to_read) = chunk.current_size then begin
                    (* finished a chunk: get rid of the CRLF *)
                    let blank = "\000\000" in
                    Buf_io.really_input chunk.bufio blank 0 2;
                    if blank <> "\r\n" then failwith "chunked encoding error";
                    chunk.read_headers <- true
                end else begin
                    (* partway through a chunk. *)
                    chunk.current_offset <- (chunk.current_offset + bytes_to_read)
                end; 
                ( data ^ read chunk (size - bytes_to_read) )
            end
        end
end

let read_chunked_encoding req bio = 
  let rec next () = 
    let size = Buf_io.input_line bio in
    (* Strictly speaking need to kill anything past an ';' if present *)
    let size = String.strip String.isspace size in
    let size = int_of_string ("0x" ^ size) in

    if size = 0 then Http.End
    else
      let chunk = String.make size '\000' in
      Buf_io.really_input bio chunk 0 size;
      (* Then get rid of the CRLF *)
      let blank = "\000\000" in
      Buf_io.really_input bio blank 0 2;
      Http.Item (chunk, next) 
  in
  next ()
