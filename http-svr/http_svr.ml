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

(** Type of a function which can handle a request *)
type bufio_req_handler = request -> Buf_io.t -> unit
type fdio_req_handler = request -> Unix.file_descr -> unit
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


(* If http/1.0 was requested, return that, else return http/1.1 *)
let get_return_version req =
  try
    let (maj,min) = Scanf.sscanf req.version "HTTP/%d.%d" (fun a b -> (a,b)) in
    match (maj,min) with
	(1,0) -> "1.0"
      | _ -> "1.1"
  with _ -> "1.1"
    
let response_fct req ?(hdrs=[]) s (response_length: int64) (write_response_to_fd_fn: Unix.file_descr -> unit) = 
  let version = get_return_version req in
  let keep_alive = if req.close then false else true in
  headers s ((http_200_ok_with_content response_length ~version ~keep_alive ()) @ hdrs);
  write_response_to_fd_fn s

let response_str req ?hdrs s response = 
  let length = String.length response in
  response_fct req ?hdrs s (Int64.of_int length) (fun s -> ignore(Unix.write s response 0 length))

let response_missing ?(hdrs=[]) s response =
  let length = String.length response in
  (* XXX: output_http puts a \r\n on the end of everything... *)
  let len_hdr = "Content-Length: "^string_of_int (length + 2) in
  headers s (http_404_missing @ (len_hdr::hdrs));
  output_http s [response]

(* Should this have a content length? *)
let response_redirect s url =
  headers s (http_302_redirect url);
  output_http s [url]

let response_unauthorised label s =
  let myheaders = Http.http_401_unauthorised ~realm:label () in
  headers s myheaders

let response_forbidden s =
  headers s (Http.http_403_forbidden@["Content-Type: text/html"]);
  output_http s ["<html><body><h1>403: Forbidden</h1></body></html>"]

let response_file ?(hdrs=[]) ~mime_content_type s file =
  (* XXX: replace with Unixext.copy_file *)
  let st = Unix.LargeFile.stat file in
  let size = st.Unix.LargeFile.st_size in
  let mime_header =
    match mime_content_type with
    | None    -> []
    | Some ty -> [ "Content-Type: " ^ ty ]
    in
  headers s (http_200_ok_with_content size ~version:"1.1" ~keep_alive:true ()
            @ mime_header);
  let buffer = String.make 65536 '\000' in
  let ic = open_in file in
  Pervasiveext.finally (fun () -> 
    let rec inner bytes =
      if bytes=0 then () else 
	let n = min (String.length buffer) bytes in
	really_input ic buffer 0 n;
	ignore_int (Unix.write s buffer 0 n);
	inner (bytes - n) in
    inner (Int64.to_int size)) 
    (fun () -> 
      close_in ic)

(** If no handler matches the request then call this callback *)
let default_callback req bio = 
  response_forbidden (Buf_io.fd_of bio);
  req.close <- true
    

let write_error bio message =
  try
    let response=Printf.sprintf "<html><body><h1>Internal Server Error</h1>%s</body></html>" message in
    let length = String.length response in
    let fd = Buf_io.fd_of bio in
    headers fd Http.http_500_internal_error;
    ignore(Unix.write fd response 0 length)
  with _ -> ()

exception Too_many_headers
exception Generic_error of string

let handle_connection _ ss =
    (* Try to keep the connection open for a while to prevent spurious End_of_file type 
	   problems under load *)
	let initial_timeout = 5. *. 60. in
  
  let ic = Buf_io.of_fd ss in
  let finished = ref false in
  let reqno = ref 0 in

  while not !finished do
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

    try
      let request_line = Buf_io.input_line ~timeout:initial_timeout ic in
      let req = request_of_string request_line in

(*      let _ = myprint "read request line" in
      let _ = myprint request_line in
      let _ = myprint (pretty_string_of_request req) in *)

      reqno := !reqno + 1;

      (* Default for HTTP/1.1 is persistent connections. Anything else closes *)
      (* the channel as soon as the request is processed *)
      if req.version <> "HTTP/1.1" then req.close <- true;
      
      let rec read_rest_of_headers left =
	let cl_hdr = "content-length: " in
	let cookie_hdr = "cookie: " in
	let connection_hdr = "connection: " in
	let transfer_encoding_hdr = "transfer-encoding: " in
	let auth_hdr = "authorization: " in
	let task_hdr = String.lowercase Http.task_id_hdr ^ ": " in
	let subtask_of_hdr = String.lowercase Http.subtask_of_hdr ^ ": " in
	let content_type_hdr = String.lowercase Http.content_type_hdr ^ ": " in
	let user_agent_hdr = String.lowercase Http.user_agent_hdr ^ ": " in
	let r = Buf_io.input_line ~timeout:Buf_io.infinite_timeout ic in
	let r = strip_cr r in
	let lowercase_r = String.lowercase r in
	if String.startswith cl_hdr lowercase_r then
	  begin
	    let clstr = end_of_string r (String.length cl_hdr) in
	    content_length := Int64.of_string clstr
	  end;
	if String.startswith cookie_hdr lowercase_r then
	  cookie := end_of_string r (String.length cookie_hdr);
	if String.startswith transfer_encoding_hdr lowercase_r then
	  transfer_encoding := Some (end_of_string r (String.length transfer_encoding_hdr));
	if String.startswith auth_hdr lowercase_r
	then auth := Some(authorization_of_string (end_of_string r (String.length auth_hdr)));
	if String.startswith task_hdr lowercase_r
	then task := Some (end_of_string r (String.length task_hdr));
	if String.startswith subtask_of_hdr lowercase_r
	then subtask_of := Some (end_of_string r (String.length subtask_of_hdr));
	if String.startswith content_type_hdr lowercase_r
	then content_type := Some (end_of_string r (String.length content_type_hdr));
	if String.startswith user_agent_hdr lowercase_r
	then user_agent := Some (end_of_string r (String.length user_agent_hdr));
	if String.startswith connection_hdr lowercase_r 
	then
	  begin
	    let token = String.lowercase (end_of_string r (String.length connection_hdr)) in
	    match token with
	    | "keep-alive" -> req.close <- false
	    | "close" -> req.close <- true
            | _ -> ()
	  end;
	if r <> "" then (
	  if left > 0 then
	    r :: (read_rest_of_headers (left - 1))
	  else
	    raise Too_many_headers
	) else
	  [] in
      let headers = read_rest_of_headers 242 in
      let req = { req with 
		    cookie = (Http.parse_keyvalpairs !cookie);
		    content_length = if !content_length = -1L then None else Some(!content_length);
		    auth = !auth;
		    task = !task;
		    subtask_of = !subtask_of;
			content_type = !content_type;
		    user_agent = !user_agent;
		    headers = headers;
		} in
      let ty = Http.string_of_method_t req.m in
      D.debug "HTTP %s %s %s%s%s%s%s"
	ty req.uri 
	(Opt.default " " (Opt.map (fun x -> Printf.sprintf " (Content-length: %Ld)" x) req.content_length))
	(Opt.default " " (Opt.map (fun x -> Printf.sprintf " (Task: %s)" x) req.task))
	(Opt.default " " (Opt.map (fun x -> Printf.sprintf " (Subtask-of: %s)" x) req.subtask_of))
	(Opt.default " " (Opt.map (fun x -> Printf.sprintf " (Content-Type: %s)" x) req.content_type))
	(Opt.default " " (Opt.map (fun x -> Printf.sprintf " (User-agent: %s)" x) req.user_agent));
      let table = handler_table req.m in
      (* Find a specific handler: the last one whose URI is a prefix of the received
	 URI defaulting to 'default_callback' if none can be found *)
      let uris = Hashtbl.fold (fun k v a -> k::a) table [] in
      let uris = List.sort (fun a b -> compare (String.length b) (String.length a)) uris in
      let handlerfn = 
	try
	  let longest_match = List.find (fun uri -> String.startswith uri req.uri) uris in
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
      finished := (req.close)
    with
      End_of_file -> 
	DCritical.debug "Premature termination of connection!";
	finished := true
    | Http.Http_parse_failure ->
	myprint "Error parsing HTTP headers";
	finished := true;
	write_error ic "Error parsing HTTP headers"
    | Too_many_headers ->
	(* don't log anything, since it could fill the log *)
	finished := true;
	write_error ic "Error reading HTTP headers: too many headers"
    | Buf_io.Timeout ->
	DCritical.debug "Idle connection closed after %.0f seconds" initial_timeout; (* NB infinite timeout used when headers are being read *)
	finished := true
    | Buf_io.Eof ->
	DCritical.debug "Connection terminated";
	finished := true
    | Buf_io.Line x ->
	begin
	  match x with
	    Buf_io.Too_long ->
	      DCritical.debug "Line too long!"
	  | Buf_io.No_newline ->
	      DCritical.debug "Newline not found!"
	end;
	DCritical.debug "Buffer contains: '%s'" (Buf_io.get_data ic);
	finished := true;
	write_error ic ""
    | Unix.Unix_error (a,b,c) ->
	DCritical.debug "Unhandled unix exception: %s %s %s" (Unix.error_message a) b c;
	finished := true;
	write_error ic (Printf.sprintf "Got UNIX error: %s %s %s" (Unix.error_message a) b c)
    | Generic_error s ->
	finished := true;
	write_error ic s
    | Http.Unauthorised realm ->
	let fd = Buf_io.fd_of ic in
	response_unauthorised realm fd;
	finished := true;	  
    | Http.Forbidden ->
	let fd = Buf_io.fd_of ic in
	response_forbidden fd;
	finished := true;	  
    | exc ->
	DCritical.debug "Unhandled exception: %s" (Printexc.to_string exc);
	log_backtrace ();
	write_error ic (escape (Printexc.to_string exc));
	finished := true

  done;
  
  best_effort (fun () ->
    debug "Shutting down connection...";
    Unix.close ss
  )

let bind ?(listen_backlog=128) sockaddr = 
  let domain = match sockaddr with
    | Unix.ADDR_UNIX path ->
	myprint "Establishing Unix domain server on path: %s" path;
	Unix.PF_UNIX
    | Unix.ADDR_INET(_,_) ->
	myprint "Establishing inet domain server";
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
	match req.content_length with
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
