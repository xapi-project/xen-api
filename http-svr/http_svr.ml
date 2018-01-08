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

open Xapi_stdext_monadic
open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_threads
open Xapi_stdext_unix

(* This resolves the lowercase deprecation for all compiler versions *)
let lowercase = Astring.String.Ascii.lowercase

module D = Debug.Make(struct let name="http" end)
open D

type uri_path = string

module Stats = struct
  (** Record of statistics per-handler *)

  type t = {
    mutable n_requests: int; (** successful requests *)
    mutable n_connections: int; (** closed connections *)
    mutable n_framed: int; (** using the more efficient framed protocol *)
  }
  let empty () = {
    n_requests = 0;
    n_connections = 0;
    n_framed = 0;
  }
  let update (x: t) (m: Mutex.t) req =
    Threadext.Mutex.execute m
      (fun () ->
         x.n_requests <- x.n_requests + 1;
         if req.Http.Request.close then x.n_connections <- x.n_connections + 1;
         if req.Http.Request.frame then x.n_framed <- x.n_framed + 1;
      )
end

(** Type of a function which can handle a Request.t *)
type 'a handler =
  | BufIO of (Http.Request.t -> Buf_io.t -> 'a -> unit)
  | FdIO of (Http.Request.t -> Unix.file_descr -> 'a -> unit)

(* try and do f (unit -> unit), ignore exceptions *)
let best_effort f =
  try f() with _ -> ()

let headers s headers = 
  output_http s headers;
  output_http s [""]

(* let response s hdrs length f =
  output_http s hdrs;
  output_http s [ Printf.sprintf "Content-Length: %Ld" length ];
  output_http s [ "" ];
  f s *)

(* If http/1.0 was requested, return that, else return http/1.1 *)
let get_return_version req =
  try
    let (maj,min) = Scanf.sscanf (Request.get_version req) "%d.%d" (fun a b -> (a,b)) in
    match (maj,min) with
      (1,0) -> "1.0"
    | _ -> "1.1"
  with _ -> "1.1"

let response_of_request req hdrs =
  let connection = Http.Hdr.connection, if req.Request.close then "close" else "keep-alive" in
  let cache = Http.Hdr.cache_control, "no-cache, no-store" in
  Http.Response.make
    ~version:(get_return_version req) ~frame:req.Http.Request.frame
    ~headers:(connection :: cache :: hdrs) "200" "OK"

let response_fct req ?(hdrs=[]) s (response_length: int64) (write_response_to_fd_fn: Unix.file_descr -> unit) = 
  let res = { (response_of_request req hdrs) with Http.Response.content_length = Some response_length } in
  Unixext.really_write_string s (Http.Response.to_wire_string res);
  write_response_to_fd_fn s

let response_str req ?hdrs s body =
  let length = String.length body in
  response_fct req ?hdrs s (Int64.of_int length) (fun s -> Unixext.really_write_string s body)

let response_missing ?(hdrs=[]) s body =
  let connection = Http.Hdr.connection, "close" in
  let cache = Http.Hdr.cache_control, "no-cache, no-store" in
  let res = Http.Response.make
      ~version:"1.1" ~headers:(connection :: cache :: hdrs)
      ~body "404" "Not Found" in
  Unixext.really_write_string s (Http.Response.to_wire_string res)

let response_error_html ?(version="1.1") s code message hdrs body =
  let connection = Http.Hdr.connection, "close" in
  let cache = Http.Hdr.cache_control, "no-cache, no-store" in
  let content_type = Http.Hdr.content_type, "text/html" in
  let res = Http.Response.make
      ~version ~headers:(content_type :: connection :: cache :: hdrs)
      ~body code message in
  Unixext.really_write_string s (Http.Response.to_wire_string res)

let response_unauthorised ?req label s =
  let version = Opt.map get_return_version req in
  let body = "<html><body><h1>HTTP 401 unauthorised</h1>Please check your credentials and retry.</body></html>" in
  let realm = "WWW-Authenticate", Printf.sprintf "Basic realm=\"%s\"" label in
  response_error_html ?version s "401" "Unauthorised" [ realm ] body

let response_forbidden ?req s =
  let version = Opt.map get_return_version req in
  let body = "<html><body><h1>HTTP 403 forbidden</h1>Access to the requested resource is forbidden.</body></html>" in	
  response_error_html ?version s "403" "Forbidden" [] body

let response_badrequest ?req s =
  let version = Opt.map get_return_version req in
  let body = "<html><body><h1>HTTP 400 bad request</h1>The HTTP request was malformed. Please correct and retry.</body></html>" in
  response_error_html ?version s "400" "Bad Request" [] body

let response_internal_error ?req ?extra s =
  let version = Opt.map get_return_version req in
  let extra = Opt.default "" (Opt.map (fun x -> "<h1> Additional information </h1>" ^ x) extra) in
  let body = "<html><body><h1>HTTP 500 internal server error</h1>An unexpected error occurred; please wait a while and try again. If the problem persists, please contact your support representative." ^ extra ^ "</body></html>" in
  response_error_html ?version s "500" "Internal Error" [] body

let response_method_not_implemented ?req s =
  let version = Opt.map get_return_version req in
  let extra = Opt.default "" (Opt.map (fun req ->
      Printf.sprintf "<p>%s not supported.<br /></p>" (Http.string_of_method_t req.Http.Request.m)
    ) req) in
  let body = "<html><body><h1>HTTP 501 Method Not Implemented</h1>" ^ extra ^ "</body></html>" in
  response_error_html ?version s "501" "Method not implemented" [] body

let response_file ?mime_content_type s file =
  let size = (Unix.LargeFile.stat file).Unix.LargeFile.st_size in
  let mime_header = Opt.default [] (Opt.map (fun ty -> [ Hdr.content_type, ty ]) mime_content_type) in
  let keep_alive = Http.Hdr.connection, "keep-alive" in
  let res = Http.Response.make ~version:"1.1" ~headers:(keep_alive :: mime_header)
      ~length:size "200" "OK" in
  Unixext.with_file file [ Unix.O_RDONLY ] 0
    (fun f ->
       Unixext.really_write_string s (Http.Response.to_wire_string res);
       let (_: int64) = Unixext.copy_file f s in
       ()
    )

let respond_to_options req s =
  let access_control_allow_headers = 
    try
      let acrh = List.assoc Hdr.acrh req.Request.additional_headers in
      Printf.sprintf "%s, X-Requested-With" acrh
    with Not_found -> 
      "X-Requested-With"
  in
  response_fct req ~hdrs:[
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Headers", access_control_allow_headers;
    "Access-Control-Allow-Methods","PUT"] s 0L (fun _ -> ())


(** If no handler matches the request then call this callback *)
let default_callback req bio _ = 
  response_forbidden (Buf_io.fd_of bio);
  req.Request.close <- true


module TE = struct
  type 'a t = {
    stats: Stats.t;
    stats_m: Mutex.t;
    handler: 'a handler
  }
  let empty () = {
    stats = Stats.empty ();
    stats_m = Mutex.create ();
    handler = BufIO default_callback;
  }
end


module MethodMap = Map.Make(struct type t = Http.method_t let compare = compare end)

module Server = struct
  type 'a t = {
    mutable handlers: 'a TE.t Radix_tree.t MethodMap.t;
    mutable use_fastpath: bool;
    default_context: 'a;
  }

  let empty default_context = {
    handlers = MethodMap.empty;
    use_fastpath = false;
    default_context = default_context;
  }

  let add_handler x ty uri handler =
    let existing =
      if MethodMap.mem ty x.handlers
      then MethodMap.find ty x.handlers
      else Radix_tree.empty in
    x.handlers <- MethodMap.add ty (Radix_tree.insert uri { (TE.empty ()) with TE.handler = handler } existing) x.handlers

  let find_stats x m uri =
    if not (MethodMap.mem m x.handlers)
    then None
    else
      let rt = MethodMap.find m x.handlers in
      Opt.map (fun te -> te.TE.stats) (Radix_tree.longest_prefix uri rt)

  let all_stats x =
    let open Radix_tree in
    MethodMap.fold (fun m rt acc ->
        fold (fun k te acc -> (m, k, te.TE.stats) :: acc) acc rt
      ) x.handlers []

  let enable_fastpath x = x.use_fastpath <- true
end

let escape uri =
  (* from xapi-stdext-std xstringext *)
  let escaped ~rules string =
    let aux h t = (
      if List.mem_assoc h rules
      then List.assoc h rules
      else Astring.String.of_char h) :: t 
    in
    String.concat "" (Astring.String.fold_right aux string [])
  in
  escaped ~rules:[ '<', "&lt;"
                 ; '>', "&gt;"
                 ; '\'', "&apos;"
                 ; '"', "&quot;"
                 ; '&', "&amp;"
                 ] uri

exception Too_many_headers
exception Generic_error of string

let request_of_bio_exn_slow ic =
  (* Try to keep the connection open for a while to prevent spurious End_of_file type 
     	   problems under load *)
  let initial_timeout = 5. *. 60. in

  let content_length = ref (-1L) in
  let cookie = ref "" in
  let transfer_encoding = ref None in
  let accept = ref None in
  let auth = ref None in
  let task = ref None in
  let subtask_of = ref None in
  let content_type = ref None in
  let host = ref None in
  let user_agent = ref None in

  content_length := -1L;
  cookie := "";

  let request_line = Buf_io.input_line ~timeout:initial_timeout ic in
  let req = Request.of_request_line request_line in

  (* Default for HTTP/1.1 is persistent connections. Anything else closes *)
  (* the channel as soon as the request is processed *)
  if req.Request.version <> "1.1" then req.Request.close <- true;

  let rec read_rest_of_headers left =
    let cl_hdr = lowercase Http.Hdr.content_length in
    let cookie_hdr = lowercase Http.Hdr.cookie in
    let connection_hdr = lowercase Http.Hdr.connection in
    let transfer_encoding_hdr = lowercase Http.Hdr.transfer_encoding in
    let accept_hdr = lowercase Http.Hdr.accept in
    let auth_hdr = lowercase Http.Hdr.authorization in
    let task_hdr = lowercase Http.Hdr.task_id in
    let subtask_of_hdr = lowercase Http.Hdr.subtask_of in
    let content_type_hdr = lowercase Http.Hdr.content_type in
    let host_hdr = lowercase Http.Hdr.host in
    let user_agent_hdr = lowercase Http.Hdr.user_agent in
    let r = Buf_io.input_line ~timeout:Buf_io.infinite_timeout ic in
    match Astring.String.cut ~sep:":" r with
    | Some (k, v) ->
      let k = lowercase k in
      let v = String.trim v in
      let absorbed = match k with
        | k when k = cl_hdr -> content_length := Int64.of_string v; true
        | k when k = cookie_hdr -> cookie := v; true
        | k when k = transfer_encoding_hdr -> transfer_encoding := Some v; true
        | k when k = accept_hdr -> accept := Some v; true
        | k when k = auth_hdr -> auth := Some(authorization_of_string v); true
        | k when k = task_hdr -> task := Some v; true
        | k when k = subtask_of_hdr -> subtask_of := Some v; true
        | k when k = content_type_hdr -> content_type := Some v; true
        | k when k = host_hdr -> host := Some v; true
        | k when k = user_agent_hdr -> user_agent := Some v; true
        | k when k = connection_hdr ->
          req.Request.close <- lowercase v = "close";
          true
        | _ -> false in
      if not absorbed && left <= 0 then raise Too_many_headers;
      if absorbed
      then read_rest_of_headers (left - 1)
      else (k, v) :: (read_rest_of_headers (left - 1))
    | None -> [] in
  let headers = read_rest_of_headers 242 in
  { req with
    Request.cookie = (Http.parse_keyvalpairs !cookie);
    content_length = if !content_length = -1L then None else Some(!content_length);
    auth = !auth;
    task = !task;
    subtask_of = !subtask_of;
    content_type = !content_type;
    host = !host;
    user_agent = !user_agent;
    additional_headers = headers;
    accept = !accept;
  }

(** [request_of_bio_exn ic] reads a single Http.req from [ic] and returns it. On error
    	it simply throws an exception and doesn't touch the output stream. *)

let request_of_bio_exn bio =
  let fd = Buf_io.fd_of bio in

  let buf = Bytes.create 1024 in
  let b, frame = Http.read_http_request_header buf fd in
  let buf = Bytes.sub buf 0 b in
(*
	Printf.printf "parsed = [%s]\n" buf;
	flush stdout;
*)
  let open Http.Request in
  snd(List.fold_left
        (fun (status, req) header ->
           if not status then begin
             match Astring.String.fields header with
             | [ meth; uri; version ] ->
               (* Request-Line   = Method SP Request-URI SP HTTP-Version CRLF *)
               let uri, query = Http.parse_uri uri in
               let m = Http.method_t_of_string meth in
               let version =
                 let x = String.trim version in
                 let prefix = "HTTP/" in
                 String.sub x (String.length prefix) (String.length x - (String.length prefix)) in
               let close = version = "1.0" in
               true,
               { req with m = m; uri = uri; query = query;
                          version = version; close = close
               }
             | _ -> raise Http_parse_failure
           end else begin
             match Astring.String.cut ~sep:":" header with
             | Some (k, v) ->
               let k = lowercase k in
               let v = String.trim v in
               true, begin match k with
                 | k when k = Http.Hdr.content_length -> { req with content_length = Some (Int64.of_string v) }
                 | k when k = Http.Hdr.cookie -> { req with cookie = Http.parse_keyvalpairs v }
                 | k when k = Http.Hdr.transfer_encoding -> { req with transfer_encoding = Some v }
                 | k when k = Http.Hdr.accept -> { req with accept = Some v }
                 | k when k = Http.Hdr.authorization -> { req with auth = Some(authorization_of_string v) }
                 | k when k = Http.Hdr.task_id -> { req with task = Some v }
                 | k when k = Http.Hdr.subtask_of -> { req with subtask_of = Some v }
                 | k when k = Http.Hdr.content_type -> { req with content_type = Some v }
                 | k when k = Http.Hdr.host -> { req with host = Some v }
                 | k when k = Http.Hdr.user_agent -> { req with user_agent = Some v }
                 | k when k = Http.Hdr.connection && lowercase v = "close" -> { req with close = true }
                 | k when k = Http.Hdr.connection && lowercase v = "keep-alive" -> { req with close = false }
                 | _ -> { req with additional_headers = (k, v) :: req.additional_headers }
               end
             | None -> true, req (* end of headers *)
           end
        ) (false, { empty with Http.Request.frame = frame }) (Astring.String.cuts ~sep:"\n" buf))

(** [request_of_bio ic] returns [Some req] read from [ic], or [None]. If [None] it will have
    	already sent back a suitable error code and response to the client. *)
let request_of_bio ?(use_fastpath=false) ic =
  try
    let r = (if use_fastpath then request_of_bio_exn else request_of_bio_exn_slow) ic in
(*
		Printf.fprintf stderr "Parsed [%s]\n" (Http.Request.to_wire_string r);
		flush stderr;
*)
    Some r
  with e ->
    Printf.fprintf stderr "%s" (Printexc.to_string e);
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
        | Buf_io.Timeout -> ()
        (* Idle connection closed. NB infinite timeout used when headers are being read *)
        | Buf_io.Eof -> ()
        (* Connection terminated *)
        | Buf_io.Line _ ->
          response_internal_error ss ~extra:"One of the header lines was too long.";
          (* Generic errors thrown during parsing *)
        | End_of_file -> ()
        (* Premature termination of connection! *)
        | Unix.Unix_error (a,b,c) ->
          response_internal_error ss ~extra:(Printf.sprintf "Got UNIX error: %s %s %s" (Unix.error_message a) b c)
        | exc ->
          response_internal_error ss ~extra:(escape (Printexc.to_string exc));
          log_backtrace ();
      );
    None

let handle_one (x: 'a Server.t) ss context req =
  let ic = Buf_io.of_fd ss in
  let finished = ref false in
  try
    D.debug "Request %s" (Http.Request.to_string req);
    let method_map = try MethodMap.find req.Request.m x.Server.handlers with Not_found -> raise Method_not_implemented in
    let empty = TE.empty () in
    let te = Opt.default empty (Radix_tree.longest_prefix req.Request.uri method_map) in
    (match te.TE.handler with
     | BufIO handlerfn -> handlerfn req ic context
     | FdIO handlerfn ->
       let fd = Buf_io.fd_of ic in
       Buf_io.assert_buffer_empty ic;
       handlerfn req fd context
    );
    finished := (req.Request.close);
    Stats.update te.TE.stats te.TE.stats_m req;
    !finished
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
        | Http.Method_not_implemented ->
          response_method_not_implemented ~req ss
        | End_of_file -> ()
        (* Premature termination of connection! *)
        | Unix.Unix_error (a,b,c) ->
          response_internal_error ~req ss ~extra:(Printf.sprintf "Got UNIX error: %s %s %s" (Unix.error_message a) b c)
        | exc ->
          response_internal_error ~req ss ~extra:(escape (Printexc.to_string exc));
          log_backtrace ()			
      );
    !finished

let handle_connection (x: 'a Server.t) _ ss =
  let ic = Buf_io.of_fd ss in
  let finished = ref false in

  while not !finished do
    (* 1. we must successfully parse a request *)
    let req = request_of_bio ~use_fastpath:x.Server.use_fastpath ic in

    (* 2. now we attempt to process the request *)
    finished := Opt.default true (Opt.map (handle_one x ss x.Server.default_context) req);
  done;
  Unix.close ss

let bind ?(listen_backlog=128) sockaddr name =
  let domain = match sockaddr with
    | Unix.ADDR_UNIX path ->
      debug "Establishing Unix domain server on path: %s" path;
      Unix.PF_UNIX
    | Unix.ADDR_INET(_,_) ->
      debug "Establishing inet domain server";
      Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  (* Make sure exceptions cause the socket to be closed *)
  try
    Unix.set_close_on_exec sock;
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.setsockopt sock Unix.SO_KEEPALIVE true;
    (match sockaddr with Unix.ADDR_INET _ -> Unixext.set_tcp_nodelay sock true | _ -> ());
    Unix.bind sock sockaddr;
    Unix.listen sock listen_backlog;
    sock, name
  with e ->
    debug "Caught exception in Http_svr.bind (closing socket): %s" (Printexc.to_string e);
    Unix.close sock;
    raise e

let bind_retry ?(listen_backlog=128) sockaddr =
  let description = match sockaddr with
    | Unix.ADDR_INET(ip, port) -> Printf.sprintf "INET %s:%d" (Unix.string_of_inet_addr ip) port
    | Unix.ADDR_UNIX path -> Printf.sprintf "UNIX %s" path in
  (* Sometimes we see failures which we hope are transient. If this
     	   happens then we'll retry a couple of times before failing. *)
  let result = ref None in
  let start = Unix.gettimeofday () in
  let timeout = 30.0 in (* 30s *)
  while !result = None && (Unix.gettimeofday () -. start < timeout) do
    try
      result := Some (bind ~listen_backlog sockaddr description);
    with Unix.Unix_error(code, _, _) ->
      debug "While binding %s: %s" description (Unix.error_message code);
      Thread.delay 5.
  done;
  match !result with
  | None -> failwith (Printf.sprintf "Repeatedly failed to bind: %s" description)
  | Some s ->
    info "Successfully bound socket to: %s" description;
    s

(* Maps sockets to Server_io.server records *)
let socket_table = Hashtbl.create 10

type socket = Unix.file_descr * string

(* Start an HTTP server on a new socket *)
let start (x: 'a Server.t) (socket, name) =
  let handler = { Server_io.name = name;
                  body = handle_connection x } in
  let server = Server_io.server handler socket in
  Hashtbl.add socket_table socket server

exception Socket_not_found
(* Stop an HTTP server running on a socket *)
let stop (socket, _name) =
  let server = try Hashtbl.find socket_table socket with Not_found -> raise Socket_not_found in
  Hashtbl.remove socket_table socket;
  server.Server_io.shutdown ()

exception Client_requested_size_over_limit

(** Read the body of an HTTP request (requires a content-length: header). *)
let read_body ?limit req bio =
  match req.Request.content_length with
  | None -> failwith "We require a content-length: HTTP header"
  | Some length ->
    let length = Int64.to_int length in
    maybe (fun l -> if length > l then raise Client_requested_size_over_limit) limit;
    if Buf_io.is_buffer_empty bio then Unixext.really_read_string (Buf_io.fd_of bio) length
    else Buf_io.really_input_buf ~timeout:Buf_io.infinite_timeout bio length

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
      let size = String.trim size in
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

let read_chunked_encoding _req bio = 
  let rec next () = 
    let size = Buf_io.input_line bio in
    (* Strictly speaking need to kill anything past an ';' if present *)
    let size = String.trim size in
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
