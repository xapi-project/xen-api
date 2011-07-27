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

open Stringext
open Pervasiveext

exception Http_parse_failure
exception Unauthorised of string
exception Forbidden
exception Malformed_url of string


module D = Debug.Debugger(struct let name = "http" end)
open D

let http_403_forbidden ?(version="1.1") () =
    [ Printf.sprintf "HTTP/%s 403 Forbidden" version;
      "Connection: close";
      "Cache-Control: no-cache, no-store" ]

let http_200_ok ?(version="1.1") ?(keep_alive=true) () =
    [ Printf.sprintf "HTTP/%s 200 OK" version;
      "Connection: " ^ (if keep_alive then "keep-alive" else "close");
      "Cache-Control: no-cache, no-store" ]

let http_200_ok_with_content length ?(version="1.1") ?(keep_alive=true) () =
    [ Printf.sprintf "HTTP/%s 200 OK" version;
      "Connection: " ^ (if keep_alive then "keep-alive" else "close");
      "Content-Length: "^(Int64.to_string length);
      "Cache-Control: no-cache, no-store" ]

let http_404_missing ?(version="1.1") () =
    [ Printf.sprintf "HTTP/%s 404 Not Found" version;
      "Connection: close";
      "Cache-Control: no-cache, no-store" ]

let http_302_redirect ?(version="1.1") url =
    [ Printf.sprintf "HTTP/%s 302 Found" version;
      "Connection: close";
      "Cache-Control: no-cache, no-store";
      "Location: "^url ]

let http_400_badrequest ?(version="1.1") () =
    [ Printf.sprintf "HTTP/%s 400 Bad Request" version;
      "Connection: close";
      "Cache-Control: no-cache, no-store" ]

let http_401_unauthorised ?(version="1.0") ?(realm="unknown") () =
	[ Printf.sprintf "HTTP/%s 401 Unauthorised" version;
      Printf.sprintf "WWW-Authenticate: Basic realm=\"%s\"" realm;
      "Connection: close";
      "Cache-Control: no-cache, no-store" ]

let http_406_notacceptable ?(version="1.0") () =
  [ Printf.sprintf "HTTP/%s 406 Not Acceptable" version;
    "Connection: close";
    "Cache-Control: no-cache, no-store" ]

let http_500_internal_error ?(version="1.0") () =
  [ Printf.sprintf "HTTP/%s 500 Internal Error" version;
    "Connection: close";
    "Cache-Control: no-cache, no-store" ]

module Hdr = struct
	let task_id = "Task-id"
	let subtask_of = "Subtask-of"
	let content_type = "Content-Type"
	let content_length = "Content-Length"
	let user_agent = "User-Agent"
	let cookie = "Cookie"
	let transfer_encoding = "Transfer-encoding"
	let authorization = "Authorization"
	let connection = "Connection"
end

let output_http fd headers =
	Unixext.really_write_string fd
		(String.concat "" (List.map (fun x -> x ^ "\r\n") headers))

let urldecode url =
    let chars = String.explode url in
    let rec fn ac = function
        |'+'::tl -> fn (' ' :: ac) tl
        |'%'::a::b::tl ->
            let cs = try int_of_string (String.implode ['0';'x';a;b])
            with _ -> raise (Malformed_url url) in
            fn (Char.chr cs :: ac) tl
        |x::tl -> fn (x :: ac) tl
        |[] ->
            String.implode (List.rev ac)
    in fn [] chars

(* Encode @param suitably for appearing in a query parameter in a URL. *)
let urlencode param =
    let chars = String.explode param in
    let rec fn = function
        | x::tl ->
            begin
              let s =
                if x = ' ' then "+"
                else
                  match x with
                    | 'A'..'Z'
                    | 'a'..'z'
                    | '0'..'9'
                    | '$' | '-' | '_' | '.' | '!'
                    | '*' | '\'' | '(' | ')' | ',' ->
                        String.of_char x
                    | _ ->
                        Printf.sprintf "%%%2x" (Char.code x)
              in
                s ^ fn tl
            end
        | [] ->
            ""
    in fn chars

(** Parses strings of the form a=b&c=d into ["a", "b"; "c", "d"] *)
let parse_keyvalpairs xs = 
  let kvpairs = List.map (String.split '=') (String.split '&' xs) in
  List.map (function
	    | k :: vs -> ((urldecode k), urldecode (String.concat "=" vs))
	    | [] -> raise Http_parse_failure) kvpairs

let parse_uri x = match String.split '?' x with
| [ uri ] -> uri, []
| [ uri; params ] -> uri, parse_keyvalpairs params
| _ -> raise Http_parse_failure


type authorization =
    | Basic of string * string
    | UnknownAuth of string
with rpc

let authorization_of_string x =
  let basic = "Basic " in
  if String.startswith basic x
  then
	  let end_of_string s from =
		  String.sub s from ((String.length s)-from) in
    let userpass = Base64.decode (end_of_string x (String.length basic)) in
    match String.split ':' userpass with
    | [ username; password ] -> Basic(username, password)
    | _ -> UnknownAuth x
  else UnknownAuth x

let string_of_authorization = function
| UnknownAuth x -> x
| Basic(username, password) -> "Basic " ^ (Base64.encode (username ^ ":" ^ password))

type method_t = Get | Post | Put | Connect | Unknown of string with rpc

let string_of_method_t = function
  | Get -> "GET" | Post -> "POST" | Put -> "PUT" | Connect -> "CONNECT" | Unknown x -> "Unknown " ^ x
let method_t_of_string = function
  | "GET" -> Get | "POST" -> Post | "PUT" -> Put | "CONNECT" -> Connect | x -> Unknown x

module Request = struct
	type t = {
		m: method_t;
		uri: string;
		query: (string*string) list;
		version: string;
		transfer_encoding: string option;
		content_length: int64 option;
		auth: authorization option;
		cookie: (string * string) list;
		task: string option;
		subtask_of: string option;
		content_type: string option;
		user_agent: string option;
		mutable close: bool;
		additional_headers: (string*string) list;
		body: string option;
	} with rpc

	let empty = {
		m=Unknown "";
		uri="";
		query=[];
		version="";
		transfer_encoding=None;
		content_length=None;
		auth=None;
		cookie=[];
		task=None;
		subtask_of=None;
		content_type = None;
		user_agent = None;
		close= true;
		additional_headers=[];
		body = None;
	}

	let make ?(version="1.0") ?(keep_alive=false) ?cookie ?length ?subtask_of ?body ?(headers=[]) ?content_type ~user_agent meth path = 
		{ empty with
			version = version;
			close = not keep_alive;
			cookie = Opt.default [] cookie;
			subtask_of = subtask_of;
			content_length = length;
			content_type = content_type;
			user_agent = Some user_agent;
			m = meth;
			uri = path;
			additional_headers = headers;
			body = body;
		}

	let get_version x = x.version

	let of_request_line x = match String.split_f String.isspace x with
		| [ m; uri; version ] ->
			(* Request-Line   = Method SP Request-URI SP HTTP-Version CRLF *)
			let uri, query = parse_uri uri in
			{ m = method_t_of_string m; uri = uri; query = query;
			content_length = None; transfer_encoding = None;
			version = version; cookie = []; auth = None; task = None;
			subtask_of = None; content_type = None; user_agent = None;
			close=false; additional_headers=[]; body = None }
		| _ -> raise Http_parse_failure

	let to_string x =
		let kvpairs x = String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x) in
		Printf.sprintf "{ method = %s; uri = %s; query = [ %s ]; content_length = [ %s ]; transfer encoding = %s; version = %s; cookie = [ %s ]; task = %s; subtask_of = %s; content-type = %s; user_agent = %s }" 
			(string_of_method_t x.m) x.uri
			(kvpairs x.query)
			(default "" (may Int64.to_string x.content_length))
			(default "" x.transfer_encoding)
			x.version
			(kvpairs x.cookie)
			(default "" x.task)
			(default "" x.subtask_of)
			(default "" x.content_type)
			(default "" x.user_agent)

	let to_header_list x =
		let kvpairs x = String.concat "&" (List.map (fun (k, v) -> urlencode k ^ "=" ^ (urlencode v)) x) in
		let query = if x.query = [] then "" else "?" ^ (kvpairs x.query) in
		let cookie = if x.cookie = [] then [] else [ Hdr.cookie ^": " ^ (kvpairs x.cookie) ] in
		let transfer_encoding = Opt.default [] (Opt.map (fun x -> [ Hdr.transfer_encoding ^": " ^ x ]) x.transfer_encoding) in
		let content_length = Opt.default [] (Opt.map (fun x -> [ Printf.sprintf "%s: %Ld" Hdr.content_length x ]) x.content_length) in
		let auth = Opt.default [] (Opt.map (fun x -> [ Hdr.authorization ^": " ^ (string_of_authorization x) ]) x.auth) in
		let task = Opt.default [] (Opt.map (fun x -> [ Hdr.task_id ^ ": " ^ x ]) x.task) in
		let subtask_of = Opt.default [] (Opt.map (fun x -> [ Hdr.subtask_of ^ ": " ^ x ]) x.subtask_of) in
		let content_type = Opt.default [] (Opt.map (fun x -> [ Hdr.content_type ^": " ^ x ]) x.content_type) in
		let user_agent = Opt.default [] (Opt.map (fun x -> [ Hdr.user_agent^": " ^ x ]) x.user_agent) in
		let close = [ Hdr.connection ^": " ^ (if x.close then "close" else "keep-alive") ] in
		[ Printf.sprintf "%s %s%s HTTP/%s" (string_of_method_t x.m) x.uri query x.version ]
		@ cookie @ transfer_encoding @ content_length @ auth @ task @ subtask_of @ content_type @ user_agent @ close
		@ (List.map (fun (k, v) -> k ^ ":" ^ v) x.additional_headers)

	let to_wire_string (x: t) =
		(* If the body is given then compute a content length *)
		let x = match x.body with
			| None -> x
			| Some b -> { x with content_length = Some (Int64.of_int (String.length b)) } in
		let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") (to_header_list x @ [""])) in
		let body = Opt.default "" x.body in
		headers ^ body

end

module Response = struct
	type t = {
		version: string;
		code: string;
		message: string;
		content_length: int64 option;
		task: string option;
		additional_headers: (string*string) list;
		body: string option;
	}
	let to_string x =
		let kvpairs x = String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x) in
		Printf.sprintf "{ version = %s; code = %s; message = %s; content_length = %s; task = %s; additional_headers = [ %s ] }"
			x.version x.code x.message
			(Opt.default "None" (Opt.map (fun x -> "Some " ^ (Int64.to_string x)) x.content_length))
			(Opt.default "None" (Opt.map (fun x -> "Some " ^ x) x.task))
			(kvpairs x.additional_headers)

	let empty = {
		version = "1.0";
		code = "500";
		message = "Unknown error message";
		content_length = None;
		task = None;
		additional_headers = [];
		body = None;
	}
	let make ?(version="1.0") ?length ?task ?(headers=[]) ?body code message = {
		version = version;
		code = code;
		message = message;
		content_length = length;
		task = task;
		additional_headers = headers;
		body = body
	}

	let internal_error = { empty with code = "500"; message = "internal error"; content_length = Some 0L }
	let to_header_list (x: t) =
		let status = Printf.sprintf "HTTP/%s %s %s" x.version x.code x.message in
		let content_length = Opt.default [] (Opt.map (fun x -> [ Printf.sprintf "%s: %Ld" Hdr.content_length x ]) x.content_length) in
		let task = Opt.default [] (Opt.map (fun x -> [ Hdr.task_id ^ ": " ^ x ]) x.task) in
		let headers = List.map (fun (k, v) -> k ^ ":" ^ v) x.additional_headers in
		status :: (content_length @ task @ headers)

	let to_wire_string (x: t) =
		(* If the body is given then compute a content length *)
		let x = match x.body with
			| None -> x
			| Some b -> { x with content_length = Some (Int64.of_int (String.length b)) } in
		let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") (to_header_list x @ [""])) in
		let body = Opt.default "" x.body in
		headers ^ body
end


(* For transfer-encoding: chunked *)

type 'a ll = End | Item of 'a * (unit -> 'a ll)

let rec ll_iter f = function
  | End -> ()
  | Item (x, xs) -> f x; ll_iter f (xs ())
