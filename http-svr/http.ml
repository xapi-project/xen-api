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

module D = Debug.Debugger(struct let name = "http" end)
open D

let http_403_forbidden =
    [ "HTTP/1.1 403 Forbidden";
      "Connection: close";
      "Cache-Control: no-cache, no-store" ]

let http_200_ok ?(version="1.1") ?(keep_alive=true) () =
    [ Printf.sprintf "HTTP/%s 200 OK" version;
      "Connection: " ^ (if keep_alive then "keep-alive" else "close");
      "Cache-Control: no-cache, no-store" ]

let http_200_ok_with_content length ?(version="HTTP/1.1") ?(keep_alive=true) () =
    [ version^" 200 OK";
      "Connection: " ^ (if keep_alive then "keep-alive" else "close");
      "Content-Length: "^(Int64.to_string length);
      "Cache-Control: no-cache, no-store" ]

let http_404_missing =
    [ "HTTP/1.1 404 Not Found";
      "Connection: close";
      "Cache-Control: no-cache, no-store" ]

let http_302_redirect url =
    [ "HTTP/1.1 302 Found";
      "Connection: close";
      "Cache-Control: no-cache, no-store";
      "Location: "^url ]

let http_400_badrequest =
      [ "HTTP/1.1 400 Bad Request";
      "Connection: close";
      "Cache-Control: no-cache, no-store" ]

let http_401_unauthorised ?(realm="unknown") () = 
  [ "HTTP/1.0 401 Unauthorised";
    Printf.sprintf "WWW-Authenticate: Basic realm=\"%s\"" realm;
    "Connection: close";
    "Cache-Control: no-cache, no-store" ]    

let http_406_notacceptable =
  [ "HTTP/1.0 406 Not Acceptable";
    "Connection: close";
    "Cache-Control: no-cache, no-store" ]    

let http_500_internal_error =
  [ "HTTP/1.0 500 Internal Error";
    "Connection: close";
    "Cache-Control: no-cache, no-store" ]

let task_id_hdr = "Task-id"

let subtask_of_hdr = "Subtask-of"

let content_type_hdr = "Content-Type"

let user_agent_hdr = "User-Agent"

let myprint fmt = debug fmt

let output_http oc headers =
  let mywrite string =
    let string = string ^ "\r\n" in
    ignore(Unix.write oc string 0 (String.length string)) in
  List.iter mywrite headers

let end_of_string s from =
  String.sub s from ((String.length s)-from)

let strip_cr r =
  if String.length r=0 then raise Http_parse_failure;
  let last_char = String.sub r ((String.length r)-1) 1 in
    if last_char <> "\r" then raise Http_parse_failure;
    String.sub r 0 ((String.length r)-1)

type method_t = Get | Post | Put | Connect | Unknown of string

and authorization = 
    | Basic of string * string
    | UnknownAuth of string

and request = { m: method_t; 
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
		 headers: string list} with rpc

let string_of_method_t = function
  | Get -> "GET" | Post -> "POST" | Put -> "PUT" | Connect -> "CONNECT" | Unknown x -> "Unknown " ^ x
let method_t_of_string = function
  | "GET" -> Get | "POST" -> Post | "PUT" -> Put | "CONNECT" -> Connect | x -> Unknown x


let nullreq = { m=Unknown "";
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
		headers=[];}

let authorization_of_string x = 
  let basic = "Basic " in
  if String.startswith basic x 
  then 
    let userpass = Base64.decode (end_of_string x (String.length basic)) in
    match String.split ':' userpass with
    | [ username; password ] -> Basic(username, password)
    | _ -> UnknownAuth x
  else UnknownAuth x

exception Malformed_url of string

let print_keyvalpairs xs = 
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) xs)

let http_request ?(version="1.0") ?(keep_alive=false) ?cookie ?length ~user_agent meth host path = 
  let cookie = default [] (may (fun x -> [ "Cookie: " ^ (print_keyvalpairs x) ]) cookie) in
  let content_length = default [] (may (fun l -> [ "Content-Length: "^(Int64.to_string l)]) length) in
  [ Printf.sprintf "%s %s HTTP/%s" (string_of_method_t meth) path version;
    Printf.sprintf "Host: %s" host;
    Printf.sprintf "Connection: %s" (if keep_alive then "keep-alive" else "close");
    Printf.sprintf "%s :%s" user_agent_hdr user_agent;
  ] @ cookie @ content_length


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

let request_of_string x = 
  let parse_uri x = match String.split '?' x with
    | [ uri ] -> uri, []
    | [ uri; params ] -> uri, parse_keyvalpairs params
    | _ -> raise Http_parse_failure in
  match String.split_f String.isspace x with
  | [ m; uri; version ] ->
      (* Request-Line   = Method SP Request-URI SP HTTP-Version CRLF *)
      let uri, query = parse_uri uri in
      { m = method_t_of_string m; uri = uri; query = query; 
	content_length = None; transfer_encoding = None;
	version = version; cookie = []; auth = None; task = None; subtask_of = None; content_type = None; user_agent = None; close=false; headers=[] } 
  | _ -> raise Http_parse_failure

let pretty_string_of_request x =
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

let escape uri =
	String.escaped ~rules:[ '<', "&lt;"; '>', "&gt;"; '\'', "&apos;"; '"', "&quot;"; '&', "&amp;" ] uri

(* For transfer-encoding: chunked *)

type 'a ll = End | Item of 'a * (unit -> 'a ll)

let rec ll_iter f = function
  | End -> ()
  | Item (x, xs) -> f x; ll_iter f (xs ())
