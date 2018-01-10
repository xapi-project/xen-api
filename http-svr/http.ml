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

open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_unix

exception Http_parse_failure
exception Unauthorised of string
exception Forbidden
exception Method_not_implemented
exception Malformed_url of string


module D = Debug.Make(struct let name = "http" end)
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

let http_500_internal_server_error ?(version="1.0") () =
  [ Printf.sprintf "HTTP/%s 500 Internal Server Error" version;
    "Connection: close";
    "Cache-Control: no-cache, no-store" ]

let http_501_method_not_implemented ?(version="1.0") () =
  [ Printf.sprintf "HTTP/%s 501 Method Not Implemented" version;
    "Connection: close";
    "Cache-Control: no-cache, no-store" ]

module Hdr = struct
  let task_id = "task-id"
  let subtask_of = "subtask-of"
  let content_type = "content-type"
  let content_length = "content-length"
  let host = "host"
  let user_agent = "user-agent"
  let cookie = "cookie"
  let transfer_encoding = "transfer-encoding"
  let authorization = "authorization"
  let connection = "connection"
  let header_len = "hdr"
  let acrh = "access-control-request-headers"
  let cache_control = "cache-control"
  let content_disposition = "content-disposition"
  let accept = "accept"
end

let output_http fd headers =
  Unixext.really_write_string fd
    (String.concat "" (List.map (fun x -> x ^ "\r\n") headers))

let explode str = Astring.String.fold_right (fun c acc -> c :: acc) str []
let implode chr_list = String.concat "" (List.map Astring.String.of_char chr_list)

let urldecode url =
  let chars = explode url in
  let rec fn ac = function
    |'+'::tl -> fn (' ' :: ac) tl
    |'%'::a::b::tl ->
      let cs = try int_of_string (implode ['0';'x';a;b])
        with _ -> raise (Malformed_url url) in
      fn (Char.chr cs :: ac) tl
    |x::tl -> fn (x :: ac) tl
    |[] ->
      implode (List.rev ac)
  in fn [] chars

(* Encode @param suitably for appearing in a query parameter in a URL. *)
let urlencode param =
  let chars = explode param in
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
              Astring.String.of_char x
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
  let kvpairs = List.map (Astring.String.cuts ~sep:"=") (Astring.String.cuts ~sep:"&" xs) in
  List.map (function
      | k :: vs -> ((urldecode k), urldecode (String.concat "=" vs))
      | [] -> raise Http_parse_failure) kvpairs

let parse_uri x = match Astring.String.cuts ~sep:"?" x with
  | [ uri ] -> uri, []
  | [ uri; params ] -> uri, parse_keyvalpairs params
  | _ -> raise Http_parse_failure


type authorization =
  | Basic of string * string
  | UnknownAuth of string
[@@deriving rpc]

let authorization_of_string x =
  let basic = "Basic " in
  if Astring.String.is_prefix ~affix:basic x
  then
    let end_of_string s from =
      String.sub s from ((String.length s)-from) in
    let userpass = Xapi_stdext_base64.Base64.decode (end_of_string x (String.length basic)) in
    match Astring.String.cuts ~sep:":" userpass with
    | [ username; password ] -> Basic(username, password)
    | _ -> UnknownAuth x
  else UnknownAuth x

let string_of_authorization = function
  | UnknownAuth x -> x
  | Basic(username, password) -> "Basic " ^ (Xapi_stdext_base64.Base64.encode (username ^ ":" ^ password))

type method_t = Get | Post | Put | Connect | Options | Unknown of string [@@deriving rpc]

let string_of_method_t = function
  | Get -> "GET" | Post -> "POST" | Put -> "PUT" | Connect -> "CONNECT" | Options -> "OPTIONS" | Unknown x -> "Unknown " ^ x
let method_t_of_string = function
  | "GET" -> Get | "POST" -> Post | "PUT" -> Put | "CONNECT" -> Connect | "OPTIONS" -> Options | x -> Unknown x

module Scanner = struct
  type t = {
    marker: string;
    mutable i: int;
  }
  let make x = { marker = x; i = 0 }
  let input x c =
    if c = x.marker.[x.i] then x.i <- x.i + 1 else x.i <- 0
  let remaining x = String.length x.marker - x.i
  let matched x = x.i = String.length x.marker
  (* let to_string x = Printf.sprintf "%d" x.i *)
end

let end_of_headers = "\r\n\r\n"

let header_len_header = Printf.sprintf "\r\n%s:" Hdr.header_len

let header_len_value_len = 5

let read_up_to buf already_read marker fd =
  let marker = Scanner.make marker in
  let hl_marker = Scanner.make header_len_header in
  let b = ref 0 in (* next free byte in [buf] *)

  let header_len = ref None in
  let header_len_value_at = ref None in
  while not(Scanner.matched marker) do
    let safe_to_read = match !header_len_value_at, !header_len with
      | None, None -> Scanner.remaining marker
      | Some x, None -> header_len_value_len - (!b - x)
      | _, Some l -> l - !b in
(*
		Printf.fprintf stderr "b = %d; safe_to_read = %d\n" !b safe_to_read; 
		flush stderr;
*)
    let n =
      if !b < already_read
      then min safe_to_read (already_read - !b)
      else Unix.read fd buf !b safe_to_read in
    if n = 0 then raise End_of_file;
(*
		Printf.fprintf stderr "  n = %d\n" n;
		flush stderr;
*)
    for j = 0 to n - 1 do
(*
			Printf.fprintf stderr "b = %d; marker = %s; n = %d; j = %d\n" !b (Scanner.to_string marker) n j;
			flush stderr;
*)
      Scanner.input marker buf.[!b + j];
      if !header_len_value_at = None then begin
        Scanner.input hl_marker buf.[!b + j];
        if Scanner.matched hl_marker then begin
          header_len_value_at := Some(!b + j + 1);
(*
					Printf.fprintf stderr "header_len_value_at = %d\n" (!b + j + 1);
					flush stderr
*)
        end
      end
    done;
    b := !b + n;
(*
		Printf.fprintf stderr "b = %d\n" !b;
		flush stderr;
*)
    match !header_len_value_at with
    | Some x when x + header_len_value_len <= !b ->
      (* We can now read the header len header *)
      let hlv = String.sub buf x header_len_value_len in
(*
				Printf.fprintf stderr "hlvn=[%s]" hlv;
				flush stderr;
*)
      header_len := Some (int_of_string hlv);
    | _ -> ()
  done;
  !b

let read_http_header buf fd = read_up_to buf 0 end_of_headers fd

let smallest_request  = "GET / HTTP/1.0\r\n\r\n"
(* let smallest_response = "HTTP/1.0 200 OK\r\n\r\n" *)
let frame_header_length = String.length smallest_request

let make_frame_header headers =
  (* Frame header is the size of the smallest HTTP request
     	   and the smallest HTTP request is smaller than the smallest 
     	   HTTP response. *)
  Printf.sprintf "FRAME %012d" (String.length headers)

let read_frame_header buf =
  let prefix = String.sub buf 0 frame_header_length in
  try
    Scanf.sscanf prefix "FRAME %012d" (fun x -> Some x)
  with _ -> None

let read_http_request_header buf fd =
  Unixext.really_read fd buf 0 frame_header_length;
  match read_frame_header buf with
  | None -> read_up_to buf frame_header_length end_of_headers fd, false
  | Some length -> Unixext.really_read fd buf 0 length; length, true

let read_http_response_header buf fd =
  Unixext.really_read fd buf 0 frame_header_length;
  match read_frame_header buf with
  | None -> read_up_to buf frame_header_length end_of_headers fd
  | Some length -> Unixext.really_read fd buf 0 length; length

module Accept = struct
  (* Constraint: we can't have ty = None but subty <> None *)
  type t = {
    ty: string option; (* None means '*' *)
    subty: string option; (* None means '*' *)
    q: int; (* range 0 - 1000 *)
    (* We won't parse the more advanced stuff *)
  }

  let string_of_t x =
    let open Xapi_stdext_monadic in 
    Printf.sprintf "%s/%s;q=%.3f" (Opt.default "*" x.ty) (Opt.default "*" x.subty) (float_of_int x.q /. 1000.)

  let matches (ty, subty) = function
    | { ty = Some ty'; subty = Some subty'; _ } -> ty' = ty && (subty' = subty)
    | { ty = Some ty'; subty = None; _ } -> ty' = ty
    | { ty = None;     subty = Some _; _ } -> assert false
    | { ty = None;     subty = None; _ } -> true

  (* compare [a] and [b] where both match some media type *)
  let compare (a: t) (b: t) =
    let c = compare a.q b.q in
    if c <> 0
    then -c (* q factor (user-preference) overrides all else *)
    else match a.ty, b.ty with
      | Some _, None -> 1
      | None, Some _ -> -1
      | _, _ ->
        begin match a.subty, b.subty with
          | Some _, None -> 1
          | None, Some _ -> -1
          | _, _ -> 0
        end

  let preferred_match media ts =
    match List.filter (matches media) ts with
    | [] -> None
    | xs -> Some (List.hd (List.sort compare xs))

  exception Parse_failure of string
  let t_of_string x = match Astring.String.cuts ~sep:";" x with
    | ty_subty :: params ->
      let ty_of_string = function
        | "*" -> None
        | x -> Some x in
      let ty, subty = match Astring.String.cuts ~sep:"/" ty_subty with
        | [ ty; subty ] -> ty_of_string ty, ty_of_string subty
        | _ -> raise (Parse_failure ty_subty) in
      if ty = None && (subty <> None) then raise (Parse_failure x);

      let params = List.map (fun x -> match Astring.String.cut ~sep:"=" x with
          | Some (k, v) -> k, v
          | _ -> raise (Parse_failure x)
        ) params in
      let q = if List.mem_assoc "q" params then int_of_float (1000. *. (float_of_string (List.assoc "q" params))) else 1000 in
      { ty = ty; subty = subty; q = q }
    | _ -> raise (Parse_failure x)

  let ts_of_string x = List.map t_of_string (Astring.String.cuts ~sep:"," x)
end

module Request = struct
  type t = {
    m: method_t;
    uri: string;
    query: (string*string) list;
    version: string;
    frame: bool;
    transfer_encoding: string option;
    accept: string option;
    content_length: int64 option;
    auth: authorization option;
    cookie: (string * string) list;
    task: string option;
    subtask_of: string option;
    content_type: string option;
    host: string option;
    user_agent: string option;
    mutable close: bool;
    additional_headers: (string*string) list;
    body: string option;
  } [@@deriving rpc]

  let empty = {
    m=Unknown "";
    uri="";
    query=[];
    version="";
    frame=false;
    transfer_encoding=None;
    accept=None;
    content_length=None;
    auth=None;
    cookie=[];
    task=None;
    subtask_of=None;
    content_type = None;
    host = None;
    user_agent = None;
    close= true;
    additional_headers=[];
    body = None;
  }

  let make ?(frame=false) ?(version="1.1") ?(keep_alive=true) ?accept ?cookie ?length ?auth ?subtask_of ?body ?(headers=[]) ?content_type ?host ?(query=[]) ~user_agent meth path =
    { empty with
      version = version;
      frame = frame;
      close = not keep_alive;
      cookie = Xapi_stdext_monadic.Opt.default [] cookie;
      subtask_of = subtask_of;
      content_length = length;
      auth = auth;
      content_type = content_type;
      host = host;
      user_agent = Some user_agent;
      m = meth;
      uri = path;
      additional_headers = headers;
      body = body;
      accept = accept;
      query = query;
    }

  let get_version x = x.version

  let of_request_line x = match Astring.String.fields ~empty:false x with
    | [ m; uri; version ] ->
      (* Request-Line   = Method SP Request-URI SP HTTP-Version CRLF *)
      let uri, query = parse_uri uri in
      (* strip the "HTTP/" prefix from the version string *)
      begin match Astring.String.cut ~sep:"/" version with
        | Some (_, version) ->
          { m = method_t_of_string m; frame = false; uri = uri; query = query;
            content_length = None; transfer_encoding = None; accept = None;
            version = version; cookie = []; auth = None; task = None;
            subtask_of = None; content_type = None; host = None; user_agent = None;
            close=false; additional_headers=[]; body = None }
        | None ->
          error "Failed to parse: %s" x;
          raise Http_parse_failure
      end
    | _ -> raise Http_parse_failure

  let to_string x =
    let kvpairs x = String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x) in
    Printf.sprintf "{ frame = %b; method = %s; uri = %s; query = [ %s ]; content_length = [ %s ]; transfer encoding = %s; version = %s; cookie = [ %s ]; task = %s; subtask_of = %s; content-type = %s; host = %s; user_agent = %s }" 
      x.frame (string_of_method_t x.m) x.uri
      (kvpairs x.query)
      (default "" (may Int64.to_string x.content_length))
      (default "" x.transfer_encoding)
      x.version
      (kvpairs x.cookie)
      (default "" x.task)
      (default "" x.subtask_of)
      (default "" x.content_type)
      (default "" x.host)
      (default "" x.user_agent)

  let to_header_list x =
    let open Xapi_stdext_monadic in
    let kvpairs x = String.concat "&" (List.map (fun (k, v) -> urlencode k ^ "=" ^ (urlencode v)) x) in
    let query = if x.query = [] then "" else "?" ^ (kvpairs x.query) in
    let cookie = if x.cookie = [] then [] else [ Hdr.cookie ^": " ^ (kvpairs x.cookie) ] in
    let transfer_encoding = Opt.default [] (Opt.map (fun x -> [ Hdr.transfer_encoding ^": " ^ x ]) x.transfer_encoding) in
    let accept = Opt.default [] (Opt.map (fun x -> [ Hdr.accept ^ ": " ^ x]) x.accept) in
    let content_length = Opt.default [] (Opt.map (fun x -> [ Printf.sprintf "%s: %Ld" Hdr.content_length x ]) x.content_length) in
    let auth = Opt.default [] (Opt.map (fun x -> [ Hdr.authorization ^": " ^ (string_of_authorization x) ]) x.auth) in
    let task = Opt.default [] (Opt.map (fun x -> [ Hdr.task_id ^ ": " ^ x ]) x.task) in
    let subtask_of = Opt.default [] (Opt.map (fun x -> [ Hdr.subtask_of ^ ": " ^ x ]) x.subtask_of) in
    let content_type = Opt.default [] (Opt.map (fun x -> [ Hdr.content_type ^": " ^ x ]) x.content_type) in
    let host = Opt.default [] (Opt.map (fun x -> [ Hdr.host^": " ^ x ]) x.host) in
    let user_agent = Opt.default [] (Opt.map (fun x -> [ Hdr.user_agent^": " ^ x ]) x.user_agent) in
    let close = [ Hdr.connection ^": " ^ (if x.close then "close" else "keep-alive") ] in
    [ Printf.sprintf "%s %s%s HTTP/%s" (string_of_method_t x.m) x.uri query x.version ]
    @ cookie @ transfer_encoding @ accept @ content_length @ auth @ task @ subtask_of @ content_type @ host @ user_agent @ close
    @ (List.map (fun (k, v) -> k ^ ":" ^ v) x.additional_headers)

  let to_headers_and_body (x: t) =
    (* If the body is given then compute a content length *)
    let x = match x.body with
      | None -> x
      | Some b -> { x with content_length = Some (Int64.of_int (String.length b)) } in
    let hl = to_header_list x @ [""] in
    let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") hl) in
    let body = Xapi_stdext_monadic.Opt.default "" x.body in
    headers, body

  let to_wire_string (x: t) =
    let headers, body = to_headers_and_body x in
    let frame_header = if x.frame then make_frame_header headers else "" in
    frame_header ^ headers ^ body

end

module Response = struct
  type t = {
    version: string;
    frame: bool;
    code: string;
    message: string;
    content_length: int64 option;
    task: string option;
    additional_headers: (string*string) list;
    body: string option;
  }

  let _empty = {
    version = "1.1";
    frame = false;
    code = "500";
    message = "Empty response";
    content_length = Some 0L;
    task = None;
    additional_headers = [];
    body = None
  }

  let to_string x =
    let open Xapi_stdext_monadic in
    let kvpairs x = String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x) in
    Printf.sprintf "{ frame = %b; version = %s; code = %s; message = %s; content_length = %s; task = %s; additional_headers = [ %s ] }"
      x.frame x.version x.code x.message
      (Opt.default "None" (Opt.map (fun x -> "Some " ^ (Int64.to_string x)) x.content_length))
      (Opt.default "None" (Opt.map (fun x -> "Some " ^ x) x.task))
      (kvpairs x.additional_headers)

  let empty = {
    version = "1.1";
    frame = false;
    code = "500";
    message = "Unknown error message";
    content_length = None;
    task = None;
    additional_headers = [];
    body = None;
  }
  let make ?(frame=false) ?(version="1.1") ?length ?task ?(headers=[]) ?body code message = {
    version = version;
    frame = frame;
    code = code;
    message = message;
    content_length = length;
    task = task;
    additional_headers = headers;
    body = body
  }

  let internal_error = { empty with code = "500"; message = "internal error"; content_length = Some 0L }
  let to_header_list (x: t) =
    let open Xapi_stdext_monadic in
    let status = Printf.sprintf "HTTP/%s %s %s" x.version x.code x.message in
    let content_length = Opt.default [] (Opt.map (fun x -> [ Printf.sprintf "%s: %Ld" Hdr.content_length x ]) x.content_length) in
    let task = Opt.default [] (Opt.map (fun x -> [ Hdr.task_id ^ ": " ^ x ]) x.task) in
    let headers = List.map (fun (k, v) -> k ^ ":" ^ v) x.additional_headers in
    status :: (content_length @ task @ headers)

  let to_headers_and_body (x: t) =
    (* If the body is given then compute a content length *)
    let x = match x.body with
      | None -> x
      | Some b -> { x with content_length = Some (Int64.of_int (String.length b)) } in
    let hl = to_header_list x @ [""] in
    let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") hl) in
    let body = Xapi_stdext_monadic.Opt.default "" x.body in
    headers, body

  let to_wire_string (x: t) =
    let headers, body = to_headers_and_body x in
    let frame_header = if x.frame then make_frame_header headers else "" in
    frame_header ^ headers ^ body
end

(* For transfer-encoding: chunked *)

type 'a ll = End | Item of 'a * (unit -> 'a ll)

let rec ll_iter f = function
  | End -> ()
  | Item (x, xs) -> f x; ll_iter f (xs ())


module Url = struct
  type http = {
    host: string;
    auth: authorization option;
    port: int option;
    ssl: bool;
  }
  type file = {
    path: string;
  }
  type scheme =
    | Http of http
    | File of file
  type data = {
    uri: string;
    query_params: (string * string) list;
  }

  type t = scheme * data

  let of_string url =
    let sub_before c s = 
      String.sub s 0 (String.index s c)
    in
    let sub_after c s =
      let length = String.length s in
      let start = (String.index s c) + 1 in
      String.sub s start (length - start)
    in
    let host x =
      try x |> sub_after '[' |> sub_before ']' with Not_found ->   (* [<ipv6-literal>]... *)
      try x |> sub_before ':' with Not_found ->                    (* <hostname|ipv4-literal>:... *)
        x in                                                         (* <hostname|ipv4-literal> *)
    let port x =
      let port_part =
        try x |> sub_after ']' |> sub_after ':' with Not_found ->  (* ...]:port *)
        try x |> sub_after ']' with Not_found ->                   (* ...] *)
        try x |> sub_after ':' with Not_found ->                   (* ...:port *)
          "" in                                                      (* no port *)
      try Some (int_of_string port_part) with _ -> None in
    let uname_password_host_port x = match Astring.String.cuts ~sep:"@" x with
      | [ _ ] -> None, host x, port x
      | [ uname_password; host_port ] ->
        begin match Astring.String.cuts ~sep:":" uname_password with 
          | [ uname; password ] -> Some (Basic (uname, password)), host host_port, port host_port
          | _ -> failwith (Printf.sprintf "Failed to parse authentication substring: %s" uname_password)
        end
      | _ -> failwith (Printf.sprintf "Failed to parse username password host and port: %s" x) in
    let reconstruct_uri uri = "/" ^ (String.concat "/" uri) in
    let data_of_uri uri =
      let uri, params = parse_uri (reconstruct_uri uri) in
      { uri = uri; query_params = params } in
    let http_or_https ssl x =
      let uname_password, host, port = uname_password_host_port x in
      let scheme = Http { host = host; port = port; auth = uname_password; ssl = ssl } in
      scheme in
    match Astring.String.cuts ~sep:"/" url with
    | "http:" :: "" :: x :: uri -> http_or_https false x, data_of_uri uri
    | "https:" :: "" :: x :: uri -> http_or_https true x, data_of_uri uri
    | "file:" :: uri ->
      let uri, params = parse_uri (reconstruct_uri uri) in
      File { path = uri }, { uri = "/"; query_params = params }
    | x :: _ -> failwith (Printf.sprintf "Unknown scheme %s" x)
    | _ -> failwith (Printf.sprintf "Failed to parse URL: %s" url)

  let data_to_string { uri = uri; query_params = params } =
    let kvpairs x = String.concat "&" (List.map (fun (k, v) -> urlencode k ^ "=" ^ (urlencode v)) x) in
    let params = if params = [] then "" else "?" ^ (kvpairs params) in
    uri ^ params

  (* Wrap a literal IPv6 address in square brackets; otherwise pass through *)
  let maybe_wrap_IPv6_literal addr = 
    if Unixext.domain_of_addr addr = Some Unix.PF_INET6 then "[" ^ addr ^ "]" else addr

  let to_string = function
    | File { path = path }, data -> Printf.sprintf "file:%s%s" path (data_to_string data) (* XXX *)
    | Http h, data ->
      let userpassat = match h.auth with
        | Some (Basic (username, password)) -> Printf.sprintf "%s:%s@" username password
        | _ -> "" in
      let colonport = match h.port with
        | Some x -> Printf.sprintf ":%d" x
        | _ -> "" in
      Printf.sprintf "http%s://%s%s%s%s" (if h.ssl then "s" else "")
        userpassat (maybe_wrap_IPv6_literal h.host) colonport (data_to_string data)

  let get_uri (_scheme, data) = data.uri
  let set_uri (scheme, data) u = (scheme, { data with uri = u })

  let get_query_params (_scheme, data) = data.query_params

  let get_query (_scheme, data) = data_to_string data

  let auth_of (scheme, _) = match scheme with
    | File _ -> None
    | Http { auth = auth; _ } -> auth
end
