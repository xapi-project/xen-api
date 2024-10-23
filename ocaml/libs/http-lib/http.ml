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

open Xapi_stdext_unix

exception Http_parse_failure

exception Unauthorised of string

exception Forbidden

exception Method_not_implemented

exception Timeout

exception Too_large

exception Client_requested_size_over_limit

module D = Debug.Make (struct let name = "http" end)

open D

let http_403_forbidden ?(version = "1.1") () =
  [
    Printf.sprintf "HTTP/%s 403 Forbidden" version
  ; "Connection: close"
  ; "Cache-Control: no-cache, no-store"
  ]

let http_200_ok ?(version = "1.1") ?(keep_alive = true) () =
  [
    Printf.sprintf "HTTP/%s 200 OK" version
  ; ("Connection: " ^ if keep_alive then "keep-alive" else "close")
  ; "Cache-Control: no-cache, no-store"
  ]

let http_200_ok_with_content length ?(version = "1.1") ?(keep_alive = true) () =
  [
    Printf.sprintf "HTTP/%s 200 OK" version
  ; ("Connection: " ^ if keep_alive then "keep-alive" else "close")
  ; "Content-Length: " ^ Int64.to_string length
  ; "Cache-Control: no-cache, no-store"
  ]

let http_404_missing ?(version = "1.1") () =
  [
    Printf.sprintf "HTTP/%s 404 Not Found" version
  ; "Connection: close"
  ; "Cache-Control: no-cache, no-store"
  ]

let http_302_redirect ?(version = "1.1") url =
  [
    Printf.sprintf "HTTP/%s 302 Found" version
  ; "Connection: close"
  ; "Cache-Control: no-cache, no-store"
  ; "Location: " ^ url
  ]

let http_400_badrequest ?(version = "1.1") () =
  [
    Printf.sprintf "HTTP/%s 400 Bad Request" version
  ; "Connection: close"
  ; "Cache-Control: no-cache, no-store"
  ]

let http_500_internal_server_error ?(version = "1.0") () =
  [
    Printf.sprintf "HTTP/%s 500 Internal Server Error" version
  ; "Connection: close"
  ; "Cache-Control: no-cache, no-store"
  ]

let http_501_method_not_implemented ?(version = "1.0") () =
  [
    Printf.sprintf "HTTP/%s 501 Method Not Implemented" version
  ; "Connection: close"
  ; "Cache-Control: no-cache, no-store"
  ]

let http_503_service_unavailable ?(version = "1.0") () =
  [
    Printf.sprintf "HTTP/%s 503 Service Unavailable" version
  ; "Connection: close"
  ; "Cache-Control: no-cache, no-store"
  ]

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

  let location = "location"

  let originator = "originator"

  let traceparent = "traceparent"

  let hsts = "strict-transport-security"
end

let output_http fd headers =
  headers
  |> List.map (fun x -> Printf.sprintf "%s\r\n" x)
  |> String.concat ""
  |> Unixext.really_write_string fd

(* Encode @param suitably for appearing in a query parameter in a URL. *)
let urlencode param = Uri.pct_encode ~component:`Query param

(** Parses strings of the form a=b;c=d (new, RFC-compliant cookie format)
    and a=b&c=d (old, incorrect style) into [("a", "b"); ("c", "d")] *)
let parse_cookies xs =
  (* Determine if ';' or '&' is used as the separator.
     Both are assumed to be illegal characters otherwise *)
  let sep =
    match Astring.String.find (fun c -> c = ';') xs with
    | Some _ ->
        ";"
    | None ->
        "&"
  in
  let kvpairs =
    List.map (Astring.String.cuts ~sep:"=") (Astring.String.cuts ~sep xs)
  in
  List.map
    (function
      | k :: vs ->
          (Uri.pct_decode k, Uri.pct_decode (String.concat "=" vs))
      | [] ->
          raise Http_parse_failure
      )
    kvpairs

type authorization = Basic of string * string | UnknownAuth of string
[@@deriving rpc]

let authorization_equal a b =
  match (a, b) with
  | Basic _, UnknownAuth _ | UnknownAuth _, Basic _ ->
      false
  | Basic (a_u, a_p), Basic (b_u, b_p) ->
      String.equal a_u b_u && String.equal a_p b_p
  | UnknownAuth a, UnknownAuth b ->
      String.equal a b

let authorization_of_string x =
  let basic = "Basic " in
  if Astring.String.is_prefix ~affix:basic x then
    match Base64.decode ~off:(String.length basic) x with
    | Result.Ok userpass -> (
      match Astring.String.cut ~sep:":" userpass with
      | Some (username, password) ->
          Basic (username, password)
      | _ ->
          UnknownAuth x
    )
    | Result.Error _ ->
        UnknownAuth x
  else
    UnknownAuth x

let string_of_authorization = function
  | UnknownAuth x ->
      x
  | Basic (username, password) ->
      "Basic " ^ Base64.encode_string (username ^ ":" ^ password)

type method_t = Get | Post | Put | Connect | Options | Unknown of string
[@@deriving rpc]

let string_of_method_t = function
  | Get ->
      "GET"
  | Post ->
      "POST"
  | Put ->
      "PUT"
  | Connect ->
      "CONNECT"
  | Options ->
      "OPTIONS"
  | Unknown x ->
      "Unknown " ^ x

let method_t_of_string = function
  | "GET" ->
      Get
  | "POST" ->
      Post
  | "PUT" ->
      Put
  | "CONNECT" ->
      Connect
  | "OPTIONS" ->
      Options
  | x ->
      Unknown x

module Scanner = struct
  type t = {marker: string; mutable i: int}

  let make x = {marker= x; i= 0}

  let input x c = if c = x.marker.[x.i] then x.i <- x.i + 1 else x.i <- 0

  let remaining x = String.length x.marker - x.i

  let matched x = x.i = String.length x.marker

  (* let to_string x = Printf.sprintf "%d" x.i *)
end

let end_of_headers = "\r\n\r\n"

let header_len_header = Printf.sprintf "\r\n%s:" Hdr.header_len

let header_len_value_len = 5

let read_up_to ?deadline ?max buf already_read marker fd =
  let marker = Scanner.make marker in
  let hl_marker = Scanner.make header_len_header in
  let b = ref 0 in
  (* next free byte in [buf] *)
  let header_len = ref None in
  let header_len_value_at = ref None in
  while not (Scanner.matched marker) do
    Option.iter
      (fun d ->
        if Mtime.Span.compare (Mtime_clock.elapsed ()) d > 0 then
          raise Timeout
      )
      deadline ;
    let safe_to_read =
      match (!header_len_value_at, !header_len) with
      | None, None ->
          Scanner.remaining marker
      | Some x, None ->
          header_len_value_len - (!b - x)
      | _, Some l ->
          l - !b
    in
    Option.iter (fun m -> if !b + safe_to_read > m then raise Too_large) max ;
    let n =
      if !b < already_read then
        min safe_to_read (already_read - !b)
      else
        Unix.read fd buf !b safe_to_read
    in
    if n = 0 then raise End_of_file ;
    for j = 0 to n - 1 do
      Scanner.input marker (Bytes.get buf (!b + j)) ;
      if !header_len_value_at = None then (
        Scanner.input hl_marker (Bytes.get buf (!b + j)) ;
        if Scanner.matched hl_marker then
          header_len_value_at := Some (!b + j + 1)
      )
    done ;
    b := !b + n ;
    match !header_len_value_at with
    | Some x when x + header_len_value_len <= !b ->
        (* We can now read the header len header *)
        let hlv = Bytes.sub_string buf x header_len_value_len in
        header_len := Some (int_of_string hlv)
    | _ ->
        ()
  done ;
  !b

let smallest_request = "GET / HTTP/1.0\r\n\r\n"

(* let smallest_response = "HTTP/1.0 200 OK\r\n\r\n" *)
let frame_header_length = String.length smallest_request

let make_frame_header headers =
  (* Frame header is the size of the smallest HTTP request
     	   and the smallest HTTP request is smaller than the smallest
     	   HTTP response. *)
  Printf.sprintf "FRAME %012d" (String.length headers)

let read_frame_header buf =
  let prefix = Bytes.sub_string buf 0 frame_header_length in
  try Scanf.sscanf prefix "FRAME %012d" (fun x -> Some x) with _ -> None

let read_http_request_header ~read_timeout ~total_timeout ~max_length fd =
  Unixext.with_socket_timeout fd read_timeout @@ fun () ->
  let buf = Bytes.create (Option.value ~default:1024 max_length) in
  let deadline =
    Option.map
      (fun t ->
        let start = Mtime_clock.elapsed () in
        let timeout_ns = int_of_float (t *. 1e9) in
        Mtime.Span.(add start (timeout_ns * ns))
      )
      total_timeout
  in
  let check_timeout_and_read x y =
    Option.iter
      (fun d ->
        if Mtime.Span.compare (Mtime_clock.elapsed ()) d > 0 then
          raise Timeout
      )
      deadline ;
    Unixext.really_read fd buf x y
  in
  check_timeout_and_read 0 6 ;
  (* return PROXY header if it exists, and then read up to FRAME header length (which also may not exist) *)
  let proxy =
    match Bytes.sub_string buf 0 6 with
    | "PROXY " ->
        let proxy_header_length = read_up_to ?deadline buf 6 "\r\n" fd in
        (* chop 'PROXY ' from the beginning, and '\r\n' from the end *)
        let proxy = Bytes.sub_string buf 6 (proxy_header_length - 6 - 2) in
        check_timeout_and_read 0 frame_header_length ;
        Some proxy
    | _ ->
        check_timeout_and_read 6 (frame_header_length - 6) ;
        None
  in
  let frame, headers_length =
    match read_frame_header buf with
    | None ->
        let max = Option.map (fun m -> m - frame_header_length) max_length in
        ( false
        , read_up_to ?deadline ?max buf frame_header_length end_of_headers fd
        )
    | Some length ->
        check_timeout_and_read 0 length ;
        (true, length)
  in
  (frame, Bytes.sub_string buf 0 headers_length, proxy)

let read_http_response_header buf fd =
  Unixext.really_read fd buf 0 frame_header_length ;
  match read_frame_header buf with
  | None ->
      read_up_to buf frame_header_length end_of_headers fd
  | Some length ->
      Unixext.really_read fd buf 0 length ;
      length

module Accept = struct
  (* Constraint: we can't have ty = None but subty <> None *)
  type t = {
      ty: (string * string option) option
    ; (* None means '*' *)
      q: int
          (* range 0 - 1000 *)
          (* We won't parse the more advanced stuff *)
  }

  let equal_ty a b =
    match (a, b) with
    | None, None ->
        true
    | Some (a, None), Some (b, None) ->
        String.equal a b
    | Some (a, Some a_s), Some (b, Some b_s) when String.equal a b ->
        String.equal a_s b_s
    | _ ->
        false

  let equal {ty= a_ty; q= a_q} {ty= b_ty; q= b_q} =
    Int.equal a_q b_q && equal_ty a_ty b_ty

  let to_string x =
    let ty, subty =
      match x.ty with
      | None ->
          ("*", "*")
      | Some (ty, subty) ->
          (ty, Option.value ~default:"*" subty)
    in
    Printf.sprintf "%s/%s;q=%.3f" ty subty (float_of_int x.q /. 1000.)

  let ( // ) = Filename.concat

  let matches ty = function
    | {ty= Some (ty', Some subty'); _} ->
        String.equal (ty' // subty') ty
    | {ty= Some (ty', None); _} -> (
      match String.split_on_char '/' ty with
      | [] ->
          false
      | ty :: _ ->
          String.equal ty' ty
    )
    | {ty= None; _} ->
        true

  let compare_user_preference (a : t) (b : t) =
    let c = Stdlib.compare a.q b.q in
    if c <> 0 then
      -c (* q factor (user-preference) overrides all else *)
    else
      match (a.ty, b.ty) with
      | Some _, None | Some (_, Some _), Some (_, None) ->
          -1
      | None, Some _ | Some (_, None), Some (_, Some _) ->
          1
      | None, None
      | Some (_, None), Some (_, None)
      | Some (_, Some _), Some (_, Some _) ->
          0

  let preferred ~from types =
    let same_priority = function
      | [] ->
          None
      | {q; _} :: _ as xs ->
          Some (List.partition (fun y -> y.q = q) xs)
    in
    let rec loop xs =
      (* Assumes elements are ordered from best to worst *)
      match same_priority xs with
      | None ->
          []
      | Some (best, others) -> (
        match List.filter (fun a -> List.exists (matches a) best) from with
        | [] ->
            loop others
        | negotiated_types ->
            negotiated_types
      )
    in
    loop (List.sort compare_user_preference types)

  exception Parse_failure of string

  let of_string_single x =
    match Astring.String.cuts ~sep:";" x with
    | ty_subty :: params ->
        let ty_of_string = function "*" -> None | x -> Some x in
        let ty =
          match Astring.String.cuts ~sep:"/" ty_subty with
          | [ty; subty] ->
              Option.map (fun ty -> (ty, ty_of_string subty)) (ty_of_string ty)
          | _ ->
              raise (Parse_failure ty_subty)
        in
        let params =
          List.map
            (fun x ->
              match Astring.String.cut ~sep:"=" x with
              | Some (k, v) ->
                  (k, v)
              | _ ->
                  raise (Parse_failure x)
            )
            params
        in
        let q =
          match List.assoc_opt "q" params with
          | Some q ->
              int_of_float (1000. *. float_of_string q)
          | None ->
              1000
        in
        {ty; q}
    | _ ->
        raise (Parse_failure x)

  let of_string x =
    List.map of_string_single (Astring.String.cuts ~empty:false ~sep:"," x)
end

module Request = struct
  type t = {
      m: method_t
    ; uri: string
    ; query: (string * string) list
    ; version: string
    ; frame: bool
    ; transfer_encoding: string option
    ; accept: string option
    ; content_length: int64 option
    ; auth: authorization option
    ; cookie: (string * string) list
    ; task: string option
    ; subtask_of: string option
    ; content_type: string option
    ; host: string option
    ; user_agent: string option
    ; mutable close: bool
    ; additional_headers: (string * string) list
    ; body: string option
    ; traceparent: string option
  }
  [@@deriving rpc]

  let empty =
    {
      m= Unknown ""
    ; uri= ""
    ; query= []
    ; version= ""
    ; frame= false
    ; transfer_encoding= None
    ; accept= None
    ; content_length= None
    ; auth= None
    ; cookie= []
    ; task= None
    ; subtask_of= None
    ; content_type= None
    ; host= None
    ; user_agent= None
    ; close= true
    ; additional_headers= []
    ; body= None
    ; traceparent= None
    }

  let make ?(frame = false) ?(version = "1.1") ?(keep_alive = true) ?accept
      ?cookie ?length ?auth ?subtask_of ?body ?(headers = []) ?content_type
      ?host ?(query = []) ?traceparent ~user_agent meth path =
    {
      empty with
      version
    ; frame
    ; close= not keep_alive
    ; cookie= Option.value ~default:[] cookie
    ; subtask_of
    ; content_length= length
    ; auth
    ; content_type
    ; host
    ; user_agent= Some user_agent
    ; m= meth
    ; uri= path
    ; additional_headers= headers
    ; body
    ; accept
    ; query
    ; traceparent
    }

  let get_version x = x.version

  let to_string x =
    let kvpairs x =
      String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x)
    in
    Printf.sprintf
      "{ frame = %b; method = %s; uri = %s; query = [ %s ]; content_length = [ \
       %s ]; transfer encoding = %s; version = %s; cookie = [ %s ]; task = %s; \
       subtask_of = %s; content-type = %s; host = %s; user_agent = %s; \
       traceparent = %s }"
      x.frame (string_of_method_t x.m) x.uri (kvpairs x.query)
      (Option.fold ~none:"" ~some:Int64.to_string x.content_length)
      (Option.value ~default:"" x.transfer_encoding)
      x.version "(value filtered)" (* cookies *)
      (Option.value ~default:"" x.task)
      (Option.value ~default:"" x.subtask_of)
      (Option.value ~default:"" x.content_type)
      (Option.value ~default:"" x.host)
      (Option.value ~default:"" x.user_agent)
      (Option.value ~default:"" x.traceparent)

  let to_header_list x =
    let kvpairs x =
      String.concat "&"
        (List.map (fun (k, v) -> urlencode k ^ "=" ^ urlencode v) x)
    in
    let query = if x.query = [] then "" else "?" ^ kvpairs x.query in
    let cookie =
      if x.cookie = [] then [] else [Hdr.cookie ^ ": " ^ kvpairs x.cookie]
    in
    let transfer_encoding =
      Option.fold ~none:[]
        ~some:(fun x -> [Hdr.transfer_encoding ^ ": " ^ x])
        x.transfer_encoding
    in
    let accept =
      Option.fold ~none:[] ~some:(fun x -> [Hdr.accept ^ ": " ^ x]) x.accept
    in
    let content_length =
      Option.fold ~none:[]
        ~some:(fun x -> [Printf.sprintf "%s: %Ld" Hdr.content_length x])
        x.content_length
    in
    let auth =
      Option.fold ~none:[]
        ~some:(fun x -> [Hdr.authorization ^ ": " ^ string_of_authorization x])
        x.auth
    in
    let task =
      Option.fold ~none:[] ~some:(fun x -> [Hdr.task_id ^ ": " ^ x]) x.task
    in
    let subtask_of =
      Option.fold ~none:[]
        ~some:(fun x -> [Hdr.subtask_of ^ ": " ^ x])
        x.subtask_of
    in
    let content_type =
      Option.fold ~none:[]
        ~some:(fun x -> [Hdr.content_type ^ ": " ^ x])
        x.content_type
    in
    let host =
      Option.fold ~none:[] ~some:(fun x -> [Hdr.host ^ ": " ^ x]) x.host
    in
    let user_agent =
      Option.fold ~none:[]
        ~some:(fun x -> [Hdr.user_agent ^ ": " ^ x])
        x.user_agent
    in
    let traceparent =
      Option.fold ~none:[]
        ~some:(fun x -> [Hdr.traceparent ^ ": " ^ x])
        x.traceparent
    in
    let close =
      [(Hdr.connection ^ ": " ^ if x.close then "close" else "keep-alive")]
    in
    [
      Printf.sprintf "%s %s%s HTTP/%s" (string_of_method_t x.m) x.uri query
        x.version
    ]
    @ cookie
    @ transfer_encoding
    @ accept
    @ content_length
    @ auth
    @ task
    @ subtask_of
    @ content_type
    @ host
    @ user_agent
    @ traceparent
    @ close
    @ List.map (fun (k, v) -> k ^ ": " ^ v) x.additional_headers

  let to_headers_and_body (x : t) =
    (* If the body is given then compute a content length *)
    let x =
      match x.body with
      | None ->
          x
      | Some b ->
          {x with content_length= Some (Int64.of_int (String.length b))}
    in
    let hl = to_header_list x @ [""] in
    let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") hl) in
    let body = Option.value ~default:"" x.body in
    (headers, body)

  let to_wire_string (x : t) =
    let headers, body = to_headers_and_body x in
    let frame_header = if x.frame then make_frame_header headers else "" in
    frame_header ^ headers ^ body

  let with_originator_of req f =
    Option.iter
      (fun req ->
        let originator = List.assoc_opt Hdr.originator req.additional_headers in
        f originator
      )
      req

  let traceparent_of req =
    let open Tracing in
    let ( let* ) = Option.bind in
    let* traceparent = req.traceparent in
    let* span_context = SpanContext.of_traceparent traceparent in
    let span = Tracer.span_of_span_context span_context req.uri in
    Some span

  let with_tracing ?attributes ~name req f =
    let open Tracing in
    let parent = traceparent_of req in
    with_child_trace ?attributes parent ~name (fun (span : Span.t option) ->
        match span with
        | Some span ->
            let traceparent =
              Some (span |> Span.get_context |> SpanContext.to_traceparent)
            in
            let req = {req with traceparent} in
            f req
        | None ->
            f req
    )
end

module Response = struct
  type t = {
      version: string
    ; frame: bool
    ; code: string
    ; message: string
    ; content_length: int64 option
    ; task: string option
    ; additional_headers: (string * string) list
    ; body: string option
  }

  let _empty =
    {
      version= "1.1"
    ; frame= false
    ; code= "500"
    ; message= "Empty response"
    ; content_length= Some 0L
    ; task= None
    ; additional_headers= []
    ; body= None
    }

  let to_string x =
    let kvpairs x =
      String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x)
    in
    Printf.sprintf
      "{ frame = %b; version = %s; code = %s; message = %s; content_length = \
       %s; task = %s; additional_headers = [ %s ] }"
      x.frame x.version x.code x.message
      (Option.fold ~none:"None"
         ~some:(fun x -> "Some " ^ Int64.to_string x)
         x.content_length
      )
      (Option.fold ~none:"None" ~some:(fun x -> "Some " ^ x) x.task)
      (kvpairs x.additional_headers)

  let empty =
    {
      version= "1.1"
    ; frame= false
    ; code= "500"
    ; message= "Unknown error message"
    ; content_length= None
    ; task= None
    ; additional_headers= []
    ; body= None
    }

  let make ?(frame = false) ?(version = "1.1") ?length ?task ?(headers = [])
      ?body code message =
    {
      version
    ; frame
    ; code
    ; message
    ; content_length= length
    ; task
    ; additional_headers= headers
    ; body
    }

  let internal_error =
    {empty with code= "500"; message= "internal error"; content_length= Some 0L}

  let to_header_list (x : t) =
    let status = Printf.sprintf "HTTP/%s %s %s" x.version x.code x.message in
    let content_length =
      Option.fold ~none:[]
        ~some:(fun x -> [Printf.sprintf "%s: %Ld" Hdr.content_length x])
        x.content_length
    in
    let task =
      Option.fold ~none:[] ~some:(fun x -> [Hdr.task_id ^ ": " ^ x]) x.task
    in
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) x.additional_headers in
    status :: (content_length @ task @ headers)

  let to_headers_and_body (x : t) =
    (* If the body is given then compute a content length *)
    let x =
      match x.body with
      | None ->
          x
      | Some b ->
          {x with content_length= Some (Int64.of_int (String.length b))}
    in
    let hl = to_header_list x @ [""] in
    let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") hl) in
    let body = Option.value ~default:"" x.body in
    (headers, body)

  let to_wire_string (x : t) =
    let headers, body = to_headers_and_body x in
    let frame_header = if x.frame then make_frame_header headers else "" in
    frame_header ^ headers ^ body
end

(* For transfer-encoding: chunked *)

type 'a ll = End | Item of 'a * (unit -> 'a ll)

let rec ll_iter f = function
  | End ->
      ()
  | Item (x, xs) ->
      f x ;
      ll_iter f (xs ())

module Url = struct
  type http = {
      host: string
    ; auth: authorization option
    ; port: int option
    ; ssl: bool
  }

  type file = {path: string}

  type scheme = Http of http | File of file

  type data = {uri: string; query_params: (string * string) list}

  type t = scheme * data

  let file_equal a b = String.equal a.path b.path

  let query_params_equal (ak, av) (bk, bv) =
    String.equal ak bk && String.equal av bv

  let data_equal a b =
    String.equal a.uri b.uri
    && List.equal query_params_equal a.query_params b.query_params

  let http_equal a b =
    a.ssl = b.ssl
    && Option.equal Int.equal a.port b.port
    && Option.equal authorization_equal a.auth b.auth
    && String.equal a.host b.host

  let scheme_equal a b =
    match (a, b) with
    | Http _, File _ | File _, Http _ ->
        false
    | Http a_h, Http b_h ->
        http_equal a_h b_h
    | File a_f, File b_f ->
        file_equal a_f b_f

  let equal (a_scheme, a_data) (b_scheme, b_data) =
    scheme_equal a_scheme b_scheme && data_equal a_data b_data

  let of_string url =
    debug "%s: %s" __FUNCTION__ url ;
    let fail fmt = Printf.ksprintf failwith fmt in
    let query = function k, v :: _ -> (k, v) | k, [] -> (k, "") in
    try
      let uri = Uri.of_string url in
      debug "%s: %s" __FUNCTION__ (Uri.to_string uri) ;
      let auth =
        match (Uri.user uri, Uri.password uri) with
        | Some user, Some pw ->
            Some (Basic (user, pw))
        | Some user, None ->
            Some (Basic (user, ""))
        | _ ->
            None
      in
      let data =
        {
          uri= (match Uri.path_unencoded uri with "" -> "/" | path -> path)
        ; query_params= Uri.query uri |> List.map query
        }
      in
      let scheme ~ssl =
        Http {host= Uri.host uri |> Option.get; auth; port= Uri.port uri; ssl}
      in
      match Uri.scheme uri with
      | Some "http" ->
          (scheme ~ssl:false, data)
      | Some "https" ->
          (scheme ~ssl:true, data)
      | Some "file" ->
          let scheme = File {path= Uri.path_unencoded uri} in
          (scheme, {data with uri= "/"})
      | _ ->
          failwith "unsupported URI scheme"
    with e ->
      fail "%s: can't parse '%s': %s" __FUNCTION__ url (Printexc.to_string e)

  let data_to_string {uri; query_params= params} =
    let kvpairs x =
      String.concat "&"
        (List.map (fun (k, v) -> urlencode k ^ "=" ^ urlencode v) x)
    in
    let params = if params = [] then "" else "?" ^ kvpairs params in
    uri ^ params

  let to_string scheme =
    let query (k, v) = (k, [v]) in
    let str =
      match scheme with
      | File {path}, {query_params= params; _} ->
          Uri.make ~scheme:"file" ~path ~query:(List.map query params) ()
          |> Uri.to_string
      | Http h, {uri; query_params= params} ->
          let auth =
            match h.auth with
            | Some (Basic (username, password)) ->
                Printf.sprintf "%s:%s" username password
                |> Uri.pct_encode
                |> Option.some
            | _ ->
                Option.none
          in
          Uri.make
            ~scheme:(if h.ssl then "https" else "http")
            ~host:h.host ?port:h.port ?userinfo:auth ~path:uri
            ~query:(List.map query params) ()
          |> Uri.to_string
    in
    debug "%s: %s" __FUNCTION__ str ;
    str

  let get_uri (_scheme, data) = data.uri

  let set_uri (scheme, data) u = (scheme, {data with uri= u})

  let get_query_params (_scheme, data) = data.query_params

  let get_query (_scheme, data) = data_to_string data

  let auth_of (scheme, _) =
    match scheme with File _ -> None | Http {auth; _} -> auth

  let set_ssl ssl = function Http h, d -> (Http {h with ssl}, d) | x -> x
end
