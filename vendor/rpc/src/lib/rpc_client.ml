(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2006-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

let lib_version = "0.1.1"
let path = "/"

exception Connection_reset

module Utils = struct

  exception Host_not_found of string

  let open_connection_unix_fd filename =
    let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    try
      let addr = Unix.ADDR_UNIX(filename) in
      Unix.connect s addr;
      s
    with e ->
      Unix.close s;
      raise e

  let open_connection_fd host port =
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    try
      let he =
        try Unix.gethostbyname host
        with Not_found -> raise (Host_not_found host) in
      if Array.length he.Unix.h_addr_list = 0
      then failwith (Printf.sprintf "Couldn't resolve hostname: %s" host);
      let ip = he.Unix.h_addr_list.(0) in
      let addr = Unix.ADDR_INET(ip, port) in
      Unix.connect s addr;
      s
    with e ->
      Unix.close s;
      raise e

  let rec split ?(accu=[]) c s =
    try
      let i = String.index s c in
      let prefix = String.sub s 0 i in
      let suffix =
        if i = String.length s - 1 then
          ""
        else
          String.sub s (i+1) (String.length s - i - 1) in
      split ~accu:(prefix :: accu) c suffix
    with _ ->
      List.rev (s :: accu)

  let strip s =
    let is_space c = c = ' ' || c = '\n' || c = '\r' || c = '\t' in
    let n = String.length s in
    let start = ref 0 in
    let ends = ref (n - 1) in
    while !start < n && is_space s.[!start] do
      incr start;
    done;
    while  !ends > 0 && is_space s.[!ends] do
      decr ends;
    done;
    if !start = 0 && !ends = n - 1 then
      s
    else
      String.sub s !start (!ends - !start + 1)

end

type content_type = [ `XML | `JSON ]

let string_of_content_type = function
  | `XML  -> "text/xml"
  | `JSON -> "application/json"

let content_type_of_string = function
  | "text/xml"        -> `XML
  | "application/json"-> `JSON
  | s                 -> failwith (s ^ " is an invalid MIME content type")

module Headers = struct

  type t = {
    version : string;
    host : string;
    user_agent : string;
    content_type : content_type;
  }

  let create ~host ~content_type = {
    host = host;
    version = "1.1";
    user_agent = "rpc/" ^ lib_version;
    content_type = content_type;
  }

  let to_string ~path ~headers ~body =
    Printf.sprintf "POST %s HTTP/%s\r\nUser-Agent: %s\r\nHost: %s\r\nContent-Length: %d\r\nContent-Type: %s\r\n\r\n"
      path headers.version headers.user_agent headers.host (String.length body) (string_of_content_type headers.content_type)

  exception Http_401_unauthorized
  exception Http_request_rejected of string
  exception Http_headers_truncated of string
  exception Http_empty_request of string

  let assert_success s =
    match Utils.split ' ' s with
    | "HTTP/1.1" :: "200" :: _ -> ()
    | "HTTP/1.1" :: "401" :: _ -> raise Http_401_unauthorized
    | _                        -> raise (Http_request_rejected s)

  (* Consumes the headers *)
  let strip (fd: Unix.file_descr) =
    let buffer = Bytes.of_string " " in
    let buf = Buffer.create 64 in
    let finished = ref false in
    begin try
        while not !finished do
          let read = Unix.read fd buffer 0 1 in
          if read < 1 then raise (Http_headers_truncated (Buffer.contents buf));
          let n = Buffer.length buf in
          Buffer.add_char buf (Bytes.get buffer 0);
          if n >= 4
          && Buffer.nth buf (n-3) = '\r'
          && Buffer.nth buf (n-2) = '\n'
          && Buffer.nth buf (n-1) = '\r'
          && Buffer.nth buf (n) = '\n' then
            finished := true
        done;
      with Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset end;
end

let string_of_rpc_call headers call =
  match headers.Headers.content_type with
  | `XML  -> Xmlrpc.string_of_call call
  | `JSON -> Jsonrpc.string_of_call call

let rpc_response_of_fd headers fd =
  match headers.Headers.content_type with
  | `XML  -> Xmlrpc.response_of_in_channel (Unix.in_channel_of_descr fd)
  | `JSON -> Jsonrpc.response_of_in_channel (Unix.in_channel_of_descr fd)

let http_send_call ~fd ~path ~headers call =
  let body = string_of_rpc_call headers call in
  let output_string str =
    ignore (Unix.write fd (Bytes.of_string str) 0 (String.length str)) in
  output_string (Headers.to_string ~path ~headers ~body);
  output_string body

(** Read the HTTP response from the fd *)
let http_recv_response ~fd ~headers =
  Headers.strip fd;
  rpc_response_of_fd headers fd

let http_rpc_fd (fd: Unix.file_descr) headers call =
  try
    http_send_call ~fd ~path ~headers call;
    http_recv_response ~fd ~headers
  with Unix.Unix_error(Unix.ECONNRESET, _, _) ->
    raise Connection_reset

type connection =
  | Unix_socket of string
  | Remote_port of int

let with_fd ~connection ~headers ~path ~call f =
  let s =
    match connection with
    | Remote_port port ->
      let s = Utils.open_connection_fd headers.Headers.host port in
      Unix.setsockopt s Unix.TCP_NODELAY true;
      s
    | Unix_socket sock ->
      Utils.open_connection_unix_fd sock in
  try
    let result = http_rpc_fd s headers call in
    Unix.close s;
    result;
  with e ->
    Unix.close s;
    raise e

let do_rpc ~content_type ~host ~port ~path call =
  let headers = Headers.create ~content_type ~host in
  let connection = Remote_port port in
  with_fd ~connection ~headers ~path ~call rpc_response_of_fd

let do_rpc_unix ~content_type ~filename ~path call =
  let headers = Headers.create ~content_type ~host:"localhost" in
  let connection = Unix_socket filename in
  with_fd ~connection ~headers ~path ~call rpc_response_of_fd
