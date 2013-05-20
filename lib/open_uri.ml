(*
 * Copyright (c) 2012 Citrix Inc
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
 *
 *)


let with_open_uri uri f =
  let handle_socket s =
    try
      let result = f s in
      Unix.close s;
      result
    with e ->
      Unix.close s;
      raise e in
  match Uri.scheme uri with
  | Some "http" ->
    begin match Uri.host uri, Uri.port uri with
    | Some host, Some port ->
      let inet_addr = Unix.inet_addr_of_string host in
      let sockaddr = Unix.ADDR_INET(inet_addr, port) in
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.connect s sockaddr;
      handle_socket s
    | _, _ -> failwith (Printf.sprintf "Failed to parse host and port from URI: %s" (Uri.to_string uri))
    end
  | Some "file" ->
    let filename = Uri.path_and_query uri in
    let sockaddr = Unix.ADDR_UNIX filename in
    let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect s sockaddr;
    handle_socket s
  | Some x -> failwith (Printf.sprintf "Unsupported URI scheme: %s" x)
  | None -> failwith (Printf.sprintf "Failed to parse URI: %s" (Uri.to_string uri))
