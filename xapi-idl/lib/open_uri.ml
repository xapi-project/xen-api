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

open Xapi_stdext_pervasives.Pervasiveext

module D = Debug.Make (struct let name = "open_uri" end)
open D

let handle_socket f s = try f s with e -> Backtrace.is_important e ; raise e

let open_tcp f host port =
  let host = Scanf.ksscanf host (fun _ _ -> host) "[%s@]" Fun.id in
  let sockaddr =
    match Unix.getaddrinfo host (string_of_int port) [] with
    | [] ->
      error "No addrinfo found for host: %s on port: %d" host port ;
      raise Not_found
    | addrinfo::_ -> addrinfo.Unix.ai_addr
  in
  let family = Unix.domain_of_sockaddr sockaddr in
  let s = Unix.socket family Unix.SOCK_STREAM 0 in
  finally
    (fun () -> Unix.connect s sockaddr ; handle_socket f s)
    (fun () -> Unix.close s)

let with_open_uri uri f =
  match Uri.scheme uri with
  | Some "http" -> (
    match (Uri.host uri, Uri.port uri) with
    | Some host, Some port ->
        open_tcp f host port
    | Some host, None ->
        open_tcp f host 80
    | _, _ ->
        failwith
          (Printf.sprintf "Failed to parse host and port from URI: %s"
             (Uri.to_string uri))
  )
  | Some "file" ->
      let filename = Uri.path_and_query uri in
      let sockaddr = Unix.ADDR_UNIX filename in
      let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      finally
        (fun () -> Unix.connect s sockaddr ; handle_socket f s)
        (fun () -> Unix.close s)
  | Some x ->
      failwith (Printf.sprintf "Unsupported URI scheme: %s" x)
  | None ->
      failwith (Printf.sprintf "Failed to parse URI: %s" (Uri.to_string uri))
