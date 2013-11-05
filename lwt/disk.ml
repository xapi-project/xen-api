(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

open Lwt

type authentication =
  | Session_id of string
  | UserPassword of string * string

let to_uri ~use_https ~host ~authentication ~vdi =
  let scheme = if use_https then "https" else "http" in

  Uri.of_string (match authentication with
  | Session_id session_id -> Printf.sprintf "%s://%s/import_raw_vdi?session_id=%s&vdi=%s" scheme host session_id vdi
  | UserPassword (user, pass) -> Printf.sprintf "%s://%s:%s@%s/import_raw_vdi?vdi=%s" scheme user pass host vdi
  )

let socket sockaddr =
  let family = match sockaddr with
  | Lwt_unix.ADDR_INET(_, _) -> Unix.PF_INET
  | Lwt_unix.ADDR_UNIX _ -> Unix.PF_UNIX in
  Lwt_unix.socket family Unix.SOCK_STREAM 0

let start_upload ~chunked ~uri =
  let use_ssl = match Uri.scheme uri with
  | Some "https" -> true
  | Some "http" -> false
  | x -> failwith (Printf.sprintf "Unsupported URI scheme: %s" (match x with None -> "None" | Some x -> x)) in
  let port = match Uri.port uri with None -> (if use_ssl then 443 else 80) | Some port -> port in
  let host = match Uri.host uri with None -> failwith "Please supply a host in the URI" | Some host -> host in

  Lwt_unix.gethostbyname host >>= fun host_entry ->
  let sockaddr = Lwt_unix.ADDR_INET(host_entry.Lwt_unix.h_addr_list.(0), port) in
  let sock = socket sockaddr in
  Lwt_unix.connect sock sockaddr >>= fun () ->

  let open Cohttp in
  ( if use_ssl then Data_channel.of_ssl_fd sock else Data_channel.of_fd ~seekable:false sock ) >>= fun c ->

  let module Request = Request.Make(Cohttp_unbuffered_io) in
  let module Response = Response.Make(Cohttp_unbuffered_io) in
  let headers = Header.init () in
  let k, v = Cookie.Cookie_hdr.serialize [ "chunked", "true" ] in
  let headers = if chunked then Header.add headers k v else headers in
  let headers = match Uri.userinfo uri with
    | None -> headers
    | Some x ->
      begin match Re_str.bounded_split_delim (Re_str.regexp_string ":") x 2 with
      | [ user; pass ] ->
        let b = Cohttp.Auth.(to_string (Basic (user, pass))) in
        Header.add headers "authorization" b
      | _ ->
        failwith (Printf.sprintf "I don't know how to handle authentication for %s (try scheme://user:password@host/path)" (Uri.to_string uri));
      end in
  let request = Cohttp.Request.make ~meth:`PUT ~version:`HTTP_1_1 ~headers uri in
  Request.write (fun t _ -> return ()) request c >>= fun () ->
  Response.read (Cohttp_unbuffered_io.make_input c) >>= fun r ->

  begin match r with
  | None -> fail (Failure "Unable to parse HTTP response from server")
  | Some x ->
    let code = Code.code_of_status (Cohttp.Response.status x) in
    if Code.is_success code
    then return c
    else fail (Failure (Code.reason_phrase_of_code code))
  end

