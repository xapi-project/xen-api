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

let uri ~pool ~authentication ~vdi =
  let ssl, scheme = match Uri.scheme pool with
    | Some "https" -> true, "https"
    | Some "http" -> false, "http"
    | x -> failwith (Printf.sprintf "Unknown scheme: %s" (match x with None -> "None" | Some x -> x)) in
  let port = match Uri.port pool with
    | Some x -> x
    | None -> if ssl then 443 else 80 in
  let query = [ "vdi", [ API.Ref.string_of vdi ] ] in
  let userinfo = match authentication with
    | UserPassword (user, pass) -> Some (user ^ ":" ^ pass)
    | Session_id _ -> None in
  let query = match authentication with
    | UserPassword (_, _) -> query
    | Session_id s -> ("session_id", [ s ]) :: query in
  Uri.make ~scheme ?userinfo ?host:(Uri.host pool) ~port ~path:"/import_raw_vdi" ~query ()

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
  Lwt.catch (fun () ->
     Lwt_unix.connect sock sockaddr
    ) (fun e ->
      Lwt_unix.close sock >>= fun () -> Lwt.fail e
    )
  >>= fun () ->

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
      begin match Re.Str.bounded_split_delim (Re.Str.regexp_string ":") x 2 with
        | [ user; pass ] ->
          let b = Cohttp.Auth.string_of_credential (`Basic (user, pass)) in
          Header.add headers "authorization" b
        | _ ->
          failwith (Printf.sprintf "I don't know how to handle authentication for %s (try scheme://user:password@host/path)" (Uri.to_string uri));
      end in
  let request = Cohttp.Request.make ~meth:`PUT ~version:`HTTP_1_1 ~headers uri in
  Request.write (fun _ -> return ()) request c >>= fun () ->
  Response.read (Cohttp_unbuffered_io.make_input c) >>= fun r ->

  begin match r with
    | `Eof | `Invalid _ -> fail (Failure "Unable to parse HTTP response from server")
    | `Ok x ->
      let code = Code.code_of_status (Cohttp.Response.status x) in
      if Code.is_success code
      then return c
      else fail (Failure (Code.reason_phrase_of_code code))
  end

