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

type authentication = Session_id of string | UserPassword of string * string

let uri ~pool ~authentication ~vdi =
  let ssl, scheme =
    match Uri.scheme pool with
    | Some "https" ->
        (true, "https")
    | Some "http" ->
        (false, "http")
    | x ->
        failwith
          (Printf.sprintf "Unknown scheme: %s"
             (match x with None -> "None" | Some x -> x)
          )
  in
  let port =
    match Uri.port pool with Some x -> x | None -> if ssl then 443 else 80
  in
  let query = [("vdi", [API.Ref.string_of vdi])] in
  let userinfo =
    match authentication with
    | UserPassword (user, pass) ->
        Some (user ^ ":" ^ pass)
    | Session_id _ ->
        None
  in
  let query =
    match authentication with
    | UserPassword (_, _) ->
        query
    | Session_id s ->
        ("session_id", [s]) :: query
  in
  Uri.make ~scheme ?userinfo ?host:(Uri.host pool) ~port ~path:"/import_raw_vdi"
    ~query ()

let socket sockaddr =
  let family =
    match sockaddr with
    | Lwt_unix.ADDR_INET (_, _) ->
        Unix.domain_of_sockaddr sockaddr
    | Lwt_unix.ADDR_UNIX _ ->
        Unix.PF_UNIX
  in
  Lwt_unix.socket family Unix.SOCK_STREAM 0

module DataChannelConstrained : sig
  type t = Data_channel.t

  type reader = Cstruct.t -> unit Lwt.t

  val really_read : t -> reader

  val really_write : t -> reader
end = struct
  type t = Data_channel.t

  type reader = Cstruct.t -> unit Lwt.t

  let really_read x = x.Data_channel.really_read

  let really_write x = x.Data_channel.really_write
end

module Cohttp_io_with_channel =
  Cohttp_unbuffered_io.Make (DataChannelConstrained)

let start_upload ~chunked ~uri =
  Uri_util.sockaddr_of_uri uri >>= fun (sockaddr, use_ssl) ->
  let sock = socket sockaddr in
  Lwt.catch
    (fun () -> Lwt_unix.connect sock sockaddr)
    (fun e -> Lwt_unix.close sock >>= fun () -> Lwt.fail e)
  >>= fun () ->
  let open Cohttp in
  ( if use_ssl then
      Data_channel.of_ssl_fd sock
    else
      Data_channel.of_fd ~seekable:false sock
  )
  >>= fun c ->
  let module Request = Request.Make (Cohttp_io_with_channel) in
  let module Response = Response.Make (Cohttp_io_with_channel) in
  let headers = Header.init () in
  let k, v = Cookie.Cookie_hdr.serialize [("chunked", "true")] in
  let headers = if chunked then Header.add headers k v else headers in
  let headers =
    match Uri.userinfo uri with
    | None ->
        headers
    | Some x -> (
      match Xapi_stdext_std.Xstringext.String.split ~limit:2 ':' x with
      | [user; pass] ->
          let b = Cohttp.Auth.string_of_credential (`Basic (user, pass)) in
          Header.add headers "authorization" b
      | _ ->
          failwith
            (Printf.sprintf
               "I don't know how to handle authentication for %s (try \
                scheme://user:password@host/path)"
               (Uri.to_string uri)
            )
    )
  in
  let request =
    Cohttp.Request.make ~meth:`PUT ~version:`HTTP_1_1 ~headers uri
  in
  Request.write (fun _ -> return ()) request c >>= fun () ->
  Response.read (Cohttp_io_with_channel.make_input c) >>= fun r ->
  match r with
  | `Eof | `Invalid _ ->
      fail (Failure "Unable to parse HTTP response from server")
  | `Ok x ->
      let code = Code.code_of_status (Cohttp.Response.status x) in
      if Code.is_success code then
        return c
      else
        fail (Failure (Code.reason_phrase_of_code code))
