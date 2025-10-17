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
(*
 * HTTP handler for serving files in the rt subdir
 *)

open Http
open Xapi_stdext_std.Xstringext

module D = Debug.Make (struct let name = "fileserver" end)

open D

let escape uri =
  String.escaped
    ~rules:
      [
        ('<', "&lt;")
      ; ('>', "&gt;")
      ; ('\'', "&apos;")
      ; ('"', "&quot;")
      ; ('&', "&amp;")
      ]
    uri

let missing uri =
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\"> <html><head> \
   <title>404 Not Found</title> </head><body> <h1>Not Found</h1> <p>The \
   requested URL "
  ^ escape uri
  ^ " was not found on this server.</p> <hr><address>Xapi \
     Server</address></body></html>"

let response_file s file_path =
  let mime_content_type = Magic_mime.lookup file_path in
  let hsts_time = !Xapi_globs.hsts_max_age in
  Http_svr.response_file ~mime_content_type ~hsts_time s file_path

let is_external_http req s =
  (not (Context.is_unix_socket s)) && Http_svr.https_client_of_req req = None

let access_forbidden req s =
  (* Reject external non-TLS requests (depending on config) *)
  !Xapi_globs.website_https_only && is_external_http req s

let send_file (uri_base : string) (dir : string) (req : Request.t)
    (s : Unix.file_descr) _ =
  let uri_base_len = String.length uri_base in
  let is_external_http = is_external_http req s in
  if is_external_http && !Xapi_globs.website_https_only then
    Http_svr.response_forbidden ~req s
  else if is_external_http && Option.is_some req.Request.host then
    (* Redirect towards HTTPS *)
    let host = Option.get req.Request.host in
    let path = req.Request.path in
    let dest = Uri.make ~scheme:"https" ~host ~path () |> Uri.to_string in
    Http_svr.response_redirect ~req s dest
  else
    let uri = req.Request.path in
    try
      let relative_url =
        String.sub uri uri_base_len (String.length uri - uri_base_len)
      in
      (* file_path is the thing which should be served *)
      let file_path = dir ^ "/" ^ relative_url in
      (* remove any dodgy use of "." or ".." NB we don't prevent the use of symlinks *)
      let file_path =
        Xapi_stdext_unix.Unixext.resolve_dot_and_dotdot file_path
      in
      if not (String.starts_with ~prefix:dir file_path) then (
        debug "Rejecting request for file: %s (outside of directory %s)"
          file_path dir ;
        Http_svr.response_forbidden ~req s
      ) else
        let stat = Unix.stat file_path in
        (* if a directory, automatically add index.html *)
        let file_path =
          if stat.Unix.st_kind = Unix.S_DIR then
            file_path ^ "/index.html"
          else
            file_path
        in
        response_file s file_path
    with _ -> Http_svr.response_missing s (missing uri)
