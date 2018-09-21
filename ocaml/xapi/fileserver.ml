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

open Printf
open Http
open Stdext.Xstringext
open Stdext.Pervasiveext

module D = Debug.Make(struct let name="xapi" end)
open D

let escape uri =
  String.escaped ~rules:[ '<', "&lt;"; '>', "&gt;"; '\'', "&apos;"; '"', "&quot;"; '&', "&amp;" ] uri

let missing uri = "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\"> \
                   <html><head> \
                   <title>404 Not Found</title> \
                   </head><body> \
                   <h1>Not Found</h1> \
                   <p>The requested URL " ^ (escape uri) ^ " was not found on this server.</p> \
                                                            <hr>\
                                                            <address>Xapi Server</address>\
                                                            </body></html>"

let get_extension filename =
  try
    let basename = Filename.basename filename in
    let i = String.rindex basename '.' in
    Some (String.sub basename (i + 1) (String.length basename - i - 1))
  with _ ->
    None

let application_octet_stream = "application/octet-stream"

let mime_of_extension = function
  | "html" | "htm" -> "text/html"
  | "css"          -> "text/css"
  | "js"           -> "application/javascript"
  | "gif"          -> "image/gif"
  | "png"          -> "image/png"
  | "jpg" | "jpeg" -> "image/jpeg"
  | "xml"          -> "application/xml"
  | "rpm"          -> "application/x-rpm"
  | _              -> application_octet_stream

let response_file s file_path =
  let mime_content_type =
    let open Stdext.Opt in
    let ext = map String.lowercase_ascii (get_extension file_path) in
    default application_octet_stream (map mime_of_extension ext) in
  Http_svr.response_file ~mime_content_type s file_path

let send_file (uri_base: string) (dir: string) (req: Request.t) (bio: Buf_io.t) _ =
  let uri_base_len = String.length uri_base in
  let s = Buf_io.fd_of bio in
  Buf_io.assert_buffer_empty bio;
  let uri = req.Request.uri in
  try
    let relative_url = String.sub uri uri_base_len (String.length uri - uri_base_len) in
    (* file_path is the thing which should be served *)
    let file_path = dir ^ "/" ^ relative_url in
    (* remove any dodgy use of "." or ".." NB we don't prevent the use of symlinks *)
    let file_path = Stdext.Unixext.resolve_dot_and_dotdot file_path in

    if not(String.startswith dir file_path) then begin
      debug "Rejecting request for file: %s (outside of directory %s)" file_path dir;
      Http_svr.response_forbidden ~req s
    end else begin
      let stat = Unix.stat file_path in
      (* if a directory, automatically add index.html *)
      let file_path = if stat.Unix.st_kind = Unix.S_DIR then file_path ^ "/index.html" else file_path in
      response_file s file_path
    end
  with
    _ -> Http_svr.response_missing s (missing uri)
