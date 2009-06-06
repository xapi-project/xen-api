(* 
 * Copyright (c) 2006 XenSource Inc.
 * Author: Jon Ludlam <jludlam@xensource.com>
 *
 * HTTP handler for serving files in the rt subdir
 *)

open Printf
open Http
open Stringext
open Pervasiveext

module D = Debug.Debugger(struct let name="xapi" end)
open D

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

let send_file (uri_base: string) (dir: string) (req: request) (bio: Buf_io.t) =
  let uri_base_len = String.length uri_base in
  let s = Buf_io.fd_of bio in
  Buf_io.assert_buffer_empty bio;
  let uri = req.uri in
  try
    let relative_url = String.sub uri uri_base_len (String.length uri - uri_base_len) in
    (* file_path is the thing which should be served *)
    let file_path = dir ^ "/" ^ relative_url in
    (* remove any dodgy use of "." or ".." NB we don't prevent the use of symlinks *)
    let file_path = Unixext.resolve_dot_and_dotdot file_path in

    if not(String.startswith dir file_path) then begin 
      debug "Rejecting request for file: %s (outside of directory %s)" file_path dir;
      Http_svr.default_callback req bio
    end else begin
      let stat = Unix.stat file_path in
      (* if a directory, automatically add index.html *)
      let file_path = if stat.Unix.st_kind = Unix.S_DIR then file_path ^ "/index.html" else file_path in
      
      let mime_content_type =
        let ext = may String.lowercase (get_extension file_path) in
        match ext with
        | Some "html" | Some "htm" -> Some "text/html"
        | Some "css" -> Some "text/css"
        | Some "js"  -> Some "application/javascript"
        | Some "gif" -> Some "image/gif"
        | Some "png" -> Some "image/png"
        | Some "jpg" | Some "jpeg" -> Some "image/jpeg"
        | Some _ | None -> Some "application/octet-stream"
        in
      Http_svr.response_file ~mime_content_type s file_path
    end
  with
      _ -> Http_svr.response_missing s (missing uri)
