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
 * HTTP handler for file uploads (must have chunked encodings)
 * To provoke curl into using chunked encodings, feed it data from stdin eg
 *   cat foo | curl -T - http://server:port/upload
 *)

open Printf
open Http

let upload_file (req: Request.t) (bio: Buf_io.t) =
  let chunks = Http_svr.read_chunked_encoding req bio in
  let s = Buf_io.fd_of bio in
  let oc = open_out "/tmp/test-upload" in
  try
    Http.ll_iter (output_string oc) chunks;
    close_out oc;
    Http.output_http s (Http.http_200_ok ());
    Unix.close s
  with e ->
    close_out oc;
    Http.output_http s (Http.http_400_badrequest ());
    Unix.close s;
    raise e
