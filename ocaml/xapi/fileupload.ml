(* 
 * Copyright (c) 2007 XenSource Inc.
 * Author: David Scott <david.scott@xensource.com>
 *
 * HTTP handler for file uploads (must have chunked encodings)
 * To provoke curl into using chunked encodings, feed it data from stdin eg
 *   cat foo | curl -T - http://server:port/upload
 *)

open Printf
open Http

let upload_file (req: request) (bio: Buf_io.t) =
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
    Http.output_http s Http.http_400_badrequest;
    Unix.close s;
    raise e
