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
open Http
open Pervasiveext
open Forkhelpers

module D = Debug.Debugger(struct let name="xapi" end)
open D

let logs_download = Filename.concat Fhs.libexecdir "logs-download"

let logs_download_handler (req: Request.t) s _ =
  debug "running logs-download handler";
  Xapi_http.with_context "Downloading host logs" req s
    (fun __context ->
      Http_svr.headers s (Http.http_200_ok ());
      
      debug "send the http headers";
      let pid = safe_close_and_exec None (Some s) None [] logs_download [] in
      waitpid_fail_if_bad_exit pid)
