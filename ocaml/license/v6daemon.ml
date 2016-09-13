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

(* v6 licensing daemon *)
open Stdext.Xstringext
open Printf

module D=Debug.Make(struct let name="v6daemon" end)
open D

let xmlrpc_handler process req bio _ =
  Debug.with_thread_associated "v6d_handler" (fun () ->
      let path = match String.split '/' req.Http.Request.uri with
        | x::path::_ -> path
        | _ -> failwith "Unknown path"
      in
      debug "path=%s" path;
      let body = Http_svr.read_body req bio in
      let s = Buf_io.fd_of bio in
      let rpc = Xmlrpc.call_of_string body in
      debug "Request name: %s" rpc.Rpc.name ;
      List.iter (fun param ->
          debug "Request param: %s" (Rpc.to_string param)
        ) rpc.Rpc.params ;
      let result = process rpc in
      debug "Response: %s" (Rpc.to_string result.Rpc.contents);
      let str = Xmlrpc.string_of_response result in
      Http_svr.response_str req s str
    ) ()

let server = Http_svr.Server.empty ()

let startup process =
  Debug.with_thread_associated "daemon_init" (fun () ->
      info "(Re)starting v6d...";
      (* unix socket *)
      let unix_socket_path = Filename.concat "/var/lib/xcp" "v6" in
      Stdext.Unixext.mkdir_safe (Filename.dirname unix_socket_path) 0o700;
      Stdext.Unixext.unlink_safe unix_socket_path;
      let domain_sock = Http_svr.bind (Unix.ADDR_UNIX(unix_socket_path)) "unix_rpc" in
      Http_svr.start server domain_sock;
      Http_svr.Server.add_handler server Http.Post "/" (Http_svr.BufIO (xmlrpc_handler process));
      ignore Daemon.(notify State.Ready);
      (* keep daemon alive *)
      Stdext.Threadext.keep_alive ()
    ) ()

