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
open Create_storage
open Client

let _ = 
  let host = ref "localhost"
  and port = ref Xapi_globs.default_cleartext_port
  and username = ref "root"
  and password = ref "" in
  Arg.parse 
    [ "-h", Arg.Set_string host, "hostname to configure";
      "-p", Arg.Set_int port, "port to talk to";
      "-u", Arg.Set_string username, "username to log in as";
      "-pw", Arg.Set_string password, "password to use"
    ] (fun _ -> ())
    "Create the default set of SRs on a server";

  let open Xmlrpcclient in
  let http = xmlrpc ~version:"1.0" "/" in
  let rpc xml = XML_protocol.rpc ~transport:(TCP(!host, !port)) ~http xml in
  let session_id = Client.Session.login_with_password ~rpc 
    ~uname:!username ~pwd:!password ~version:Xapi_globs.api_version_string ~originator:"" in
  create_storage_localhost rpc session_id;
  Client.Session.logout ~rpc ~session_id
