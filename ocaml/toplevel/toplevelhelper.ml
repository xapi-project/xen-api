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

let host = ref ""
let port = ref 0

let rpc xml =
	let open Xmlrpcclient in
	let transport = SSL(SSL.make(), !host, !port) in
	let http = xmlrpc ~version:"1.1" "/" in
	XML_protocol.rpc ~transport ~http xml

open Client

let init_session username password =
  Client.Session.login_with_password ~rpc ~uname:username ~pwd:password ~version:"1.2" ~originator:"toplevel"
