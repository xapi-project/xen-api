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

open Client

let xapirpc xml = Xmlrpcclient.do_xml_rpc_unix ~version:"1.0" ~filename:"/var/xapi/xapi" ~path:"/" xml

let send_alert msg body = 
	let host_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
	let session = Client.Session.login_with_password ~rpc:xapirpc ~uname:"" ~pwd:"" ~version:Xapi_globs.api_version_string in
	Pervasiveext.finally
		(fun () -> Client.Message.create xapirpc session msg 1L `Host host_uuid body)
		(fun () -> Client.Session.logout xapirpc session)

