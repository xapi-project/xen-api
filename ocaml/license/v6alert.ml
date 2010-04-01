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

let send_v6_grace_license () =
	ignore (send_alert Api_messages.v6_grace_license "The license server is unreachable. However, a grace license is given, as a similar license was successfully checked out recently.")

let send_v6_upgrade_grace_license () =
	ignore (send_alert Api_messages.v6_grace_license "An upgrade grace license is given, which allows 30 days to connect to a license server holding a valid license.")
	
let send_v6_rejected () =
	ignore (send_alert Api_messages.v6_rejected "The requested license is not available at the license server.")

let send_v6_comm_error () =
	ignore (send_alert Api_messages.v6_comm_error "The license could not be checked out, because the license server could not be reached at the given address/port. Please check the connection details, and verify that the license server is running.")
