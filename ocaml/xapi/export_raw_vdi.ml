(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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
(** HTTP handler for exporting a raw VDI.
 * @group Import and Export
 *)

module D = Debug.Debugger(struct let name="export_raw_vdi" end)
open D

open Http

let handler (req: Request.t) (s: Unix.file_descr) _  =
	debug "export_raw_vdi handler";
	req.Request.close <- true;
	Xapi_http.assert_credentials_ok "VDI.eport_raw" ~http_action:"get_export_raw_vdi" req
