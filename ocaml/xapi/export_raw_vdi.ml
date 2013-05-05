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

let localhost_handler rpc session_id vdi (req: Http.Request.t) (s: Unix.file_descr) =
	req.Http.Request.close <- true;
	Xapi_http.with_context "Exporting raw VDI" req s
		(fun __context ->
			let task_id = Context.get_task_id __context in
			debug "export_raw_vdi task_id = %s vdi = %s;" (Ref.string_of task_id) (Ref.string_of vdi);
			try
				Sm_fs_ops.with_open_block_attached_device __context rpc session_id vdi `RO
					(fun fd ->
						let headers = Http.http_200_ok ~keep_alive:false () @
							[ Http.Hdr.task_id ^ ":" ^ (Ref.string_of task_id); Http.Hdr.content_type ] in
						Http_svr.headers s headers;
						try
							debug "Copying VDI contents...";
							ignore(Unixext.copy_file fd s);
							debug "Copying VDI complete.";
							Unixext.fsync fd;
						with Unix.Unix_error(Unix.EIO, _, _) ->
							raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O errors"]))
					)
			with e ->
				log_backtrace ();
				TaskHelper.failed ~__context (Api_errors.internal_error, ["Caught exception: " ^ (ExnHelper.string_of_exn e)]);
				raise e)

let export_raw vdi (req: Http.Request.t) (s: Unix.file_descr) _ =
	(* Check the SR is reachable (in a fresh task context) *)
	Server_helpers.exec_with_new_task "VDI.export_raw_vdi"
		(fun __context ->
			Helpers.call_api_functions ~__context
			(fun rpc session_id ->
				let sr = Db.VDI.get_SR ~__context ~self:vdi in
				debug "Checking whether localhost can see SR: %s" (Ref.string_of sr);
				if (Importexport.check_sr_availability ~__context sr)
				then localhost_handler rpc session_id vdi req s
				else
					let host = Importexport.find_host_for_sr ~__context sr in
					let address = Db.Host.get_address ~__context ~self:host in
					Importexport.return_302_redirect req s address
			)
		)

let handler (req: Http.Request.t) (s: Unix.file_descr) _  =
	debug "export_raw_vdi handler";
	Xapi_http.assert_credentials_ok "VDI.export_raw" ~http_action:"get_export_raw_vdi" req;

	Server_helpers.exec_with_new_task "VDI.export_raw_vdi"
		(fun __context ->
			export_raw (Importexport.vdi_of_req ~__context req) req s ()
		)
