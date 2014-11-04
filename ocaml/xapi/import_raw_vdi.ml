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
(** HTTP handler for importing a raw VDI.
 * @group Import and Export
 *)

module D=Debug.Make(struct let name="import" end)
open D

open Http
open Importexport
open Sparse_encoding
open Unixext
open Pervasiveext
open Client

let localhost_handler rpc session_id vdi (req: Request.t) (s: Unix.file_descr) =
	req.Request.close <- true;
	Xapi_http.with_context "Importing raw VDI" req s
		(fun __context ->
			let prezeroed = not (Sm_fs_ops.must_write_zeroes_into_new_vdi ~__context vdi) in
			let all = req.Request.query @ req.Request.cookie in
			let chunked = List.mem_assoc "chunked" all in
			let task_id = Context.get_task_id __context in
			match Importexport.Format.of_req req with
			| `Unknown x ->
				error "import_raw_vdi task_id = %s; vdi = %s; unknown disk format = %s"
					(Ref.string_of task_id) (Ref.string_of vdi) x;
				TaskHelper.failed ~__context (Api_errors.Server_error(Api_errors.internal_error, ["Unknown format " ^ x]));
				Http_svr.headers s (Http.http_404_missing ~version:"1.0" ())
			| `Ok format when format <> Importexport.Format.Raw && chunked ->
				error "import_raw_vdi task_id = %s; vdi = %s; unable to import a .vhd using chunked encoding"
					(Ref.string_of task_id) (Ref.string_of vdi)
			| `Ok format ->
				debug "import_raw_vdi task_id = %s vdi = %s; chunked = %b; format = %s"
					(Ref.string_of task_id) (Ref.string_of vdi) chunked (Importexport.Format.to_string format);
				try
					match req.Request.transfer_encoding with
					| Some x ->
						error "Chunked encoding not yet implemented in the import code";
						Http_svr.headers s (http_403_forbidden ());
						raise (Failure (Printf.sprintf "import code cannot handle encoding: %s" x))
					| None ->
							Sm_fs_ops.with_block_attached_device __context rpc session_id vdi `RW
								(fun path ->
									let headers = Http.http_200_ok ~keep_alive:false () @
										[ Http.Hdr.task_id ^ ":" ^ (Ref.string_of task_id);
										content_type ] in
									Http_svr.headers s headers;
									if chunked
									then Vhd_tool_wrapper.receive (Vhd_tool_wrapper.update_task_progress __context) "raw" "chunked" s path "" prezeroed
									else Vhd_tool_wrapper.receive (Vhd_tool_wrapper.update_task_progress __context) (Importexport.Format.to_string format) "none" s path "" prezeroed
								);
						TaskHelper.complete ~__context None;
				with e ->
					Backtrace.is_important e;
					error "Caught exception: %s" (ExnHelper.string_of_exn e);
					TaskHelper.failed ~__context e;
					raise e)

let import vdi (req: Request.t) (s: Unix.file_descr) _ =
	Xapi_http.assert_credentials_ok "VDI.import" ~http_action:"put_import_raw_vdi" req;

	(* Perform the SR reachability check using a fresh context/task because
	   we don't want to complete the task in the forwarding case *)
	Server_helpers.exec_with_new_task "VDI.import" 
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
				return_302_redirect req s address
		)
       )


let handler (req: Request.t) (s: Unix.file_descr) _ =
	Xapi_http.assert_credentials_ok "VDI.import" ~http_action:"put_import_raw_vdi" req;

	(* Using a fresh context/task because we don't want to complete the
	   task in the forwarding case *)
	Server_helpers.exec_with_new_task "VDI.import" 
	(fun __context ->
		import (vdi_of_req ~__context req) req s ()
	)
