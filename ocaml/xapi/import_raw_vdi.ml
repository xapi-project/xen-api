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
(* HTTP handler for importing a raw VDI *)

module D=Debug.Debugger(struct let name="import" end)
open D

open Http
open Importexport
open Unixext
open Pervasiveext

let handler (req: request) (s: Unix.file_descr) =
  req.close := true;
  Xapi_http.with_context "Importing raw VDI" req s
    (fun __context ->
      let vdi = 
	if List.mem_assoc "vdi" req.Http.query
	then Ref.of_string (List.assoc "vdi" req.Http.query)
	else raise (Failure "Missing vdi query parameter") in
      try
	match req.transfer_encoding, req.content_length with
	| Some "chunked", _ ->
	    error "Chunked encoding not yet implemented in the import code";
	    Http_svr.headers s http_403_forbidden;
	    raise (Failure "import code cannot handle chunked encoding")
	| None, Some len ->
	    let headers = Http.http_200_ok ~keep_alive:false () @
	      [ Http.task_id_hdr ^ ":" ^ (Ref.string_of (Context.get_task_id __context));
		content_type ] in
            Http_svr.headers s headers;
	    
	    Helpers.call_api_functions ~__context
	      (fun rpc session_id ->
		 Sm_fs_ops.with_block_attached_device __context rpc session_id vdi `RW
		   (fun device ->
		      let fd = Unix.openfile device  [ Unix.O_WRONLY ] 0 in
		      finally 
			(fun () -> Unixext.copy_file ~limit:len s fd)
			(fun () -> Unix.close fd)
		   )
	      );

	    TaskHelper.complete ~__context []
      with e ->
	TaskHelper.failed ~__context (Api_errors.internal_error, ["Caught exception: " ^ (ExnHelper.string_of_exn e)]);
	raise e)
