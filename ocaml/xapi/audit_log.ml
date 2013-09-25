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
module D = Debug.Make(struct let name="audit_log" end)
open D

open Http
open Stringext
open Pervasiveext

let audit_log_whitelist_prefix = "/var/log/audit.log"

let line_timestamp_length = 21 (* the timestamp length at the debug line *)

(* location of [ at the beginning of the line timestamp *)
let timestamp_index line = 
	try ((String.index line '[') + 1) with Not_found -> 0

let went_through ?filter line =
	match filter with
	|None->true
	|Some fs->
		 List.fold_left
			 (fun acc f->acc&&(String.has_substr line f))
			 true
       fs

let write_line line fd ?filter since =
	if String.length line >
		(line_timestamp_length + (timestamp_index line))
	then
	let line_timestamp =
		String.sub line (timestamp_index line) line_timestamp_length
	in
	if since="" || ((String.compare line_timestamp since) >= 0)
	then
	if went_through ?filter line
  then
	let len = String.length line in
	ignore(Unix.write fd line 0 len)

let transfer_audit_file _path compression fd_out ?filter since : unit =
	let path = Unixext.resolve_dot_and_dotdot _path in
	let in_whitelist = (String.startswith audit_log_whitelist_prefix path) in
	if in_whitelist then
	let file_exists = (Unixext.file_exists path) in
	if file_exists then
	begin
		debug "transfer_audit_file path=%s,compression=[%s],since=%s" path compression since;
		try
			if compression="" (* uncompressed *)
			then begin
				Unixext.readfile_line
					(fun line -> write_line (line^"\n") fd_out ?filter since)
					path
			end
			else if compression="gz"
			then (
				Unixext.with_file path [ Unix.O_RDONLY ] 0o0
					(fun gz_fd_in ->
						Gzip.decompress_passive gz_fd_in
							(fun fd_in -> (*fd_in is closed by gzip module*)
								let cin = Unix.in_channel_of_descr fd_in in
								try
									while true do
										let line = input_line cin in
										write_line (line^"\n") fd_out ?filter since
									done
								with End_of_file -> () (* ok, expected *)
							)
					)
			)
			else (
				(* nothing to do with an unknown file format *)
				debug "unknown compression format %s in audit log file %s" compression path
			)
		with e -> begin
			debug "error reading audit log file %s: %s" path (ExnHelper.string_of_exn e);
			raise e
		end
	end

let transfer_all_audit_files fd_out ?filter since =
	let atransfer _infix _suffix =
		let infix = if _infix="" then "" else "."^_infix in
		let suffix = if _suffix="" then "" else "."^_suffix in
		transfer_audit_file
			(audit_log_whitelist_prefix^infix^suffix)
			_suffix
			fd_out
			?filter
			since
	in
	let atransfer_try_gz infix =
		ignore_exn (fun ()->atransfer infix "gz");(* try the compressed file *)
		ignore_exn (fun ()->atransfer infix "") (* then the uncompressed one *)
	in
	(* go through audit.log.n->0 first, ascending order of time *)
	for i=100 downto 0 do
		atransfer_try_gz (string_of_int i)
	done;
	(* finally transfer /var/log/audit.log (the latest one in time) *)
	atransfer_try_gz ""


(* map the ISO8601 timestamp format into the one in our logs *)
let log_timestamp_of_iso8601 iso8601_timestamp =
	let step1 = iso8601_timestamp in
	let step2 = Stringext.String.replace "-" "" step1 in
	let step3 = Stringext.String.replace "Z" "" step2 in
	step3

(*
 Assume that RBAC access for the session_id already verified by xapi_http.ml
 
 GET /audit_log?session_id=<session>&task_id=<task>&
                [since=<timestamp in ISO 8601 / log.gettimestring() format>]

 eg. /audit_log?...&since=20090910T11:31:11.417
 eg. /audit_log?...&since=20090910%2011:31:11.417
 eg. /audit_log?...&since=2009-09-10T11:31:11.417Z
 eg. /audit_log?...&since=2009-09-10%2011:31:11.417
 eg. /audit_log?...&since=2009-09-10T11:31
 eg. /audit_log?...&since=2009-09-10
*)
let handler (req: Request.t) (bio: Buf_io.t) _ =

	let s = Buf_io.fd_of bio in
	Buf_io.assert_buffer_empty bio;
	req.Request.close <- true;

	Xapi_http.with_context (* makes sure to signal task-completed to cli *)
		(Printf.sprintf "audit_log_get request")
		req
		s
		(fun __context ->

			 let all = req.Request.cookie @ req.Request.query in
			 let since_iso8601 =
				 if List.mem_assoc "since" all then List.assoc "since" all else ""
			 in
			 let since = log_timestamp_of_iso8601 since_iso8601 in
			 (*debug "since=[%s]" since;*)
			 (* we need to return an http header without content-length *)
			 Http_svr.headers s (http_200_ok() @ [ Http.Hdr.content_type ^": text/plain"]);
			 (* then the contents *)
			 transfer_all_audit_files s since
		)
