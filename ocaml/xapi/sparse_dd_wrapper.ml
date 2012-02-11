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
(* Allows xapi to drive the sparse_dd program *)

open Vmopshelpers
open Pervasiveext
open Client
open Printf

module D=Debug.Debugger(struct let name="xapi" end)
open D

let sparse_dd_path = Filename.concat Fhs.libexecdir "sparse_dd"

(** Use the new external sparse_dd program *)
let dd ~__context prezeroed infile outfile size = 
	let pipe_read, pipe_write = Unix.pipe () in
	let to_close = ref [ pipe_read; pipe_write ] in
	let close x = if List.mem x !to_close then (Unix.close x; to_close := List.filter (fun y -> y <> x) !to_close) in
	finally
	(fun () ->
		match Forkhelpers.with_logfile_fd "sparse_dd"
			(fun log_fd ->
				let pid = Forkhelpers.safe_close_and_exec None (Some pipe_write) (Some log_fd) []
				sparse_dd_path
				([ "-machine"; "-src"; infile; "-dest"; outfile; "-size"; Int64.to_string size ] @
				(if prezeroed then [ "-prezeroed" ] else [])) in
				close pipe_write;
				(* Read Progress: output from the binary *)
				let buf = String.create 128 in
				let finished = ref false in
				while not (!finished) do
					let n = Unix.read pipe_read buf 0 (String.length buf) in
					if n = 0 then finished := true else debug "sparse_dd: %s" (String.sub buf 0 n);
					try 
						Scanf.sscanf (String.sub buf 0 n) "Progress: %d"
						(fun progress ->
							TaskHelper.exn_if_cancelling ~__context;
							TaskHelper.operate_on_db_task ~__context 
		  					(fun self -> Db.Task.set_progress ~__context ~self ~value:(float_of_int progress /. 100.))
						)
					with _ -> ()
				done;
				match Forkhelpers.waitpid pid with
				| (_, Unix.WEXITED 0) -> ()
				| (_, Unix.WEXITED n) -> error "sparse_dd exit: %d" n; failwith "sparse_dd"
				| _ -> error "sparse_dd exit with WSTOPPED or WSIGNALED"; failwith "sparse_dd"
			) with
		| Forkhelpers.Success _ -> ()
		| Forkhelpers.Failure (log, exn) ->
			error "Failure from sparse_dd: %s" log;
			raise exn	
	)
	(fun () ->
		close pipe_read;
		close pipe_write)
	
