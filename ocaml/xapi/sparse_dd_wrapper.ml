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
let dd ?(progress_cb=(fun _ -> ())) ?base prezeroed infile outfile size =
	let pipe_read, pipe_write = Unix.pipe () in
	let to_close = ref [ pipe_read; pipe_write ] in
	let close x = if List.mem x !to_close then (Unix.close x; to_close := List.filter (fun y -> y <> x) !to_close) in
	finally
	(fun () ->
		match Forkhelpers.with_logfile_fd "sparse_dd"
			(fun log_fd ->
				let args = [
					"-machine";
					"-src"; infile;
					"-dest"; outfile;
					"-size"; Int64.to_string size
				] @ (if prezeroed then [ "-prezeroed" ] else []
				) @ (Opt.default [] (Opt.map (fun x -> [ "-base"; x ]) base)) in
				debug "%s %s" sparse_dd_path (String.concat " " args);
				let pid = Forkhelpers.safe_close_and_exec None (Some pipe_write) (Some log_fd) []
					sparse_dd_path args in
				close pipe_write;
				(* Read Progress: output from the binary *)
				let open Sparse_encoding in
				Chunk.fold
					(fun () chunk ->
						debug "sparse_dd: %s" chunk.Chunk.data;
						try 
							Scanf.sscanf chunk.Chunk.data "Progress: %d"
								(fun progress ->
									progress_cb (float_of_int progress /. 100.)
								)
						with _ -> ()
					) () pipe_read;
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
	
