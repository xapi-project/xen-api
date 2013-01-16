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

open Pervasiveext
open Client
open Printf
open Threadext

module D=Debug.Debugger(struct let name="xapi" end)
open D

let sparse_dd_path = Filename.concat Fhs.libexecdir "sparse_dd"

type progress =
		| Started of Forkhelpers.pidty
		| Continuing of float
		| Finished of exn option

exception Cancelled

(** Use the new external sparse_dd program *)
let dd_internal progress_cb base prezeroed infile outfile size =
	let pipe_read, pipe_write = Unix.pipe () in
	let to_close = ref [ pipe_read; pipe_write ] in
	let close x = if List.mem x !to_close then (Unix.close x; to_close := List.filter (fun y -> y <> x) !to_close) in
	finally
	(fun () ->
		try match Forkhelpers.with_logfile_fd "sparse_dd"
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
				progress_cb (Started pid);
				(* Read Progress: output from the binary *)
				let open Sparse_encoding in
				Chunk.fold
					(fun () chunk ->
						debug "sparse_dd: %s" chunk.Chunk.data;
						try 
							Scanf.sscanf chunk.Chunk.data "Progress: %d"
								(fun progress ->
									progress_cb (Continuing (float_of_int progress /. 100.))
								)
						with _ -> ()
					) () pipe_read;
				match Forkhelpers.waitpid pid with
				| (_, Unix.WEXITED 0) -> progress_cb (Finished None)
				| (_, Unix.WEXITED 5) -> error "sparse_dd received NBD error"; failwith "sparse_dd NBD error"
				| (_, Unix.WEXITED n) -> error "sparse_dd exit: %d" n; failwith "sparse_dd"
				| _ -> error "sparse_dd exit with WSTOPPED or WSIGNALED"; failwith "sparse_dd"
			) with
		| Forkhelpers.Success _ -> progress_cb (Finished None)
		| Forkhelpers.Failure (log, exn) ->
			error "Failure from sparse_dd: %s raising %s" log (Printexc.to_string exn);
		        raise (Api_errors.Server_error (("VDI_COPY_FAILED", [Printexc.to_string exn])));		
		with e -> 
			progress_cb (Finished (Some e));
			raise e
	)
	(fun () ->
		close pipe_read;
		close pipe_write)
	
let dd ?(progress_cb=(fun _ -> ())) ?base prezeroed =
	dd_internal (function | Continuing x -> progress_cb x | _ -> ()) base prezeroed


let start ?(progress_cb=(fun _ -> ())) ?base prezeroed infile outfile size =
	let m = Mutex.create () in
	let c = Condition.create () in
	let pid = ref None in
	let finished = ref false in
	let cancelled = ref false in
	let exn = ref None in
	let thread_progress_cb = function
		| Started pid' -> 
			pid := Some pid';
			Mutex.execute m (fun () -> Condition.broadcast c)
		| Continuing progress -> progress_cb progress
		| Finished exn' ->
			finished := true;
			exn := exn';
			Mutex.execute m (fun () -> Condition.broadcast c)
	in
	let _ = Thread.create (fun () ->
		dd_internal thread_progress_cb base prezeroed infile outfile size) () in
	Mutex.execute m (fun () ->
		while (!pid = None) && (!finished = false) && (!cancelled = false) do
			Condition.wait c m
		done);
	match (!pid,!exn) with
		| Some pid, None -> 
			(m,c,pid,finished,cancelled,exn)
		| _, Some e ->
			raise e
		| _ ->
			failwith "Unexpected error in start_dd"
		
let wait (m,c,pid,finished,cancelled,exn) =
	Mutex.execute m (fun () ->
		while (!finished = false) do
			Condition.wait c m
		done);
	if !cancelled then raise Cancelled;
	match !exn with 
		| Some exn -> raise exn
		| None -> ()

let cancel (m,c,pid,finished,cancelled,exn) =
	cancelled := true;
	let pid = Forkhelpers.getpid pid in
	try Unix.kill pid Sys.sigkill with _ -> () 

		
				
		
