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

open Pervasiveext

(** Path to the gzip binary *)
let gzip = "/bin/gzip"

(** Helper function to prevent double-closes of file descriptors *)
let close to_close fd = 
  if List.mem fd !to_close then Unix.close fd;
  to_close := List.filter (fun x -> fd <> x) !to_close 

type zcat_mode = Compress | Decompress

type input_type = 
  | Active  (** we provide a function which writes into the compressor and a fd output *)
  | Passive (** we provide an fd input and a function which reads from the compressor *)

(* renice/ionice the pid with lowest priority so that it doesn't *)
(* use up all cpu resources in dom0 *)
let lower_priority pid =
  let pid=Printf.sprintf "%d" (Forkhelpers.getpid pid) in
  (* renice 19 -p pid *)
  let renice="/usr/bin/renice" in
  let renice_args = ["19";"-p";pid] in
  let _=Forkhelpers.execute_command_get_output renice renice_args in
  (* ionice -c 3 [idle] -p pid *)
  let ionice="/usr/bin/ionice" in
  let ionice_args = ["-c";"3";"-p";pid] in
  let _=Forkhelpers.execute_command_get_output ionice ionice_args in
  ()

(** Runs a zcat process which is either:
    i) a compressor; or (ii) a decompressor
    and which has either
    i) an active input (ie a function and a pipe) + passive output (fd); or
    ii) a passive input (fd) + active output (ie a function and a pipe)
*)
let go (mode: zcat_mode) (input: input_type) fd f = 
    let zcat_out, zcat_in = Unix.pipe() in
    
    let to_close = ref [ zcat_in; zcat_out ] in
    let close = close to_close in
    
    finally
      (fun () ->
	 let args = if mode = Compress then [] else ["--decompress"] @ [ "--stdout"; "--force" ] in

	 let stdin, stdout, close_now, close_later = match input with
	   | Active -> 
	       Some zcat_out,                              (* input comes from the pipe+fn *)
	       Some fd,                                    (* supplied fd is written to *)
	       zcat_out,                                   (* we close this now *)
	       zcat_in                                     (* close this before waitpid *)
	   | Passive -> 
	       Some fd,                                    (* supplied fd is read from *)
	       Some zcat_in,                               (* output goes into the pipe+fn *) 
	       zcat_in,                                    (* we close this now *)
	       zcat_out in                                 (* close this before waitpid *)
	 let pid = Forkhelpers.safe_close_and_exec stdin stdout None [] gzip args in
	 close close_now;
   finally
     (fun () -> 
       lower_priority pid;  (* lowest priority to gzip *)
       f close_later
     )
	   (fun () ->
	      let failwith_error s =
		let mode = if mode = Compress then "Compression" else "Decompression" in
		let msg = Printf.sprintf "%s via zcat failed: %s" mode s in
		Printf.eprintf "%s" msg;
		failwith msg
	        in
	      close close_later;
	      match snd (Forkhelpers.waitpid pid) with
	      | Unix.WEXITED 0 -> ();
	      | Unix.WEXITED i -> failwith_error (Printf.sprintf "exit code %d" i)
	      | Unix.WSIGNALED i -> failwith_error (Printf.sprintf "killed by signal %d" i)
	      | Unix.WSTOPPED i -> failwith_error (Printf.sprintf "stopped by signal %d" i)
	   )
      ) (fun () -> List.iter close !to_close)

let compress fd f = go Compress Active fd f
let decompress fd f = go Decompress Active fd f

let decompress_passive fd f = go Decompress Passive fd f
