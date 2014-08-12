(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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

module M = struct
	type ('a, 'b) t = [ `Ok of 'a | `Error of 'b ]
    let (>>=) m f = match m with | `Ok x -> f x | `Error x -> `Error x
    let return x = `Ok x
end

open M

let (|>) a f = f a

module Xenops_record = struct
	type t = {
		time: string;
		word_size: int;
		xs_subtree: (string * string) list option;
	}

	let make ?xs_subtree () =
		let time = Date.(to_string (of_float (Unix.time ()))) in
		let word_size = Sys.word_size in
		{ word_size; time; xs_subtree }

	(* This needs to be compatible with usptream Xenopsd which uses sexplib
	 * which cannot be ported to this branch. As such the following is an
	 * recreation of what would happen when calling:
	 *     sexp_of_t t |> Sexplib.Sexp.to_string
	 * and has been tested in utop by chaining this function with sexplib like:
	 *     to_string t |> Sexplib.Sexp.of_string |> t_of_sexp
	 * and checking that we get t back where sexp_of_t and t_of_sexp are the
	 * result of the following type definition with syntax extension upstream:
	 *     type t = {
	 *       time: string;
	 *       word_size: int;
	 *       xs_subtree: (string * string) list sexp_option;
	 *     } with sexp
	 * Upsteam may grow to have more fields, but as long as these have the
	 * sexp_option modifier then what we produce will be parsed correctly *)
	let to_string t =
		Printf.sprintf "((time %s)(word_size %d)%s)"
		t.time t.word_size
		begin match t.xs_subtree with
		| None -> ""
		| Some entries ->
			List.map (fun (k, v) ->
				Printf.sprintf "(\"%s\" \"%s\")"
				(String.escaped k) (String.escaped v)
			) entries
			|> String.concat ""
			|> Printf.sprintf "(xs_subtree(%s))"
		end
end


type format = Structured | Legacy

type header_type =
	| Xenops
	| Libxc
	| Libxl
	| Libxc_legacy
	| Qemu_trad
	| Qemu_xen
	| Demu
	| End_of_image

exception Invalid_header_type

let header_type_of_int64 = function
	| 0x000fL -> `Ok Xenops
	| 0x00f0L -> `Ok Libxc
	| 0x00f1L -> `Ok Libxl
	| 0x00f2L -> `Ok Libxc_legacy
	| 0x0f00L -> `Ok Qemu_trad
	| 0x0f01L -> `Ok Qemu_xen
	| 0x0f10L -> `Ok Demu
	| 0xffffL -> `Ok End_of_image
	| _ -> `Error Invalid_header_type

let int64_of_header_type = function
	| Xenops       -> 0x000fL
	| Libxc        -> 0x00f0L
	| Libxl        -> 0x00f1L
	| Libxc_legacy -> 0x00f2L
	| Qemu_trad    -> 0x0f00L
	| Qemu_xen     -> 0x0f01L
	| Demu         -> 0x0f10L
	| End_of_image -> 0xffffL

type header = header_type * int64 (* length *)

let wrap f =
	try
		return (f ())
	with e -> 
		`Error e

let read_int64 fd = wrap (fun () -> Io.read_int64 ~endianness:`little fd)
let write_int64 fd x = wrap (fun () -> Io.write_int64 ~endianness:`little fd x)

let save_signature = "XenSavedDomv2-\n"
let legacy_save_signature = "XenSavedDomain\n"
let legacy_qemu_save_signature = "QemuDeviceModelRecord\n"
let qemu_save_signature_legacy_libxc = "DeviceModelRecord0002"

let write_save_signature fd = Io.write fd save_signature
let read_save_signature fd =
	match Io.read fd (String.length save_signature) with
	| x when x = save_signature -> `Ok Structured
	| x when x = legacy_save_signature -> `Ok Legacy
	| x -> `Error (Printf.sprintf "Not a valid signature: \"%s\"" x)

let read_legacy_qemu_header fd =
	try
		match Io.read fd (String.length legacy_qemu_save_signature) with
		| x when x = legacy_qemu_save_signature ->
			`Ok (Int64.of_int (Io.read_int ~endianness:`big fd))
		| x -> `Error "Read invalid legacy qemu save signature"
	with e ->
		`Error ("Failed to read signature: " ^ (Printexc.to_string e))

let write_qemu_header_for_legacy_libxc fd size =
	wrap (fun () -> Io.write fd qemu_save_signature_legacy_libxc) >>= fun () ->
	wrap (fun () -> Io.write_int ~endianness:`little fd (Io.int_of_int64_exn size))

let read_header fd =
	read_int64 fd >>= fun x ->
	header_type_of_int64 x >>= fun hdr ->
	read_int64 fd >>= fun len ->
	return (hdr, len)

let write_header fd (hdr_type, len) =
	write_int64 fd (int64_of_header_type hdr_type) >>= fun () ->
	write_int64 fd len

let conv_script = "/usr/lib64/xen/bin/convert-legacy-stream"

let check_conversion_script () =
	let open Unix in
	try return (access conv_script [X_OK])
	with _ -> `Error (Failure (Printf.sprintf "Executable not found: %s" conv_script))

type 'a thread_status = Running | Thread_failure of exn | Success of 'a

let with_conversion_script task name hvm fd f =
	let module D = Debug.Debugger(struct let name = "suspend_image_conversion" end) in
	let open D in
	let open Pervasiveext in
	let open Threadext in
	check_conversion_script () >>= fun () ->
	let (pipe_r, pipe_w) = Unix.pipe () in
	let fd_uuid = Uuid.(to_string (make_uuid ()))
	and pipe_w_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let args =
		[ "--in"; fd_uuid; "--out"; pipe_w_uuid;
			"--width"; "32"; "--skip-qemu";
			"--guest-type"; if hvm then "hvm" else "pv";
			"--syslog";
		]
	in
	let (m, c) = Mutex.create (), Condition.create () in
	let spawn_thread_and_close_fd name fd' f =
		let status = ref Running in
		let thread =
			Thread.create (fun () ->
				try
					let result =
						finally (fun () -> f ()) (fun () -> Unix.close fd')
					in
					Mutex.execute m (fun () ->
						status := Success result;
						Condition.signal c
					)
				with e ->
					Mutex.execute m (fun () ->
						status := Thread_failure e;
						Condition.signal c
					)
			) ()
		in
		(thread, status)
	in
	let (conv_th, conv_st) =
		spawn_thread_and_close_fd "convert-legacy-stream" pipe_w (fun () ->
			Cancel_utils.cancellable_subprocess task
				[ fd_uuid, fd; pipe_w_uuid, pipe_w; ] conv_script args
		)
	and (f_th, f_st) =
		spawn_thread_and_close_fd name pipe_r (fun () ->
			f pipe_r
		)
	in
	debug "Spawned threads for conversion script and %s" name;
	let rec handle_threads () = match (!conv_st, !f_st) with
	| Thread_failure e, _ ->
		`Error (Failure (Printf.sprintf "Conversion script thread caught exception: %s"
			(Printexc.to_string e)))
	| _, Thread_failure e ->
		`Error (Failure (Printf.sprintf "Thread executing %s caught exception: %s"
			name (Printexc.to_string e)))
	| Running, _ | _, Running ->
		Condition.wait c m;
		handle_threads ()
	| Success _, Success res ->
		debug "Waiting for conversion script thread to join";
		Thread.join conv_th;
		debug "Waiting for xenguest thread to join";
		Thread.join f_th;
		`Ok res
	in
	Mutex.execute m handle_threads
