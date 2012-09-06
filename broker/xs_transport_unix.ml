(*
 * Copyright (C) Citrix Systems Inc.
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

(** A byte-level transport over the xenstore Unix domain socket *)

open Lwt

let error fmt = Logging.error "xs_transport_unix" fmt

let xenstored_socket = ref "/tmp/socket"

(* Individual connections *)
type t = Lwt_unix.file_descr * Lwt_unix.sockaddr
let create () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  lwt () = Lwt_unix.connect fd sockaddr in
  return (fd, sockaddr)
let destroy (fd, _) = Lwt_unix.close fd
let read (fd, _) = Lwt_unix.read fd
let write (fd, _) bufs ofs len =
	lwt n = Lwt_unix.write fd bufs ofs len in
	if n <> len then begin
		error "Short write (%d<%d)" n len;
		fail End_of_file
	end else return ()

let int_of_file_descr fd =
	let fd = Lwt_unix.unix_file_descr fd in
	let (fd: int) = Obj.magic fd in
	fd

let address_of (fd, _) =
	let creds = Lwt_unix.get_credentials fd in
	let pid = creds.Lwt_unix.cred_pid in
	lwt cmdline =
			Lwt_io.with_file ~mode:Lwt_io.input
				(Printf.sprintf "/proc/%d/cmdline" pid)
				(fun ic ->
					lwt cmdline = Lwt_io.read_line_opt ic in
					match cmdline with
						| Some x -> return x
						| None -> return "unknown") in
	(* Take only the binary name, stripped of directories *)
	let filename =
		try
			let i = String.index cmdline '\000' in
			String.sub cmdline 0 i
		with Not_found -> cmdline in
	let basename = Filename.basename filename in
	let name = Printf.sprintf "%d:%s:%d" pid basename (int_of_file_descr fd) in
	let padto x y =
		if String.length x > y
		then String.sub x 0 y
		else x ^ (String.make (y - (String.length x)) ' ') in
	return (Xs_protocol.Unix(padto name 16))

(* Servers which accept connections *)
type server = Lwt_unix.file_descr

let _ =
	(* Make sure a write to a closed fd doesn't cause us to quit
	   with SIGPIPE *)
	Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let listen () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  lwt () = try_lwt Lwt_unix.unlink !xenstored_socket with _ -> return () in
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd 5;
  return fd

let rec accept_forever fd process =
  lwt conns, _ (*exn_option*) = Lwt_unix.accept_n fd 16 in
  let (_: unit Lwt.t list) = List.map process conns in
  accept_forever fd process

let namespace_of (fd, _) =
	let module Interface = struct
		include Namespace.Unsupported

	let read t (perms: Perms.t) (path: Store.Path.t) =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [] -> ""
		| [ "readable" ] ->
			string_of_bool (Lwt_unix.readable fd)
		| [ "writable" ] ->
			string_of_bool (Lwt_unix.writable fd)
		| _ -> Store.Path.doesnt_exist path

	let exists t perms path = try ignore(read t perms path); true with Store.Path.Doesnt_exist _ -> false

	let list t perms path =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [] -> [ "readable"; "writable" ]
		| _ -> []

	end in
	Some (module Interface: Namespace.IO)

