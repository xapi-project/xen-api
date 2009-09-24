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
(* XSRPC client *)

let () =
	let domid = ref (-1) in
	let use_stdin = ref false in
	let data = ref "" in
	let cmd = ref "" in
	let service = ref "" in
	Arg.parse [
		"-service", Arg.Set_string service, "RPC Service name";
		"-domid", Arg.Set_int domid, "Domain ID to query";
		"-cmd", Arg.Set_string cmd, "Command";
		"-stdin", Arg.Set use_stdin, "pass data from stdin instead of as an argument";
	] (fun anon -> data := anon) "x";

	let data =
		if !use_stdin then (
			let buf = Buffer.create 1024 in
			let s = String.make 1024 '\000' in
			begin try
				while true
				do
					let rd = input stdin s 0 1024 in
					Buffer.add_substring buf s 0 rd
				done
			with End_of_file -> ()
			end;
			Buffer.contents buf
		) else
			!data
		in
	if !service = "" then (
		Printf.eprintf "error: you need to specify a service\n";
		exit 1	
	);
	if !cmd = "" then (
		Printf.eprintf "error: you need to specify a command\n";
		exit 1	
	);

	let domid = !domid in
	let service = !service in
	let cmd = !cmd in

	let t = Xsrpc.bind domid service in

	let status, msg = Xsrpc.query t cmd data in
	match status with
	| Xsrpc.Error -> Printf.eprintf "error: %s\n" msg; exit 1
	| Xsrpc.Success -> Printf.printf "%s" msg
