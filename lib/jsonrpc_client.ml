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

(* JSON-RPC Client *)

module D = Debug.Make(struct let name = "jsonrpc_client" end)
open D

let input_json_object fin =
	let buf = Buffer.create 1024 in
	let brace_cnt = ref 0 in
	let in_string = ref false in
	let last_char () = Buffer.nth buf (Buffer.length buf - 1) in
	let rec get () =
		let c = input_char fin in
		begin
			match c with
			| '{' when not !in_string -> brace_cnt := !brace_cnt + 1
			| '}' when not !in_string -> brace_cnt := !brace_cnt - 1
			| '"' when !in_string && (last_char () <> '\\') -> in_string := false
			| '"' when not !in_string -> in_string := true
			| _ -> ()
		end;
		Buffer.add_char buf c;
		if !brace_cnt > 0 then
			get ()
	in
	get ();
	Buffer.contents buf

let receive fin =
	let obj = input_json_object fin in
	debug "Response: %s" obj;
	Jsonrpc.response_of_string obj

let with_connection sockaddr f =
	let fin, fout = Unix.open_connection sockaddr in
	debug "Connected.";
	let result = f fin fout in
	Unix.shutdown_connection fin;
	close_in fin;
	debug "Shut down.";
	result

let with_rpc ~path ~call =
	let sockaddr = Unix.ADDR_UNIX path in
	with_connection sockaddr (fun fin fout ->
		let req = Jsonrpc.string_of_call call in
		debug "Request: %s" req;
		output_string fout req;
		flush fout;
		receive fin
	)

