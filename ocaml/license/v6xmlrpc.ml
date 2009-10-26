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

module D = Debug.Debugger(struct let name="v6xmlrpc" end)
open D

exception Unmarshalling_error of string

module type V6api = sig
	val initialise : string -> int32 -> string -> string * int32
	val shutdown : unit -> bool
	val reopen_logs : unit -> bool
end

module V6process = functor(V: V6api) -> struct
	let myassoc key args =
		try List.assoc key args with Not_found -> raise (Unmarshalling_error key)

	let get_named_string name args =
		XMLRPC.From.string (myassoc name args)
	
	let get_named_int name args =
		XMLRPC.From.int (myassoc name args)

	let process xml =
		let call,args = XMLRPC.From.methodCall xml in
		let args = try XMLRPC.From.structure (List.hd args) with _ -> [] in
		let response = 
			try match call with
			| "initialise" -> 
				let address = get_named_string "address" args in
				let port = get_named_int "port" args in
				let edition = get_named_string "edition" args in
				let response = match (V.initialise address port edition) with
				| l, d -> XMLRPC.To.structure 
					["license", XMLRPC.To.string l; "days_to_expire", XMLRPC.To.int d] in
				XMLRPC.Success [response]
			| "shutdown" ->
				let response = XMLRPC.To.boolean (V.shutdown ()) in
				XMLRPC.Success [response]
			| "reopen-logs" ->
				let response = XMLRPC.To.boolean (V.reopen_logs ()) in
				XMLRPC.Success [response]
			| x -> XMLRPC.Fault (Int32.of_int 0, "unknown RPC: " ^ x)
			with e ->
				log_backtrace ();
				XMLRPC.Failure ("INTERNAL_ERROR",[Printexc.to_string e])
		in
		XMLRPC.To.methodResponse response
end

