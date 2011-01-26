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

type initialise_in = {
	address: string;
	port: int32;
	edition: string;
} with rpc

type initialise_out = {
	license: string;
	days_to_expire: int32;
} with rpc

type failure = string * (string list) with rpc
let response_of_failure code params =
	Rpc.failure (rpc_of_failure (code, params))
let response_of_fault code =
	Rpc.failure (rpc_of_failure ("Fault", [code]))

module type V6api = sig
	val initialise : string -> int32 -> string -> string * int32
	val shutdown : unit -> bool
	val reopen_logs : unit -> bool
end

module V6process = functor(V: V6api) -> struct
	let process call =
		let response =
			try match call.Rpc.name with
			| "initialise" -> 
				let arg_rpc = match call.Rpc.params with [a] -> a | _ -> raise (Unmarshalling_error "initialise") in
				let arg = initialise_in_of_rpc arg_rpc in
				let l,d = V.initialise arg.address arg.port arg.edition in
				let response = rpc_of_initialise_out { license = l; days_to_expire = d } in 
				Rpc.success response
			| "shutdown" ->
				let response = Rpc.rpc_of_bool (V.shutdown ()) in
				Rpc.success response
			| "reopen-logs" ->
				let response = Rpc.rpc_of_bool (V.reopen_logs ()) in
				Rpc.success response
			| x -> response_of_fault ("unknown RPC: " ^ x)
			with e ->
				log_backtrace ();
				response_of_failure "INTERNAL_ERROR" [Printexc.to_string e] in
		response
end

