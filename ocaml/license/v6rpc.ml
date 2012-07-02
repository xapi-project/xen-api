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

module D = Debug.Debugger(struct let name="v6rpc" end)
open D

type apply_edition_in = {
	edition_in: string;
	additional_in: (string * string) list;
} with rpc

type apply_edition_out = {
	edition_out: string;
	features_out: Features.feature list;
	additional_out: (string * string) list;
} with rpc

type names = string * string * string * int with rpc
type get_editions_out = {
	editions: names list;
} with rpc

module type V6api = sig
	(* dbg_str -> edition -> additional_params -> enabled_features, additional_params *)
	val apply_edition : string -> string -> (string * string) list ->
		string * Features.feature list * (string * string) list
	(* dbg_str -> list of editions (name, marketing name, short name) *)
	val get_editions : string -> (string * string * string * int) list
	(* dbg_str -> result *)
	val get_version : string -> string
	(* () -> version *)
	val reopen_logs : unit -> bool
end

module V6process = functor(V: V6api) -> struct
	let process call =
		let dbg = "startup" in
		let response =
			try match call.Rpc.name with
			| "apply_edition" -> 
				let arg_rpc = match call.Rpc.params with [a] -> a | _ -> raise (V6errors.Error ("unmarchalling_error", [])) in
				let arg = apply_edition_in_of_rpc arg_rpc in
				let edition, features, additional_params =
					V.apply_edition dbg arg.edition_in arg.additional_in in
				let response = rpc_of_apply_edition_out
					{edition_out = edition; features_out = features; additional_out = additional_params} in 
				Rpc.success response
			| "get_editions" ->
				let response = rpc_of_get_editions_out {editions = V.get_editions dbg} in
				Rpc.success response
			| "get_version" ->
				let response = Rpc.rpc_of_string (V.get_version dbg) in
				Rpc.success response
			| "reopen-logs" ->
				let response = Rpc.rpc_of_bool (V.reopen_logs ()) in
				Rpc.success response
			| x -> failwith ("unknown RPC: " ^ x)
			with 
			| V6errors.Error e as exn ->
				error "%s" (V6errors.to_string exn);
				log_backtrace ();
				Rpc.failure (V6errors.rpc_of_error e)
			| e ->
				let e = Printexc.to_string e in
				error "Error: %s" e;
				log_backtrace ();
				Rpc.failure (V6errors.rpc_of_error (V6errors.v6d_failure, [e]))
		in
		response
end

