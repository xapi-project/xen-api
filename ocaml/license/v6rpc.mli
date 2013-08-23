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

(** RPC definition of the licensing daemon *)

(** The RPC interface of the licensing daemon *)
module type V6api =
	sig
		(*  dbg_str -> edition -> additional_params -> enabled_features, additional_params *)
		val apply_edition : string -> string -> (string * string) list ->
			string * Features.feature list * (string * string) list
		(* dbg_str -> list of editions *)
		val get_editions : string -> (string * string * string * int) list
		(* dbg_str -> result *)
		val get_version : string -> string
		(* () -> version *)
		val reopen_logs : unit -> bool
	end  

(** RPC handler module *)
module V6process : functor (V : V6api) ->
	sig
		(** Process an RPC call *)
		val process : Rpc.call -> Rpc.response
	end

(** {2 Marshaling functions} *)

(** Definition of [apply_edition] RPC *)
type apply_edition_in = {
	edition_in: string; (** The requested edition *)
	additional_in: (string * string) list; (** Additional parameters *)
}

(** Convert RPC into {!apply_edition_in} structure *)
val apply_edition_in_of_rpc : Rpc.t -> apply_edition_in

(** Convert {!apply_edition_in} structure into RPC *)
val rpc_of_apply_edition_in : apply_edition_in -> Rpc.t

(** Return type of the [apply_edition] RPC *)
type apply_edition_out = {
	edition_out: string; (** The edition that was applied *)
	features_out: Features.feature list; (** The features that are now enabled *)
	additional_out: (string * string) list; (** Additional parameters *)
}

(** Convert RPC into {!apply_edition_out} structure *)
val apply_edition_out_of_rpc : Rpc.t -> apply_edition_out

(** Convert {!apply_edition_out} structure into RPC *)
val rpc_of_apply_edition_out : apply_edition_out -> Rpc.t

(** Format of the editions list returns by the [get_editions] RPC:
    - Name of the edition;
    - Long name of the edition;
    - Abbreviation of the edition name;
    - Edition order number.
 *)
type names = string * string * string * int

(** Return type of the [get_editions] RPC *)
type get_editions_out = {
	editions: names list; (** List of all available editions *)
}

(** Convert RPC into {!get_editions_out} structure *)
val get_editions_out_of_rpc : Rpc.t -> get_editions_out

(** Convert {!get_editions_out} structure into RPC *)
val rpc_of_get_editions_out : get_editions_out -> Rpc.t

