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

(** XML/RPC handler for the licensing daemon *)

(** The XML/RPC interface of the licensing daemon *)
module type V6api =
	sig
		val initialise : string -> int32 -> string -> string * int32
		val shutdown : unit -> bool
		val reopen_logs : unit -> bool
	end
  
(** XML/RPC handler *)
module V6process : functor (V : V6api) ->
	sig
		(** Process an XML/RPC call *)
		val process : XMLRPC.xmlrpc -> XMLRPC.xmlrpc
	end
