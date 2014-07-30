(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

module Common : functor (N : (sig val name : string end)) -> sig
	val wait_until_next_reading :
		?neg_shift:float ->
		protocol:Rrd_interface.plugin_protocol ->
		unit

	val now : unit -> int64

	val cut : string -> string list

	val exec_cmd : cmdstring:string -> f:(string -> 'a option) -> 'a list

	val list_directory_unsafe : string -> string list

	val list_directory_entries_unsafe : string -> string list

	val initialise : unit -> unit

	val main_loop :
		neg_shift:float ->
		protocol:Rrd_interface.plugin_protocol ->
		dss_f:(unit -> (Rrd.ds_owner * Ds.ds) list) ->
		unit
end
