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

(** Library to simplify writing an rrdd plugin. *)

module Utils : sig
	(** Utility functions useful for rrdd plugins. *)

	val now : unit -> int64
	(** Return the current unix epoch as an int64. *)

	val cut : string -> string list
	(** Split a string into a list of strings as separated by spaces and/or
	    tabs. *)

	val list_directory_unsafe : string -> string list
	(** List the contents of a directory, including . and .. *)

	val list_directory_entries_unsafe : string -> string list
	(** List the contents of a directory, not including . and .. *)
end

type target =
	| Local
	(** Specifies that we will be reporting data to an rrdd process in the same
	    domain as this process. *)
	| Interdomain of (int * int)
	(** [Interdomain (domid, pages)] specifies that we will be reporting data to
	    an rrdd process in domain [domid], and we will be sharing [pages] with
	    this domain. *)
(** Specify how the data we are collecting will be reported. *)

module Common : functor (N : (sig val name : string end)) -> sig
	(** Functions used for communication with rrdd and other processes. *)

	val exec_cmd : cmdstring:string -> f:(string -> 'a option) -> 'a list
	(** Execute the command [~cmd] with args [~args], apply f on each of
	    the lines that cmd output on stdout, and returns a list of
	    resulting values if f returns Some v *)

	val initialise : unit -> unit
	(** Utility function for daemons whose sole purpose is to report data to rrdd.
	    This will set up signal handlers, as well as daemonising and writing a pid
	    file if specified on the CLI.

	    Processes which need to use initialise should call it  before spawning any
	    threads.

	    Processes which have tasks beyond reporting data to rrdd should probably
	    not call this function. *)

	val main_loop :
		neg_shift:float ->
		target:target ->
		protocol:Rrd_interface.plugin_protocol ->
		dss_f:(unit -> (Rrd.ds_owner * Ds.ds) list) ->
		unit
	(** Begin the main loop.
	    {ul
	    {- [neg_shift] is the amount of time before rrdd collects data that we
	       should report our data.}
	    {- [target] specifies the transport via which data will be reported to
	       rrdd.}
	    {- [protocol] specifies the protocol used to tramsit the data.}
	    {- [dds_f ()] will generate the list of datasources to be reported.}} *)
end
