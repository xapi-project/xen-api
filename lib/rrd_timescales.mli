(*
 * Copyright (C) 2015 Citrix Systems Inc.
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
(*
 * Timescales: this allows an RRD server to advertise which Timescales
 * are available, to avoid clients having to already know or guess.
 *)


type t = {
	name: string;
	num_intervals: int;
	interval_in_steps: int;
}

val make: name:string -> num_intervals:int -> interval_in_steps:int -> unit -> t

val name_of: t -> string

val to_span: t -> int
(** Total length of time covered by the archive *)

val interval_to_span: t -> int
(** Length of time in one interval (clients requesting updates should poll at
    most every interval) *)

val to_json: t list -> string

val of_json: string -> t list
