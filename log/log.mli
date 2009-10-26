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

(** Logging utilities *)

type t

(** create a string representating the parameters of the logger *)
val to_string : t -> string

(** parse a string to a logger *)
val of_string : string -> t

(** try to reopen a logger *)
val reopen : t -> t

(** close a logger *)
val close : t -> unit

val gettimestring : unit -> string

(** {2 Builders} *)

type level = Debug | Info | Warn | Error
val get_level : t -> level

exception Unknown_level of string
val level_of_string : string -> level
val string_of_level : level -> string

(** open a syslog logger *)
val opensyslog : string -> level -> t

(** open a stderr logger *)
val openerr : level -> t

(** open a stdout logger *)
val openout : level -> t

(** open a stream logger - returning the output type *)
val openfile : string -> level -> t

(** open a nil logger *)
val opennil : unit -> t

(** open a string logger *)
val openstring : level -> t
val get_strings : t -> string list

(** {2 Raw output functions} *)

val output : t -> ?key:string -> ?extra:string -> level -> string -> unit
val output_and_return : t -> ?raw:bool -> ?key:string -> ?extra:string -> level -> string -> string

(** {2 Pretty output functions} *)

val debug : t -> ('a, unit, string, unit) format4 -> 'a
val info : t -> ('a, unit, string, unit) format4 -> 'a
val warn : t -> ('a, unit, string, unit) format4 -> 'a
val error : t -> ('a, unit, string, unit) format4 -> 'a
val log : t -> level -> ('a, unit, string, unit) format4 -> 'a

(** {2 Output validation} *)

val validate : string -> unit

(** {2 Concurrency} *)

(** TODO: It would be very nice to have a thread-free log module (ie. put the control outside that module).
	This mutex protects all the recorded outputs. *)
val mutex : Mutex.t

(** TODO: remove the global state (what happens if multiple log files are opened???) ! *)
val filesize : int ref

(*
type stream_type = Stderr | Stdout | File of string
type stream_log = {
  ty : stream_type;
  channel : out_channel option ref;
  mutex : Mutex.t;
}

val int_of_level : level -> int
val mkdir_safe : string -> Unix.file_perm -> unit
val mkdir_rec : string -> Unix.file_perm -> unit
val set : t -> level -> unit
val mutex : Mutex.t
*)
