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

type t [@@deriving sexp]
(** A backtrace from a particular thread. *)

val empty: t
(** An empty backtrace *)

val to_string_hum: t -> string
(** Produce a human-readable printable/loggable version of the
    backtrace. *)

(** {2 Handling exceptions without losing backtraces}
    Whenever a function raises an exception, the backtrace buffer is
    emptied. Therefore when we are handling an exception, we must
    stash away a copy of the backtrace buffer if there is any risk
    of us raising another (or even the same) exception) *)

val with_backtraces: (unit -> 'a) -> [ `Ok of 'a | `Error of (exn * t) ]
(** Allow backtraces to be recorded for this thread. All new threads
    must be wrapped in this for the backtrace tracking to work.
    It is acceptable to nest these wrappers; it will not affect the
    backtrace recording behaviour. *)

val is_important: exn -> unit
(** Declare that the backtrace is important for debugging and should be
    permanently associated with the exception. Call this function in
    an exception handler where you might need to re-raise the same
    exception at the end after performing some cleanup, which could
    clear the current backtrace buffer.*)

val get: exn -> t
(** Get a copy of the backtrace associated with [exn] *)

val add: exn -> t -> unit
(** Associate additional backtrace with an exception. This allows
    you to combine a backtrace from another process with your current
    backtrace. *)

val reraise: exn -> exn -> 'a
(** [reraise old new] associates the backtrace of [old] with [new]
    and throws [new]. Use this if you need to 'launder' an exception
    e.g. you may want to catch Not_found and throw a more descriptive
    exception instead without losing the backtrace. *)

val remove: exn -> t
(** Get a backtrace associated with [exn] and remove it from the tables.
    Use this when you want to print/log or otherwise record the final
    backtrace. *)

(** {2 Administrivia} *)

val set_my_name: string -> unit
(** Every backtrace line will include a name for this process. By default it
    will be the executable name, but it could also include the process ID
    and host. *)

(** {2 Interop with other languages}
    This allows backtraces from other languages (e.g. python) to be converted
    into OCaml-style backtraces. *)

module Interop: sig

  val of_json: string -> string -> t
  (** [of_json source_name json]: unmarshals a json-format backtrace from
      [source_name] *)
end

