(*
 * Copyright (C) Citrix Systems Inc.
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

(** This module supports building a large command line (argument) vector
 * incrementally in a monad.
 *)

type argv
type 'a t      (** monad, holding current command line *)

(** basic functions to construct a [t] value. More below *)
val return: 'a -> 'a t
val bind:   'a t -> ('a -> 'b t) -> 'b t

(** turn a [t] value into something useful, containing the values
 * we are interested in. These can be accessed using [argv] and [fd_map].
 *)
val run:    'a t -> 'a * argv
val argv:   argv -> string list                     (** argument list *)
val fd_map: argv -> (string * Unix.file_descr) list (** file descriptors *)

(** functions to build argument vector incrementally *)

module Add: sig
  val arg: string -> unit t
  (** add single string to argv *)

  val many: string list -> unit t
  (** add list of strings to argv *)

  val each: ('a -> string list) -> 'a list -> unit t
  (** expand a list of values to strings, which are added to argv *)

  val fmt: ('a, unit, string, argv -> unit * argv) format4 -> 'a
  (** like [add] but takes printf-like arguments: ["%d-%d" 4 5] *)

  val optional: ('a -> string list) -> 'a option -> unit t
  (** [optional f arg] applies [f] to [Some arg] to produce arguments.
   * If [arg] is [None], no arguments are added *)

  val file_descr: string -> Unix.file_descr -> unit t
  (** add a file descriptor under a name. Forkexecd will replace the
   * name in the argument vector with the integer of the file descriptor
   * *)
end


