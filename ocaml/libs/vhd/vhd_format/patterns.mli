(*
 * Copyright (C) 2011-2013 Citrix Inc
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

(** Interesting set of I/O patterns for testing a vhd implementation *)

(** Interesting vhd virtual sizes *)
val sizes: int64 list

(** Places within an array (either a sector bitmap or BAT) *)
type choice =
  | First        (** edge case: first entry *)
  | Last         (** edge case: last entry *)

(** Position to read or write in a vhd *)
type position = {
  block: choice;
  sector: choice;
}

(** Individual step *)
type operation =
  | Create of int64
    (** Create a vhd of a given size; open file for I/O *)

  | Snapshot
    (** Snapshot current file; open new file for I/O *)

  | Write of (position * string)
    (** Write copies of a given string over a specific sector *)

(** A program is a linear sequence of operations, like a finite
    execution trace. *)
type program = operation list

val string_of_program: program -> string
(** A short string useful as a test label *)

val descr_of_program: program -> string list
(** Printable program "listing" *)

val programs: program list
(** All definied "interesting" programs *)
