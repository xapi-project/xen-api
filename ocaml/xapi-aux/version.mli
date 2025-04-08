(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

(** a version, derived from a string representation - see below *)
type t

(** A version string violates the supported syntax *)
exception Format of string

val of_string : string -> t
(** Parse a version; may raise [Format]. A version is a sequence of
    unsigned integers separated by a dot; for axample "1.2.3" is a legal
    version. Must have at least one component. Examples:
    - 3
    - 3.10
    - 3.10.4
    - 3.10.4.0.0
    - 3.10.4.0.1
    - 0
    - 0.2
    *)

val to_string : t -> string
(** represent a version as a string *)

val compare : t -> t -> int
(** Total order over versions; yields one of -1, 0, 1 as by convention.
  - 1.2.3 = 1.2.3.0 
  - 1.10.2 > 1.9.1
  - 0.1.0.0 = 0.1
  *)

(* version equality relations *)
val eq : t -> t -> bool

val ge : t -> t -> bool

val gt : t -> t -> bool

val le : t -> t -> bool

val lt : t -> t -> bool

val ne : t -> t -> bool

(* Validate the format of a version string *)
val is_valid : string -> bool

(* Operations over version strings for convenience. Each function may
   raise [Format] *)
module String : sig
  val compare : string -> string -> int

  val ne : string -> string -> bool

  val eq : string -> string -> bool

  val le : string -> string -> bool

  val ge : string -> string -> bool

  val gt : string -> string -> bool

  val lt : string -> string -> bool
end
