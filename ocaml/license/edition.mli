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
(** Module that controls product edition and feature mappings.
 * @group Licensing
 *)

(** Available editions *)
type edition =
	| Free (** Default Edition *)

(** Raised by {!of_string} if the given string does not map to an edition. *)
exception Undefined_edition of string

(** Convert a string to an {!edition}. *)
val of_string : string -> edition

(** Convert an {!edition} to a string. *)
val to_string : edition -> string

(** Convert an {!edition} to an abbreviated string. *)
val to_short_string : edition -> string

(** Convert an {!edition} to its marketing name. *)
val to_marketing_name : edition -> string

(** Get the list of {!feature}s enabled for a given {!edition}. *)
val to_features : edition -> Features.feature list

(** Provides a total order. *)
val to_int : edition -> int

(** Compare two editions for equality (used before pool join). *)
val equal : edition -> edition -> bool

(** Return the "least capable" edition (used to determine the pool edition). *)
val min : edition list -> edition

