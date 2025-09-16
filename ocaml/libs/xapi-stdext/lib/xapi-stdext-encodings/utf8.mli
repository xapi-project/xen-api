(*
 * Copyright (c) Cloud Software Group, Inc.
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

val is_valid : string -> bool
(** true, if a string is a proper UTF-8 string *)

val longest_valid_prefix : string -> string
(** Deprecated. Longest prefix of a string that is proper UTF-8 *)

(* strings in XML are more restricted than UTF-8 in general. The must be
   valid UTF-8 and must not contain certain characters *)

module XML : sig
  val is_valid : string -> bool
  (** true, if a string is a proper UTF-8 string in XML *)

  val longest_valid_prefix : string -> string
  (** Deprecated. longest prefix of a string that is proper UTF-8.
      Better reject invalid UTF-8. *)
end
