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
module String : sig
  val isspace : char -> bool
  (** True if the character is whitespace *)

  val escaped : ?rules:(char * string) list -> string -> string
  (** Backward-compatible string escaping, defaulting to the built-in
      	OCaml string escaping but allowing an arbitrary mapping from characters
      	to strings. *)

  val split_f : (char -> bool) -> string -> string list
  (** Take a predicate and a string, return a list of strings separated by
      runs of characters where the predicate was true. Avoid if possible, it's
      very costly to execute. *)

  val split : limit:int -> char -> string -> string list
  (** split a string on a single char *)

  val rtrim : string -> string
  (** FIXME document me|remove me if similar to strip *)

  val has_substr : string -> string -> bool
  (** True if sub is a substr of str *)

  val replace : string -> string -> string -> string
  (** replace all [f] substring in [s] by [t] *)

  val filter_chars : string -> (char -> bool) -> string
  (** filter chars from a string *)

  val map_unlikely : string -> (char -> string option) -> string
  (** map a string trying to fill the buffer by chunk *)

  val sub_to_end : string -> int -> string
  (** a substring from the specified position to the end of the string *)
end
