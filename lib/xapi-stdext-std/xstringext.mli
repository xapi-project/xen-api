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
  include module type of String

  val of_char : char -> string

  val rev_map : (char -> char) -> string -> string
  (** Map a string to a string, applying the given function in reverse
      order. *)

  val rev_iter : (char -> unit) -> string -> unit
  (** Iterate over the characters in a string in reverse order. *)

  val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
  (** Fold over the characters in a string. *)

  val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
  (** Iterate over the characters in a string in reverse order. *)

  val explode : string -> char list
  (** Split a string into a list of characters. *)

  val implode : char list -> string
  (** Concatenate a list of characters into a string. *)

  val endswith : string -> string -> bool
  (** True if string 'x' ends with suffix 'suffix' *)

  val startswith : string -> string -> bool
  (** True if string 'x' starts with prefix 'prefix' *)

  val isspace : char -> bool
  (** True if the character is whitespace *)

  val strip : (char -> bool) -> string -> string
  (** Removes all the characters from the ends of a string for which the predicate is true *)

  val escaped : ?rules:(char * string) list -> string -> string
  (** Backward-compatible string escaping, defaulting to the built-in
      	OCaml string escaping but allowing an arbitrary mapping from characters
      	to strings. *)

  val split_f : (char -> bool) -> string -> string list
  (** Take a predicate and a string, return a list of strings separated by
      	runs of characters where the predicate was true *)

  val split : ?limit:int -> char -> string -> string list
  (** split a string on a single char *)

  val rtrim : string -> string
  (** FIXME document me|remove me if similar to strip *)

  val has_substr : string -> string -> bool
  (** True if sub is a substr of str *)

  val find_all : string -> string -> int list
  (** find all occurences of needle in haystack and return all their respective index *)

  val replace : string -> string -> string -> string
  (** replace all [f] substring in [s] by [t] *)

  val filter_chars : string -> (char -> bool) -> string
  (** filter chars from a string *)

  val map_unlikely : string -> (char -> string option) -> string
  (** map a string trying to fill the buffer by chunk *)

  val sub_to_end : string -> int -> string
  (** a substring from the specified position to the end of the string *)

  val sub_before : char -> string -> string
  (** a substring from the start of the string to the first occurrence of a given character, excluding the character *)

  val sub_after : char -> string -> string
  (** a substring from  the first occurrence of a given character to the end of the string, excluding the character *)
end
