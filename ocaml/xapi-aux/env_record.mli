(*
 * Copyright (C) 2023 Cloud Software Group
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

(** A Unix environment variable is a pair of a name and a value, both of which 
  * are strings. This module helps to construct a complex environment value 
  * that follows specific conventions.
  *)

(** A value of type [t] represents a value. 
  * It is constructed recursively from OCaml values using functions like [str], 
  * [option], or [list]. An environment value of type [t] can be observed using 
  * [to_shell_string] or [to_string_array].
  *)
type t

val str : string -> t
(** Returns an environment variable value from a string.
  *)

val option : string option -> t
(** Returns an environment variable value from a string option.
  * If [None], returns an empty string value.
  *)

val list : ('a -> t) -> 'a list -> t
(** [list element lst] is the list [lst] with elements converted by [element] 
  * and separated by [,]. 
  *)

val pair : string * string -> t
(** [pair (key, value)] is [key=value] pair. *)

val to_shell_string : (string * t) list -> string
(** [to_shell_string env_vars] is [env_vars] quoted as needed for use in a shell 
  * script.
  *)

val to_string_array : t list -> string array
(** Transforms a list of environment variables to a string array of environment 
  * variables.*)
