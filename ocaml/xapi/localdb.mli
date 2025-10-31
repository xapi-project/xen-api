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
(** A map of key-value pairs stored locally. Data stored here can still be
    accessed even when the master connection is down. Writes are immediately
    flushed to disk. *)

(** Thrown when a particular named key could not be found. *)
exception Missing_key of string

val get : string -> string option
(** Retrieves a value *)

val get_exn : string -> string
(** Retrieves the value for the key, raises Missing_key when the key is not
    present *)

val get_bool : string -> bool option
(** Retrieves the value for the key, returns a value when it's found and is a
    valid boolean, otherwise is [None] *)

val get_int : string -> int option
(** Retrieves the value for the key, returns a value when it's found and is a
    valid int, otherwise is [None] *)

val get_of_string : (string -> 'a option) -> string -> 'a option
(** [get_of_string of_string key] retrieves the value for [key], and if it
    exists, processes it with [of_string], otherwise it's [None] *)

val put : string -> string -> unit
(** Inserts a value into the database, only returns when the insertion has
    been persisted. *)

val putv : (string * string) list -> unit
(** Insert a set of values into the database, only returns when the insertions
    have been persisted. *)

val del : string -> unit
(** Delete a key from the local database *)
