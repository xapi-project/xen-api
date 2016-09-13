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

(** Retrieves a value *)
val get: string -> string

(** [get_with_default key default] returns the value associated with [key],
    	or [default] if the key is missing. *)
val get_with_default: string -> string -> string

(** Inserts a value into the database, only returns when the insertion has
    been persisted. *)
val put: string -> string -> unit

(** Insert a set of values into the database, only returns when the insertions
    have been persisted. *)
val putv: (string * string) list -> unit

(** Delete a key from the local database *)
val del: string -> unit
