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
(** Produce a string name -> string mean, standard deviation summary for each population *)
val summarise : unit -> (string * string) list

(** Time the given function and attribute the result to the named population *)
val time_this : string -> (unit -> 'a) -> 'a

type dbcallty = Read | Write | Create | Drop
val log_db_call : string option -> string -> dbcallty -> unit
val summarise_db_calls : unit -> (string list * string list * string list * string list * (string * ((string * string) list)) list * (int * ((string * string) list)) list)

val log_stats : bool ref
