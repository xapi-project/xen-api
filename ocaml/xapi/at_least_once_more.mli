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
(** Represents an idempotent background operation which needs to be re-executed when any of a number of external
    factors/ dependencies change. We attempt to minimise the number of times this background function is called. *)
type operation = unit -> unit

(** Instances of this type manage the background execution of the operation *)
type manager

(** Return a human-readable name for this background operation for debugging purposes *)
val name_of_t: manager -> string

(** Create a manager instance *)
val make: string -> operation -> manager

(** Signal the manager that some external factor has changed and therefore the background operation needs re-execution *)
val again: manager -> unit
