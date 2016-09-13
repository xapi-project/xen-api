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
(** Periodic scheduler for background tasks. *)

(** Timer type. *)
type func_ty =
  | OneShot				(** Fire just once *)
  | Periodic of float		(** Fire periodically with a given period in seconds *)

(** Start a new timer. *)
val add_to_queue :
  ?signal:bool -> string -> func_ty -> float -> (unit -> unit) -> unit

(** Remove a scheduled item by name *)
val remove_from_queue : string -> unit

(** The scheduler's main loop, started by {!Xapi} on start-up. *)
val loop : unit -> unit

