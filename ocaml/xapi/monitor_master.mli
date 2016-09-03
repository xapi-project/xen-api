(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

(** Pushing object properties to database.
 * @group Property Monitoring
*)

(** This module implements the saving of properties for various objects (e.g.
 * VIFs, CPUs) into xapi's database. It is also used to regularly update
 * some simple configuration from the master. *)

(** Used on a regular interval to update rrdd's use_min_max and xenops'
 * pass_through_pif_carrier. *)
val update_configuration_from_master : unit -> unit

(** A function to write information about PIFs to xapi's database. *)
val update_pifs : __context:Context.t -> 'a Ref.t -> Monitor_types.pif list -> unit
