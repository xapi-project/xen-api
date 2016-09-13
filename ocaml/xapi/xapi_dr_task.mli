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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** * Introduce SR records into the database based on a probe result.
    * Create a PBD for each SR for each host.
    * Plug all the PBDs.
    * Set the SR's introduced_by field to the returned DR_task.

    If anything goes wrong, unplug all PBDs which were created, forget the SRs,
    and re-raise the error. *)
val create : __context:Context.t ->
  _type:string ->
  device_config:(string * string) list ->
  whitelist:string list ->
  API.ref_DR_task

(** * Unplug all PBDs for each SR associated with the DR_task.
    * Forget each SR associated with the DR_task.
    * Destroy the DR_task. *)
val destroy : __context:Context.t -> self:API.ref_DR_task -> unit
