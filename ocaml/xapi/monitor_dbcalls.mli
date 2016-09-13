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

(** Gathering of fresh properties, and detecting property changes.
 * @group Property Monitoring
*)

(** This module triggers updates of xapi's database according to the PIF, bond,
 * and memory information gathered on a regular interval about the VMs and the
 * host. The module uses an in-memory cache for all information gathered, only
 * triggering pushes for changes detected. If it detects a change for an
 * object, and that object is not present in the database, then the push will
 * have no effect; furthermore, the push will not be retried. One can
 * explicitly clear a part or the whole cache in order to force pushing of
 * fresh information into the database.
*)

(** The function to be executed as a stand-alone thread as xapi starts. This
 * thread is responsible for continually gathering fresh properties and
 * pushing changes to xapi's database. *)
val monitor_dbcall_thread : unit -> unit
