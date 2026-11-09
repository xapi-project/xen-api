(*
 * Copyright (C) 2026 Vates.
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

(** Messages recording the lifecycle of a VM (VM_STARTED, VM_MIGRATED...) *)

(** The VM lifecycle events for which a message is created *)
type event =
  | Started of {host: API.ref_host}
  | Resumed of {host: API.ref_host}
  | Paused
  | Unpaused
  | Suspended
  | Shutdown of {forced: bool}
  | Rebooted of {forced: bool}
  | Migrated of {source: API.ref_host; destination: API.ref_host}
  | Snapshotted of {snapshot_name: string}
  | Checkpointed of {snapshot_name: string}
  | Snapshot_reverted of {snapshot: API.ref_VM}
  | Cloned of {clone: API.ref_VM}

val body : __context:Context.t -> vm:API.ref_VM -> event -> string
(** The text of the message for an event of [vm] *)

val create : __context:Context.t -> vm:API.ref_VM -> event -> unit
(** Create the message for an event of [vm]. Failing to compute the
    message body raises an exception; failing to create the message
    itself is ignored. *)
