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

(** Bodies of the messages recording the lifecycle of a VM, one function
    per operation creating such a message *)

val snapshot_body :
  __context:Context.t -> vm:API.ref_VM -> new_name:string -> string

val checkpoint_body :
  __context:Context.t -> vm:API.ref_VM -> new_name:string -> string

val start_body :
  __context:Context.t -> vm:API.ref_VM -> host:API.ref_host -> string

val start_on_body :
  __context:Context.t -> vm:API.ref_VM -> host:API.ref_host -> string

val pause_body : __context:Context.t -> vm:API.ref_VM -> string

val unpause_body : __context:Context.t -> vm:API.ref_VM -> string

val clean_shutdown_body : __context:Context.t -> vm:API.ref_VM -> string

val shutdown_body : __context:Context.t -> vm:API.ref_VM -> string

val clean_reboot_body : __context:Context.t -> vm:API.ref_VM -> string

val hard_shutdown_body : __context:Context.t -> vm:API.ref_VM -> string

val hard_reboot_body : __context:Context.t -> vm:API.ref_VM -> string

val suspend_body : __context:Context.t -> vm:API.ref_VM -> string

val revert_body :
  __context:Context.t -> vm:API.ref_VM -> snapshot:API.ref_VM -> string

val csvm_body :
  __context:Context.t -> vm:API.ref_VM -> result:API.ref_VM -> string

val resume_body :
  __context:Context.t -> vm:API.ref_VM -> host:API.ref_host -> string

val resume_on_body :
  __context:Context.t -> vm:API.ref_VM -> host:API.ref_host -> string

val pool_migrate_body :
     __context:Context.t
  -> vm:API.ref_VM
  -> source_host:API.ref_host
  -> host:API.ref_host
  -> string

val migrate_send_body :
     __context:Context.t
  -> vm:API.ref_VM
  -> source_host:API.ref_host
  -> host:API.ref_host
  -> string
