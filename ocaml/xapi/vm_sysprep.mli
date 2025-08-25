(*
 * Copyright (c) Cloud Software Group, Inc.
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

(** error message that may be passed to API clients *)
type error =
  | API_not_enabled
  | Other of string
  | VM_CDR_not_found
  | VM_CDR_eject
  | VM_CDR_insert
  | VM_misses_feature
  | VM_not_running
  | VM_sysprep_timeout
  | XML_too_large

exception Sysprep of error

val on_startup : __context:Context.t -> unit
(** clean up on toolstart start up *)

val sysprep :
     __context:Context.t
  -> vm:API.ref_VM
  -> unattend:string
  -> timeout:float
  -> unit
(** Execute sysprep on [vm] using script [unattend]. This requires
    driver support from the VM and is checked. [unattend:string] must
    not exceed 32kb. Raised [Failure] that must be handled, *)
