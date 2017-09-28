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

(** Module that handles assigning vUSBs to VMs.
 * @group Virtual-Machine Management
*)


(** Assign a list of USB devices to a VM for USB passthrough *)
val create_vusbs :
  __context:Context.t ->
  (API.ref_host) ->(API.ref_VM * API.vM_t) -> bool -> unit
