(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

(** Stop gpumon if it's running, perform f, then start gpumon if
  * no other threads which require gpumon to be stopped are running. *)
val with_gpumon_stopped :
  ?timeout:float ->
  (unit -> 'a) -> 'a

module Nvidia : sig
  (** The key used in the metadata assoc-list *)
  val key: string

  (** Fetch metadata about the PGPU from the driver, and return
   *  [(key, metadata)] where key is a fixed value and metadata
   *  is the base64 encoded opaque string of data from the graphics driver.
   *  IMPORTANT: This must be called on the host that has the GPU installed in it. *)
  val get_pgpu_compatibility_metadata:
    dbg: string ->
    pgpu_pci_address: string ->
    (string * string) list

  (** Check compatibility between a VM's vGPU(s) and another pGPU,
   *  and fail if they are not compatible. This function is assumed
   *  to run on the host where the VM is running. 
   *  The pgpu metadata is expected to be the encoded dump obtained
   *  from the Xapi database.
   *  The destination host is needed for the metadata of some of the errors. *)
  val assert_pgpu_is_compatible_with_vm:
    __context:Context.t ->
    vm:API.ref_VM ->
    vgpu:API.ref_VGPU ->
    dest_host:API.ref_host ->
    encoded_pgpu_metadata: string ->
    unit

end
