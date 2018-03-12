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

(** [update_vgpu_metadata] updates the vGPU compatibility metadata of
 * all vGPUs associated with [vm].  *)
val update_vgpu_metadata
  : __context:Context.t
  -> vm:API.ref_VM
  -> unit

(** [clear_vgpu_metadata] removes the compatibility metadata from all
 * VMs associated with [vm] *)
val clear_vgpu_metadata
  : __context:Context.t
  -> vm:API.ref_VM
  -> unit

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

  (** [is_nvidia] is true, if [vgpu] is an NVIDIA vGPU *)
  val is_nvidia
    : __context:Context.t
    -> vgpu:API.ref_VGPU    (** valid reference *)
    -> bool

  (** Obtain opaque compatibility metadata for a vGPU and return
   *  [(key, metadata)] association list. The metadata is a base-64
   *  encoded blob that gets passed to functions checking compatibility;
   *  the key is fixed to [key]. This function must be called on the
   *  host where the vGPU is assigned to a pGPU.
  *)
  val get_vgpu_compatibility_metadata
    : __context:Context.t
    -> vgpu:API.ref_VGPU      (** must refer to NVIDIA vGPU *)
    -> (string * string) list

  (** Predicate [vgpu_pgpu_are_compatible] checks that vGPU and pGPU are
   * comatible according to their abstract compatibility metadata. This
   * code can run on any host. If no vGPU or pGPU metadata is
   * available, compatibility is assumed.
  *)
  val vgpu_pgpu_are_compatible
    : __context:Context.t
    -> vgpu:API.ref_VGPU
    -> pgpu:API.ref_PGPU
    -> bool

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
