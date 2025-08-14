(*
 * Copyright (c) Cloud Software Group
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

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

val with_activated_disk :
     dbg:string
  -> sr:Storage_interface.sr
  -> vdi:Storage_interface.vdi option
  -> dp:string
  -> vm:Storage_interface.vm
  -> (string option -> 'a)
  -> 'a

val tapdisk_of_attach_info : Storage_interface.backend -> Tapctl.tapdev option

module Copy : sig
  val copy_into_vdi :
       task:Storage_task.Storage_task.task_handle
    -> dbg:string
    -> sr:Storage_interface.sr
    -> vdi:Storage_interface.vdi
    -> vm:Storage_interface.vm
    -> url:string
    -> dest:Storage_interface.sr
    -> dest_vdi:Storage_interface.vdi
    -> verify_dest:bool
    -> Storage_interface.async_result_t option

  val copy_into_sr :
       task:Storage_task.Storage_task.task_handle
    -> dbg:string
    -> sr:Storage_interface.sr
    -> vdi:Storage_interface.vdi
    -> vm:Storage_interface.vm
    -> url:string
    -> dest:Storage_interface.sr
    -> verify_dest:bool
    -> Storage_interface.async_result_t option
end

val mirror_pass_fds :
     dbg:string
  -> dp:string
  -> sr:Storage_interface.sr
  -> vdi:Storage_interface.vdi
  -> mirror_vm:Storage_interface.vm
  -> live_vm:Storage_interface.vm
  -> mirror_id:string
  -> url:string
  -> dest_sr:Storage_interface.sr
  -> verify_dest:bool
  -> remote_mirror:Storage_interface.Mirror.mirror_receive_result_vhd_t
  -> Tapctl.tapdev

val mirror_snapshot :
     dbg:string
  -> sr:Storage_interface.sr
  -> dp:string
  -> mirror_id:string
  -> local_vdi:Storage_interface.vdi_info
  -> image_format:string
  -> Storage_interface.vdi_info

val mirror_checker : string -> Tapctl.tapdev -> unit

val mirror_copy :
     task:Storage_task.Storage_task.task_handle
  -> dbg:string
  -> sr:Storage_interface.sr
  -> snapshot:Storage_interface.vdi_info
  -> copy_vm:Storage_interface.vm
  -> url:string
  -> dest_sr:Storage_interface.sr
  -> remote_mirror:Storage_interface.Mirror.mirror_receive_result_vhd_t
  -> verify_dest:bool
  -> Storage_interface.vdi_info

module MIRROR : SMAPIv2_MIRROR
