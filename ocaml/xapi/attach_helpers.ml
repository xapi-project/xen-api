(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Stdext.Pervasiveext
open Client

module D = Debug.Make(struct let name="xapi" end)
open D

let timeout = 300. (* 5 minutes, should never take this long *)

(** Attempt an unplug, and if it fails because the device is in use, wait for it to
    detach by polling the currently-attached field. *)
let safe_unplug rpc session_id self =
  try
    Client.VBD.unplug rpc session_id self
  with
  | Api_errors.Server_error(error, _) when error = Api_errors.device_already_detached ->
    debug "safe_unplug caught DEVICE_ALREADY_DETACHED: this is safe to ignore"
  | Api_errors.Server_error(error, _) as e when error = Api_errors.device_detach_rejected ->
    debug "safe_unplug caught DEVICE_DETACH_REJECTED: polling the currently_attached flag of the VBD";
    let start = Unix.gettimeofday () in
    let unplugged = ref false in
    while not(!unplugged) && (Unix.gettimeofday () -. start < timeout) do
      Thread.delay 5.;
      unplugged := not(Client.VBD.get_currently_attached rpc session_id self)
    done;
    if not(!unplugged) then begin
      debug "Timeout waiting for dom0 device to be unplugged";
      raise e
    end

(** For a VBD attached to a control domain, it may correspond to a running task
    	(if so the task will be linked via an other_config key) or it may be a qemu
    	frontend (if so it will be linked to another frontend) *)
let has_vbd_leaked __context vbd =
  let other_config = Db.VBD.get_other_config ~__context ~self:vbd in
  let device = Db.VBD.get_device ~__context ~self:vbd in
  let has_task = List.mem_assoc Xapi_globs.vbd_task_key other_config in
  let has_related = List.mem_assoc Xapi_globs.related_to_key other_config in
  if not has_task && (not has_related)
  then (info "Ignoring orphaned disk attached to control domain (device = %s)" device; false)
  else begin
    let has_valid_task = has_task && (
        let task_id = Ref.of_string (List.assoc Xapi_globs.vbd_task_key other_config) in
        (* check if the task record still exists and is pending *)
        try
          let status = Db.Task.get_status ~__context ~self:task_id in
          List.mem status [ `pending; `cancelling ] (* pending and cancelling => not leaked *)
        with _ -> false (* task record gone *)
      ) in
    let has_valid_related = has_related && (
        let related = Ref.of_string (List.assoc Xapi_globs.related_to_key other_config) in
        (* check if the VBD still exists and is currently_attached *)
        try
          Db.VBD.get_currently_attached ~__context ~self:related
        with _ -> false (* VBD record gone *)
      ) in
    (* leaked if neither of the two keys are still valid *)
    not has_valid_task && (not has_valid_related)
  end

(** Execute a function with a list of VBDs after attaching a bunch of VDIs to an vm *)
let with_vbds rpc session_id __context vm vdis mode f =
  let task_id = Context.get_task_id __context in
  let vbds = ref [] in
  finally
    (fun () ->
       List.iter (fun vdi ->
           let vbd = Client.VBD.create ~rpc ~session_id ~vM:vm ~empty:false ~vDI:vdi
               ~userdevice:"autodetect" ~bootable:false ~mode ~_type:`Disk ~unpluggable:true
               ~qos_algorithm_type:"" ~qos_algorithm_params:[]
               ~other_config:[ Xapi_globs.vbd_task_key, Ref.string_of task_id ] in
           (* sanity-check *)
           if has_vbd_leaked __context vbd
           then error "Attach_helpers.with_vbds new VBD has leaked: %s" (Ref.string_of vbd);

           let vbd_uuid = Client.VBD.get_uuid ~rpc ~session_id ~self:vbd in
           let uuid = Client.VM.get_uuid ~rpc ~session_id ~self:vm in
           debug "created VBD (uuid %s); attempting to hotplug to VM (uuid: %s)" vbd_uuid uuid;
           vbds := vbd :: !vbds;
           Client.VBD.plug rpc session_id vbd
         ) vdis;
       vbds := List.rev !vbds;
       f !vbds)
    (fun () ->
       (* Use a new session here to cover the case where the session has become invalid *)
       Helpers.call_api_functions ~__context (fun rpc session_id ->
           List.iter (Helpers.log_exn_continue "unplugging disk from VM"
                        (fun self -> safe_unplug rpc session_id self)) !vbds;
           List.iter (Helpers.log_exn_continue "destroying VBD on VM"
                        (fun self -> Client.VBD.destroy rpc session_id self)) !vbds))

(** Separates the implementations of the given backend returned from
    the VDI.attach2 SMAPIv2 call based on their type *)
let implementations_of_backend backend =
  let open Storage_interface in
  List.fold_left
    (fun (xendisks, blockdevices, files, nbds) implementation ->
       match implementation with
       | XenDisk xendisk -> (xendisk::xendisks, blockdevices, files, nbds)
       | BlockDevice blockdevice -> (xendisks, blockdevice::blockdevices, files, nbds)
       | File file -> (xendisks, blockdevices, file::files, nbds)
       | Nbd nbd -> (xendisks, blockdevices, files, nbd::nbds)
    )
    ([], [], [], [])
    backend.implementations

(** Extracts the UNIX domain socket path and the export name from the NBD URI in
    the NBD information returned from the VDI.attach2 SMAPIv2 call.
    This has the format nbd:unix:<domain-socket>:exportname=<name> *)
let parse_nbd_uri nbd =
  let Storage_interface.{ uri } = nbd in
  let fail () =
    raise Api_errors.(Server_error (Api_errors.internal_error, ["Unexpected NBD URI returned from the storage backend: " ^ uri]))
  in
  match String.split_on_char ':' uri with
  | ["nbd"; "unix"; socket; exportname] -> begin
      let prefix = "exportname=" in
      match Astring.String.cuts ~empty:false ~sep:prefix exportname with
      | [exportname] ->
        (socket, exportname)
      | _ -> fail ()
    end
  | _ -> fail ()
