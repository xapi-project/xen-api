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

open Printf
open Xenops_task
open Device_common
open Xenstore
open Cancel_utils
open Xenops_utils

module D = Debug.Make(struct let name = "hotplug" end)
open D


(** If we can't execute the losetup program (for example) *)
exception External_command_failure of string
(** Timeout waiting for the backend hotplug scripts *)
exception Device_timeout of device
(** Timeout waiting for the frontend hotplug scripts (in the case of dom0) *)
exception Frontend_device_timeout of device
(** An explicit failure waiting for the frontend hotplug scripts (in the case of dom0) *)
exception Frontend_device_error of string
(** Ff we can't find the file we're trying to losetup *)
exception Loopdev_error of string
(** we can't delete a loop device *)
exception Loopdev_delete_error
(** If all the loopback devices are busy *)
exception Loopdev_all_busy

(** Interface to synchronise with minimal hotplug scripts *)

(* Notes about hotplug:
   We do all the heavy lifting (allocating loopback devices, checking for sharing
   problems) but we still need to be able to synchronise with the kernel (definitely
   for unplug, not sure about plug) *)

(* Path in xenstore where we stuff our transient hotplug-related stuff *)
let get_hotplug_base domid =
  sprintf "%s/hotplug/%d" (get_private_path domid) domid
let get_hotplug_base_by_uuid uuid domid =
  sprintf "%s/hotplug/%d" (get_private_path_by_uuid uuid) domid
let get_hotplug_path (x: device) =
  sprintf "%s/%s/%d" (get_hotplug_base x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid
let get_hotplug_path_by_uuid uuid (x: device) =
  sprintf "%s/%s/%d" (get_hotplug_base_by_uuid uuid x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid

let path_written_by_hotplug_scripts ?uuid (x: device) = match x.backend.kind with
  | Vif ->
    (match uuid with
     | None -> get_hotplug_path x ^ "/hotplug"
     | Some uuid' -> get_hotplug_path_by_uuid uuid' x ^ "/hotplug")
  | Vbd _ | Qdisk ->
    sprintf "/local/domain/%d/backend/%s/%d/%d/hotplug-status"
      x.backend.domid (string_of_kind x.backend.kind) x.frontend.domid x.frontend.devid
  | k -> failwith (Printf.sprintf "No xenstore interface for this kind of device: %s" (string_of_kind k))

let vif_disconnect_path (x: device) =
  sprintf "/local/domain/%d/device/vif/%d/disconnect" x.frontend.domid x.frontend.devid

let hotplugged ~xs vm (x: device) =
  let path = path_written_by_hotplug_scripts ?uuid:(Uuidm.of_string vm) x in
  debug "Checking to see whether %s" path;
  try ignore(xs.Xs.read path); true with Xs_protocol.Enoent _ -> false

(* The path in xenstore written to by the frontend hotplug scripts *)
let frontend_status_node (x: device) = 
  sprintf "%s/frontend/%s/%d/hotplug" (get_private_path x.frontend.domid) (string_of_kind x.frontend.kind) x.frontend.devid

(* CA-15605: node written to by tapdisk to report an error (eg opening .vhd files). *)
let tapdisk_error_node ~xs (x: device) = 
  sprintf "%s/backend/%s/%d/%d/tapdisk-error" (xs.Xs.getdomainpath x.backend.domid) (string_of_kind x.backend.kind) x.frontend.domid x.frontend.devid

(* CA-39745: node written to by blkback to report an error (eg opening an empty CDROM drive) *)
let blkback_error_node ~xs (x: device) = 
  sprintf "%s/error/backend/vbd/%d/%d/error" (xs.Xs.getdomainpath x.backend.domid) x.backend.domid x.frontend.devid

(* Poll a device to see whether it is instantaneously "online" where "online" means
   "currently-attached" in the database. The event thread AND the startup code call
   this function to resynchronise the state of the world with the database. 
   If we're called for a VIF backend we rely solely on the dom0 backend hotplug scripts.
   If we're called for a VBD (blkback or blktap) then we first check to see if the 
   shutdown has been requested but not completed: if so we consider the device online and
   expect the shutdown-done event to come later. If no shutdown-request node exists
   (ie not an API-initiated hotunplug; this is start of day) then we check the state 
   of the backend hotplug scripts. *)
let device_is_online ~xs vm (x: device) = 
  let backend_shutdown () = try ignore(xs.Xs.read (backend_shutdown_done_path_of_device ~xs x)); true with Xs_protocol.Enoent _ -> false 
  and backend_request () = try ignore(xs.Xs.read (backend_shutdown_request_path_of_device ~xs x)); true with Xs_protocol.Enoent _ -> false in

  match x.backend.kind with
  | Pci | Vfs | Vkbd | Vfb -> assert false (* PCI backend doesn't create online node *)
  | Vif -> hotplugged ~xs vm x
  | ( Vbd _ | Tap | Qdisk ) -> 
    if backend_request () 
    then not(backend_shutdown ())
    else hotplugged ~xs vm x

let wait_for_plug (task: Xenops_task.task_handle) ~xs (x: device) = 
  debug "Hotplug.wait_for_plug: %s" (string_of_device x);
  try
    Stats.time_this "udev backend add event" 
      (fun () ->
         let path = path_written_by_hotplug_scripts x in
         let (_: bool) = cancellable_watch (Device x) [ Watch.map (fun _ -> ()) (Watch.value_to_appear path) ] [] task ~xs ~timeout:!Xenopsd.hotplug_timeout () in
         ()
      );
    debug "Synchronised ok with hotplug script: %s" (string_of_device x)
  with Watch.Timeout _ ->
    raise (Device_timeout x)

let wait_for_unplug (task: Xenops_task.task_handle) ~xs (x: device) =
  debug "Hotplug.wait_for_unplug: %s" (string_of_device x);
  try
    Stats.time_this "udev backend remove event" 
      (fun () ->
         let path = path_written_by_hotplug_scripts x in
         let (_: bool) = cancellable_watch (Device x) [ Watch.map (fun _ -> ()) (Watch.key_to_disappear path) ] [] task ~xs ~timeout:!Xenopsd.hotplug_timeout () in
         ()
      );
    debug "Synchronised ok with hotplug script: %s" (string_of_device x)
  with Watch.Timeout _ ->
    raise (Device_timeout x)

(* If we're running the hotplug scripts ourselves then we must wait
   for the VIF device to actually be created. libxl waits until the
   backend gets into state 2 (InitWait): see libxl__wait_device_connection *)
let wait_for_connect (task: Xenops_task.task_handle) ~xs (x: device) =
  debug "Hotplug.wait_for_connect: %s" (string_of_device x);
  try
    Stats.time_this "device backend in state 2" 
      (fun () ->
         let path = backend_state_path_of_device ~xs x in
         let (_: bool) = cancellable_watch (Device x) [ Watch.map (fun _ -> ()) (Watch.value_to_become path (Xenbus_utils.(string_of InitWait))) ] [] task ~xs ~timeout:!Xenopsd.hotplug_timeout () in
         ()
      );
    debug "Synchronised ok with device backend: %s" (string_of_device x)
  with Watch.Timeout _ ->
    raise (Device_timeout x)

let losetup = "/sbin/losetup"

(* Create a /dev/loop* device attached to file 'path' *)
let mount_loopdev_file readonly path =
  (* 1. Check to see if the path actually looks ok *)
  begin
    try
      Unix.access path [ Unix.R_OK; Unix.F_OK ]
    with _ ->
      raise (Loopdev_error path)
  end;
  (* 2. Inefficiently work through the /dev/loop* devices, attempting to g rab one *)
  let all_loop_devices = List.filter (String.startswith "loop") (Array.to_list (Sys.readdir "/dev/")) in
  let rec allocate = function
    | []    -> raise Loopdev_all_busy
    | x::xs ->
      let loopdev = "/dev/" ^ x in debug "Checking loop device %s" loopdev;
      let args = (if readonly then [ "-r" ] else []) @ [ loopdev; path ] in
      debug "Executing: losetup [ %s ]" (String.concat "; " args);
      let success = 
        try
          ignore(Forkhelpers.execute_command_get_output losetup args);
          debug "losetup successful";
          true
        with _ ->
          debug "losetup unsuccessful";
          false in
      if success then loopdev else allocate xs
  in
  allocate all_loop_devices


let umount_loopdev loopdev =
  ignore(Forkhelpers.execute_command_get_output losetup ["-d"; loopdev])

(* Allocate a loopback device and associate it with this device so that
   it can be deallocated by the release call. *)
let mount_loopdev ~xs (x: device) file readonly =
  let path = get_hotplug_path x ^ "/loop-device" in
  (* Make sure any previous loop device is gone *)
  (try umount_loopdev (xs.Xs.read path) with Xs_protocol.Enoent _ -> ());
  let loopdev = mount_loopdev_file readonly file in
  debug "Allocated loop device %s" loopdev;
  debug "xenstore-write %s = %s" path loopdev;
  xs.Xs.write path loopdev;
  loopdev

(* Wait for the device to be released by the backend driver (via udev) and
   then deallocate any resources which are registered (in our private bit of
   xenstore) *)
let release (task:Xenops_task.task_handle) ~xs (x: device) =
  debug "Hotplug.release: %s" (string_of_device x);
  wait_for_unplug task ~xs x;
  let path = get_hotplug_path x in
  let all = try xs.Xs.directory path with _ -> [] in
  List.iter (function
      | "loop-device" ->
        let loopdev = xs.Xs.read (path ^ "/loop-device") in
        debug "Hotplug.release releasing %s" loopdev;
        umount_loopdev loopdev;
        xs.Xs.rm (path ^ "/loop-device")
      | "hotplug" ->
        debug "xenstore-rm %s/online" path;
        xs.Xs.rm (path ^ "/online")
      | "" -> () (* XXX? *)
      | x ->
        warn "Warning, deleting unexpected '%s' entry from %s" x path;
        xs.Xs.rm (path ^ "/" ^ x)
    ) all;
  xs.Xs.rm path

let release' (task:Xenops_task.task_handle) ~xs (x: device) vm kind devid =
  debug "Hotplug.release: %s" (string_of_device x);
  wait_for_unplug task ~xs x;
  let path = get_hotplug_path x in
  let all = try xs.Xs.directory path with _ -> [] in
  List.iter (function
      | "loop-device" ->
        let loopdev = xs.Xs.read (path ^ "/loop-device") in
        debug "Hotplug.release releasing %s" loopdev;
        umount_loopdev loopdev;
        xs.Xs.rm (path ^ "/loop-device")
      | "hotplug" ->
        debug "xenstore-rm %s/online" path;
        xs.Xs.rm (path ^ "/online")
      | "" -> () (* XXX? *)
      | x ->
        warn "Warning, deleting unexpected '%s' entry from %s" x path;
        xs.Xs.rm (path ^ "/" ^ x)
    ) all;
  xs.Xs.rm path;
  xs.Xs.rm (get_private_data_path_of_device x)

let run_hotplug_script device args =
  let kind = string_of_kind device.backend.kind in
  let script = match device.backend.kind with
    | Vbd _ -> Some !Xl_resources.vbd_script
    | Vif | Tap -> Some !Xl_resources.vif_script
    | Qdisk -> None
    | _ -> failwith (Printf.sprintf "don't know how to run a hotplug script for: %s" kind) in
  match script with
  | Some script ->
    begin try
        let env = Array.concat [ Unix.environment (); [|
            "script=" ^ script;
            "XENBUS_TYPE=" ^ kind;
            "XENBUS_PATH=" ^ (Printf.sprintf "backend/%s/%d/%d" kind device.frontend.domid device.frontend.devid);
            "XENBUS_BASE_PATH=backend";
          |] ] in
        debug "Running hotplug script %s %s" script (String.concat " " args);
        let stdout, stderr = Forkhelpers.execute_command_get_output ~env script args in
        debug "Got %s %s" stdout stderr;
        ()
      with Forkhelpers.Spawn_internal_error(stdout, stderr, Unix.WEXITED n) as e ->
        error "%s exitted with %d (%s; %s)" script n stdout stderr;
        raise e
    end
  | None -> ()
