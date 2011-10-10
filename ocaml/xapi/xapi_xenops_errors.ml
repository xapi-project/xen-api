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
(** Code to translate misc low-level xenops exceptions into more user-friendly 
    API errors *)

open Pervasiveext
open Api_errors
open Printf

module D = Debug.Debugger(struct let name="xapi" end)
open D

let to_api_error = function
  | Hotplug.External_command_failure cmdname -> 
      Server_error(internal_error, [ sprintf "failed to execute command: %s" cmdname ])
  | Hotplug.Loopdev_delete_error ->
      Server_error(internal_error, [ "loop device failed to delete" ])
  | Hotplug.Loopdev_error filename ->
      Server_error(internal_error, [ sprintf "failed to losetup the file: %s" filename ])
  | Hotplug.Loopdev_all_busy ->
      Server_error(internal_error, [ "all loop devices are busy" ])
	
  | Device_common.Device_unrecognized x ->
      Server_error(value_not_supported, [ "VBD.device"; x; "unrecognized device" ])
  | Device_common.Device_frontend_already_connected x ->
      Server_error(internal_error, 
		   [ sprintf "another frontend device is already connected to this domain (%s)" (Device_common.string_of_device x) ])
  | Device_common.Device_backend_vanished x ->
      Server_error(internal_error,
		   [ sprintf "the device disappeared from xenstore (%s)" (Device_common.string_of_device x) ])
	
  | Device_common.Hotplug_script_expecting_field(device, field) ->
      Server_error(internal_error,
		   [ sprintf "the hotplug scripts failed to write a field to xenstore (%s) field: %s" (Device_common.string_of_device device) field ])
	
  | Device.Ioemu_failed msg ->
      Server_error(internal_error, [ sprintf "device model failed to initialise: %s" msg ])
  | Device.Ioemu_failed_dying ->
      Server_error(internal_error, [ "internal error waiting for device model to stop (for either shutdown or suspend)" ])
  | Device.Cdrom ->
		Server_error(host_cd_drive_empty, [])
	
  | Domain.Restore_signature_mismatch ->
      Server_error(internal_error, [ "restore file signature mismatch: has suspend image been corrupted?" ])
  | Domain.Domain_build_failed ->
      Server_error(internal_error, [ "failed to build domain" ])
  | Domain.Domain_restore_failed ->
      Server_error(internal_error, [ "failed to restore domain" ])
  | Domain.Xenguest_protocol_failure x ->
      Server_error(internal_error, [ sprintf "protocol failure while talking to xenguesthelper (%s)" x ])
  | Domain.Xenguest_failure x ->
      Server_error(internal_error, [ sprintf "received failure message from xenguesthelper: %s" x ])
  | XenguestHelper.Domain_builder_error(fn, code, msg) ->
      Server_error(domain_builder_error, [ fn; string_of_int code; msg ])
  | Xenctrl.Error x ->
      Vmopshelpers.with_xc
	(fun xc ->
	   let free = Memory.get_free_memory_kib ~xc
	   and total = Memory.get_total_memory_mib ~xc
	   and scrub = Memory.get_scrub_memory_kib ~xc in
	   
	   Server_error(internal_error, [ sprintf "Xenctrl.Error [ memory %Ld KiB free; to be scrubbed %Ld KiB; total %Ld MiB]: %s" free scrub total x ])
	)
  | e -> e

let handle_xenops_error f = 
  try
    f ()
  with e ->
    debug "Converting xenops exception (%s) into nice API internal error" (ExnHelper.string_of_exn e);
    log_backtrace ();
    raise (to_api_error e)
