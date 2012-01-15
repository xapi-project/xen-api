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
(**
 * @group Storage
 *)
 
open Printf
open Threadext
open Stringext
open Pervasiveext
open Vmopshelpers
open Client

module D = Debug.Debugger(struct let name="xapi" end)
open D

module L = Debug.Debugger(struct let name="license" end)


(** Thrown if an empty VBD which isn't a CDROM is attached to an HVM guest *)
exception Only_CD_VBDs_may_be_empty


let translate_vbd_device vbd_ref name is_hvm =
	try
		let i = Device_number.of_string is_hvm name in
		debug "VBD device name %s interpreted as %s (hvm = %b)" name (Device_number.to_debug_string i) is_hvm;
		i
	with _ ->
		raise (Api_errors.Server_error(Api_errors.illegal_vbd_device, [ Ref.string_of vbd_ref; name ]))

(** Create a debug-friendly string from a VBD *)
let string_of_vbd ~__context ~vbd = 
  let r = Db.VBD.get_record ~__context ~self:vbd in
  let name = r.API.vBD_userdevice ^ "/" ^ r.API.vBD_device in
  let vdi = if r.API.vBD_empty then "empty" else try Db.VDI.get_uuid ~__context ~self:r.API.vBD_VDI with _ -> "missing" in
  name ^ ":" ^ vdi

(* real helpers *)
let create_vbd ~__context ~xs ~hvm ~protocol domid self =
  Xapi_xenops_errors.handle_xenops_error
    (fun () ->
	(* Don't attempt to attach an empty VBD to a PV guest *)
	let empty = Db.VBD.get_empty ~__context ~self in
	let dev_type = Db.VBD.get_type ~__context ~self in
	if empty && dev_type <> `CD
	then raise Only_CD_VBDs_may_be_empty;

	let mode = Db.VBD.get_mode ~__context ~self in
	let mode = match mode with
	  | `RW -> Device.Vbd.ReadWrite
	  | `RO -> Device.Vbd.ReadOnly in
	let dev_type = match dev_type with
	  | `Disk -> Device.Vbd.Disk
	  | `CD   -> Device.Vbd.CDROM in
	let unpluggable = Db.VBD.get_unpluggable ~__context ~self in

	let userdevice = Db.VBD.get_userdevice ~__context ~self in
	let device_number = translate_vbd_device self userdevice hvm in

	let vdi = Db.VBD.get_VDI ~__context ~self in

	let force_loopback_vbd = Helpers.force_loopback_vbd ~__context in

	if empty then begin
		if hvm then begin
			let vbd = {
				Device.Vbd.mode = mode;
				device_number = Some device_number;
				phystype = Device.Vbd.File;
				params = "";
				dev_type = dev_type;
				unpluggable = unpluggable;
				protocol = if protocol = Device_common.Protocol_Native then None else Some protocol;
				extra_backend_keys = [];
				extra_private_keys = [ "ref", Ref.string_of self ];
				backend_domid = 0
			} in
			let (_: Device_common.device) = Device.Vbd.add ~xs ~hvm vbd domid in
			Db.VBD.set_device ~__context ~self ~value:(Device_number.to_linux_device device_number);
			Db.VBD.set_currently_attached ~__context ~self ~value:true;			
		end else info "domid: %d PV guests don't support the concept of an empty CD; skipping device" domid
	end else if System_domains.storage_driver_domain_of_vbd ~__context ~vbd:self = Db.VBD.get_VM ~__context ~self && not force_loopback_vbd then begin
		debug "VBD.plug of loopback VBD '%s'" (Ref.string_of self);
		Storage_access.attach_and_activate ~__context ~vbd:self ~domid ~hvm
			(fun params ->
				let prefix = "/dev/" in
				let prefix_len = String.length prefix in
				let path = String.sub params prefix_len (String.length params - prefix_len) in
				Db.VBD.set_device ~__context ~self ~value:path;
				Db.VBD.set_currently_attached ~__context ~self ~value:true;
			)
	end else begin
		let sr = Db.VDI.get_SR ~__context ~self:vdi in
		let phystype = Device.Vbd.physty_of_string (Sm.sr_content_type ~__context ~sr) in
		Storage_access.attach_and_activate ~__context ~vbd:self ~domid ~hvm
			(fun params ->
				let backend_domid = Storage_mux.domid_of_sr (Db.SR.get_uuid ~__context ~self:sr) in
				try
					(* The backend can put useful stuff in here on vdi_attach *)
					let extra_backend_keys = List.map (fun (k, v) -> "sm-data/" ^ k, v) (Db.VDI.get_xenstore_data ~__context ~self:vdi) in
					let vbd = {
						Device.Vbd.mode = mode;
						device_number = Some device_number;
						phystype = phystype;
						params = params;
						dev_type = dev_type;
						unpluggable = unpluggable;
						protocol = if protocol = Device_common.Protocol_Native then None else Some protocol;
						extra_backend_keys = extra_backend_keys;
						extra_private_keys = [ "ref", Ref.string_of self ];
						backend_domid = backend_domid
					} in
					let (_: Device_common.device) = Device.Vbd.add ~xs ~hvm vbd domid in
					Db.VBD.set_device ~__context ~self ~value:(Device_number.to_linux_device device_number);
					Db.VBD.set_currently_attached ~__context ~self ~value:true;
					debug "set_currently_attached to true for VBD uuid %s" (Db.VBD.get_uuid ~__context ~self)
				with
					| Hotplug.Device_timeout device ->
						error "Timeout waiting for backend hotplug scripts (%s) for VBD %s" (Device_common.string_of_device device) (string_of_vbd ~__context ~vbd:self);
						raise (Api_errors.Server_error(Api_errors.device_attach_timeout,
						[ "VBD"; Ref.string_of self ]))
					| Hotplug.Frontend_device_timeout device ->
						error "Timeout waiting for frontend hotplug scripts (%s) for VBD %s" (Device_common.string_of_device device) (string_of_vbd ~__context ~vbd:self);
						raise (Api_errors.Server_error(Api_errors.device_attach_timeout,
						[ "VBD"; Ref.string_of self ]))
			)
	end
    )

let eject_vbd ~__context ~self =
	if not (Db.VBD.get_empty ~__context ~self) then (
		let vdi = Db.VBD.get_VDI ~__context ~self in

		let is_sr_local_cdrom sr =
			let srty = Db.SR.get_type ~__context ~self:sr in
			if srty = "udev" then (
				let smconfig = Db.SR.get_sm_config ~__context ~self:sr in
				try List.assoc "type" smconfig = "cd" with _ -> false
			) else
				false
			in
		let activate_tray =
			let host = Helpers.get_localhost ~__context in
			try
				let oc = Db.Host.get_other_config ~__context ~self:host in
				bool_of_string (List.assoc Xapi_globs.cd_tray_ejector oc)
			with _ -> false
			in
		if is_sr_local_cdrom (Db.VDI.get_SR ~__context ~self:vdi) then (
			(* check if other VBD are also attached to this VDI *)
			let allvbds = Db.VDI.get_VBDs ~__context ~self:vdi in
			let running_vbds = List.fold_left (fun acc vbd ->
				try
					let vm = Db.VBD.get_VM ~__context ~self:vbd in
					if Helpers.is_running ~__context ~self:vm then
						(vbd, vm) :: acc
					else
						acc
				with _ -> acc
			) [] allvbds in
		
			(* iterate over all xenstore entries related to vbd/vm to see if the guest already
			   ejected the cd or not *)
			let notejected = List.fold_left (fun acc (vbd, vm) ->
				let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
				let device_number = Device_number.of_string true (Db.VBD.get_device ~__context ~self:vbd) in
				with_xs (fun xs ->
					if Device.Vbd.media_is_ejected ~xs ~device_number domid then begin
						Storage_access.deactivate_and_detach ~__context ~vbd ~domid ~unplug_frontends:true;
						acc
					end else
						vbd :: acc
				)
			) [] running_vbds in

			if List.length notejected = 0 then (
				if activate_tray then (
					let location = Db.VDI.get_location ~__context ~self:vdi in
					let cmd = [| "eject"; location |] in
					ignore (Unixext.spawnvp cmd.(0) cmd)
				);
			)
		) else (
			Db.VBD.set_empty ~__context ~self ~value:true;
			Db.VBD.set_VDI ~__context ~self ~value:Ref.null
		)
	)

(* Sets VBD as empty; throws an error if the VBD is not removable. Called for each
   VBD whose VDI could not be attached or locked. Intention is to allow a VM to boot
   if a removable device couldn't be attached but NOT if any non-removable disks
   couldn't be attached.
   Is a no-op in the event a VBD is already marked as empty *)
let mark_as_empty ~__context ~vbd =
  if not(Db.VBD.get_empty ~__context ~self:vbd) then begin
    let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
    let name = string_of_vbd ~__context ~vbd in
    debug "VBD device %s: VDI failed to attach" name;
    if not(Helpers.is_removable ~__context ~vbd) then begin
      error "VBD device %s: VDI failed to attach and cannot be ejected because VBD is not removable media" name;
      let vm = Ref.string_of (Db.VBD.get_VM ~__context ~self:vbd) in
      let vdi = Ref.string_of vdi in
      raise (Api_errors.Server_error(Api_errors.vm_requires_vdi, [ vm; vdi ]))
    end;
    debug "VBD device %s: marking as empty" name;
    Db.VBD.set_empty ~__context ~self:vbd ~value:true;
    Db.VBD.set_VDI ~__context ~self:vbd ~value:Ref.null
  end

(* Check to see if a VDI record still exists. If not (eg if the scanner zapped it)
   then attempt to mark the VBD as empty *)
let check_vdi_exists ~__context ~vbd = 
  let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
  try 
    ignore(Db.VDI.get_uuid ~__context ~self:vdi)
  with _ ->
    mark_as_empty ~__context ~vbd
    
(** On a start/reboot we only actually attach those disks which satisfy
    at least one of:
    (i) marked with operation `attach in the message-forwarder (VM.start); or
    (ii) marked with 'reserved' by the event thread (in-guest reboot).
    All other disks should be ignored.
    Note during resume and migrate the currently_attached field is used. *)
let vbds_to_attach ~__context ~vm = 
  let should_attach_this_vbd vbd =
    try
      let vbd_r = Db.VBD.get_record_internal ~__context ~self:vbd in
      false
      || vbd_r.Db_actions.vBD_currently_attached
      || vbd_r.Db_actions.vBD_reserved
      || (List.mem `attach (List.map snd vbd_r.Db_actions.vBD_current_operations))
    with _ ->
      (* Skip VBD because it was destroyed: it must not have been
	 any of the ones we care about *)
      false in
  List.filter should_attach_this_vbd (Db.VM.get_VBDs ~__context ~self:vm)
