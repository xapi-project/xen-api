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
 * @group Virtual-Machine Management
*)

(** Here we define a template to be a VM with 'is_a_template = true' which,
    when initially booted after having been cloned, inspects its own
    configuration (stored by the UI/CLI in VM.other_config) and uses the API
    to provision disks, make filesystems, perform any install steps and then
    leave the VM in a state such that it comes up properly on subsequent reboots. *)

(** Should make a dummy one of these for in-guest installers: *)
module D = Debug.Make(struct let name="xapi" end)
open D

(** A record which describes a disk provision request *)
type disk = { device: string; (** device inside the guest eg xvda *)
              size: int64;    (** size in bytes *)
              sr: string;     (** name or UUID of the SR in which to make the disk *)
              bootable: bool;
              _type: API.vdi_type
            }

(** A record which describes the template *)
type template = { disks: disk list;
                  post_install_script: string option }

(** The disk records are marshalled as XML *)
open Xml

let string2vdi_type s =
  match s with
    "system" -> `system
  | "user" -> `user
  | "ephemeral" -> `ephemeral
  | "suspend" -> `suspend
  | "crashdump" -> `crashdump
  | vdi_st -> raise Api_errors.(Server_error(internal_error, [
      Printf.sprintf "string2vdi_type: Unknown VDI type \"%s\"" vdi_st
    ]))

exception Parse_failure
let disk_of_xml = function
  | Element("disk", params, []) ->
    begin
      try
        let device = List.assoc "device" params
        and size = List.assoc "size" params
        and sr = List.assoc "sr" params
        and bootable = List.assoc "bootable" params
        and _type = try string2vdi_type (List.assoc "type" params) with _ -> `system
        in
        { device = device; size = Int64.of_string size; sr = sr;
          bootable = (bootable = "true"); _type = _type}
      with _ -> raise Parse_failure
    end
  | _ -> raise Parse_failure
let disks_of_xml = function
  | Element("provision", [], disks) -> List.map disk_of_xml disks
  | _ -> raise Parse_failure

(** The key name pointing to the disks *)
let disks_key = "disks"

(** The key name pointing to the post-install script *)
let post_install_key = "postinstall"

open Client

(** From a VM reference, return an 'install' record option. *)
let get_template_record rpc session_id vm =
  let other_config = Client.VM.get_other_config rpc session_id vm in
  let disks = if List.mem_assoc disks_key other_config
    then disks_of_xml (Xml.parse_string (List.assoc disks_key other_config)) else [] in
  let script = if List.mem_assoc post_install_key other_config
    then Some (List.assoc post_install_key other_config) else None in
  if disks = [] && script = None
  then None
  else Some { disks = disks; post_install_script = script }

(** Returns true if the given VM is actually a template and must be pre-installed *)
let needs_to_be_installed rpc session_id vm =
  get_template_record rpc session_id vm <> None

(** For a VM and a disk record, create a VDI, VBD and return the VBD.
    Pass in the logging functions to avoid having to link this module against the log
    library. Hopefully we can link this code directly into the in-guest installer. *)
let create_disk rpc session_id vm sm_config disk =
  let sr =
    try
      Client.SR.get_by_uuid rpc session_id disk.sr
    with _ ->
      D.error "Unable to find SR (uuid: %s) to provision the disk" disk.sr;
      raise (Api_errors.Server_error (Api_errors.uuid_invalid, ["sr"; disk.sr ]))
  in
  debug "Provisioning VDI for new VM";
  let vdi = Client.VDI.create ~rpc ~session_id
      ~name_label:"" ~name_description:"Created by template provisioner"
      ~sR:sr ~virtual_size:disk.size
      ~_type:disk._type ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config ~tags:[] in
  let vbd_ref = Client.VBD.create ~rpc ~session_id
      ~vM:vm ~vDI:vdi ~userdevice:disk.device ~bootable:disk.bootable ~mode:`RW ~_type:`Disk
      ~unpluggable:(disk._type <> `system)
      ~empty:false ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~other_config:[Xapi_globs.owner_key,""] in
  let device=Client.VBD.get_userdevice ~rpc ~session_id ~self:vbd_ref in
  Client.VDI.set_name_label ~rpc ~session_id ~self:vdi ~value:device;
  vbd_ref


(** For a given VM, if it needs to be installed, create each disk and return
    the optional post-install script and a list of created VBDs *)
let pre_install rpc session_id vm =
  debug "Performing pre_install actions (ie creating disks)";
  (* driver params for each call - vmhint and epochhint for netapp *)
  let vmuuid = Client.VM.get_uuid rpc session_id vm in
  let sm_config =
    [ Xapi_globs._sm_vm_hint, vmuuid ] in
  match get_template_record rpc session_id vm with
  | Some { disks = disks; post_install_script = script } ->
    let vbds = List.map (create_disk rpc session_id vm sm_config) disks in
    script, vbds
  | None ->
    None, []

(** For a given VM, perform post-install tidy-up (ie remove keys from other_config which would
    cause the template to be installed twice) *)
let post_install rpc session_id vm =
  debug "Performing post_install actions (ie removing template information from VM)";
  (try Client.VM.remove_from_other_config rpc session_id vm disks_key with _ -> ());
  (try Client.VM.remove_from_other_config rpc session_id vm post_install_key with _ -> ())

