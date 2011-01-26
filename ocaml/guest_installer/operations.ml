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

open Util
open Distros

let host = ref "mindanao"
let port = ref 8086

let rpc xml = Xmlrpcclient.do_xml_rpc ~version:"1.0" ~host:!host ~port:!port ~path:"/" xml

open Client

(* initialise CLI session *)
let init_session username password =
  Client.Session.login_with_password ~rpc ~uname:username ~pwd:password ~version:"1.2"


(* construct: create VM, attach some reasonable disks and network devices *)
let vm_construct session_id distro params =

  let host = Client.Session.get_this_host rpc session_id session_id in

  let vm = Client.VM.create ~rpc ~session_id 
    ~name_label:params.name 
    ~name_description:params.desc
    ~user_version:params.version
    ~is_a_template:false 
    ~auto_power_on:false 
    ~affinity:Ref.null
    ~memory_static_max:params.memory_static_max
    ~memory_static_min:params.memory_static_min
    ~memory_dynamic_max:params.memory_dynamic_max
    ~memory_dynamic_min:params.memory_dynamic_min
    ~vCPUs_params:[]
    ~vCPUs_at_startup:params.vcpus
    ~vCPUs_max:params.vcpus
    ~actions_after_shutdown:`destroy
    ~actions_after_reboot:`destroy (* In order to catch the first reboot *)
    ~actions_after_crash:`destroy
    ~pV_bootloader:""
    ~pV_kernel:distro.initial_boot.kernel_path
    ~pV_ramdisk:distro.initial_boot.ramdisk_path
    ~pV_args:distro.initial_boot.kernel_args
    ~pV_bootloader_args:""
    ~pV_legacy_args:""
    ~hVM_boot_policy:""
    ~hVM_boot_params:[]
    ~platform:[]
    ~pCI_bus:"" (* ? *)
    ~other_config:[] in

  (* Attach one disk *)
  
  let vbd_device = distro.disk_device in
  let other_config = Client.Host.get_other_config rpc session_id host in
  let sr = Ref.of_string (List.assoc Constants.default_SR other_config) in
  let vdi_name = vbd_device in
  let vdi = Client.VDI.create rpc session_id vdi_name vdi_name sr params.disk_size `user false false [] in
  let vbd = Client.VBD.create rpc session_id vm vdi vbd_device true `RW `Disk "" [] in

  (* Attach one network device *)
  
  let mac = Util.random_mac () in
  let device = "0" in
  let networks = Client.Network.get_by_name_label rpc session_id "xenbr0" in
  if List.length networks = 0 then raise (Failure "Network xenbr0 not found!");
  let network = List.hd networks in
  let vif = Client.VIF.create rpc session_id device network vm mac 1500L "" [] in
  
  vm

let boot_vm session_id vm =
	Client.VM.start rpc session_id vm false true

let reset_bootloader session_id vm =
  Client.VM.set_PV_bootloader rpc session_id vm "pygrub"




