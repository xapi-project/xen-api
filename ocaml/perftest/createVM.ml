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
open Scenario
open Perfutil
open Client
open Perfdebug

let iscsi_vm_iso = "xenserver-iscsi-target.iso"
let iscsi_vm_template = "Other install media"
let innertemplate = "Windows XP SP3"

let make_iscsi_ip pool =
  Printf.sprintf "192.168.%d.200" (pool.ipbase+2)

let find_iscsi_iso session_id =
  let vdis = Client.VDI.get_all rpc session_id in
  try
    Some (List.find (fun vdi -> Client.VDI.get_name_label rpc session_id vdi = iscsi_vm_iso) vdis)
  with _ -> None

(** Create the VM with the iscsi iso attached *)
let make_iscsi session_id pool network =
  try
    let iscsi_iso = match find_iscsi_iso session_id with
      | Some vdi -> vdi
      | None -> failwith "iSCSI VM iso not found" in
    let template = List.hd (Client.VM.get_by_name_label rpc session_id iscsi_vm_template) in
    let newvm = Client.VM.clone rpc session_id template "ISCSI target server" in
    Client.VM.provision rpc session_id newvm;
    let _ (* isovbd *) = Client.VBD.create rpc session_id newvm iscsi_iso "0" true `RO `CD false false [] "" [] in
    let realpool = List.hd (Client.Pool.get_all rpc session_id) in
    let defaultsr = Client.Pool.get_default_SR rpc session_id realpool in

    for i = 0 to pool.iscsi_luns - 1 do
      let storage_vdi_label = Printf.sprintf "SCSI VDI %d" i in
      let storage_vdi = Client.VDI.create rpc session_id storage_vdi_label "" defaultsr sr_disk_size `user false false [oc_key,pool.key] [] [] [] in
      let userdevice = Printf.sprintf "%d" (i+1) in
      ignore(Client.VBD.create rpc session_id newvm storage_vdi userdevice false `RW `Disk false false [] "" [])
    done;

    Client.VM.set_PV_bootloader rpc session_id newvm "pygrub";
    Client.VM.set_PV_args rpc session_id newvm (Printf.sprintf "net_ip=%s net_mask=255.255.255.0" (make_iscsi_ip pool));
    Client.VM.set_HVM_boot_policy rpc session_id newvm "";
    let (_ : API.ref_VIF) = Client.VIF.create rpc session_id "0" network newvm "" 1500L [oc_key,pool.key] "" [] `network_default [] [] in
    Client.VM.add_to_other_config rpc session_id newvm oc_key pool.key;
    let localhost_uuid = Inventory.lookup "INSTALLATION_UUID" in
    Client.VM.start_on rpc session_id newvm (Client.Host.get_by_uuid rpc session_id localhost_uuid) false false;
    Some newvm
  with e ->
    debug "Caught exception with iscsi VM: %s" (Printexc.to_string e);
    None

let make ~rpc ~session_id ~pool ~vm ~networks ~storages =
  let wintemplate = List.hd (Client.VM.get_by_name_label ~rpc ~session_id ~label:innertemplate) in
  let host_refs = Array.of_list (Client.Host.get_all ~rpc ~session_id) in
  for i = 0 to (Array.length storages) - 1 do
    Printf.printf "Creating %d VMs in SR %d\n%!" vm.num i;
    for j = 0 to vm.num - 1 do
      let newname = Printf.sprintf "VM %d%s%s" j (if Array.length storages > 1 then Printf.sprintf " in SR %d" i else "") (if vm.tag <> "" then " - "^vm.tag else "") in
      let clone = Client.VM.clone ~rpc ~session_id ~vm:wintemplate ~new_name:newname in
      Client.VM.add_tags ~rpc ~session_id ~self:clone ~value:vm.tag;
      Client.VM.remove_from_other_config ~rpc ~session_id ~self:clone ~key:"disks";
      for userdevice = 0 to vm.vbds - 1 do
        Printf.printf " - creating VDI %d for VM %d on SR %d of %d\n%!" userdevice j i (Array.length storages);
        let newdisk = Client.VDI.create ~rpc ~session_id ~name_label:"Guest disk" ~name_description:"" ~sR:storages.(i)
            ~virtual_size:4194304L ~_type:`user ~sharable:false ~read_only:false ~xenstore_data:[] ~other_config:[]
            ~sm_config:[] ~tags:[] in
        ignore(Client.VBD.create ~rpc ~session_id ~vM:clone ~vDI:newdisk ~userdevice:(string_of_int userdevice) ~bootable:false
                 ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~other_config:[])
      done;
      Client.VM.provision ~rpc ~session_id ~vm:clone;
      for device = 0 to (min vm.vifs (Array.length networks)) - 1 do
        ignore(Client.VIF.create ~rpc ~session_id ~device:(string_of_int device) ~network:networks.(device) ~vM:clone ~mAC:""
                 ~mTU:1500L ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~locking_mode:`network_default ~ipv4_allowed:[] ~ipv6_allowed:[])
      done;
      Client.VM.set_memory_static_min ~rpc ~session_id ~self:clone ~value:16777216L;
      Client.VM.set_memory_dynamic_min ~rpc ~session_id ~self:clone ~value:16777216L;
      Client.VM.set_memory_dynamic_max ~rpc ~session_id ~self:clone ~value:16777216L;
      Client.VM.set_memory_static_max ~rpc ~session_id ~self:clone ~value:16777216L;
      if vm.has_affinity && Array.length storages = Array.length host_refs
      then Client.VM.set_affinity ~rpc ~session_id ~self:clone ~value:host_refs.(i);
    done
  done
