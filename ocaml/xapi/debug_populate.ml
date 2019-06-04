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

module D=Debug.Make(struct let name="debug_populate" end)
open D

let srs = ref []
let nws = ref []

let rec make_srs __context i =
  if i=0 then ()
  else
    begin
      let uuid = Uuid.to_string (Uuid.make_uuid()) in
      let sr_ref = Xapi_sr.introduce  ~__context ~uuid:uuid ~name_label:("SR-"^(string_of_int i))
          ~name_description:"Dummy data" ~_type:"ext" ~content_type:"dummy" ~shared:true ~sm_config:[] in
      srs := sr_ref :: !srs;
      make_srs __context (i-1)
    end

let rec make_networks __context i =
  if i=0 then ()
  else
    begin
      let nw_ref = Xapi_network.create ~__context ~name_label:("Network-"^(string_of_int i)) ~name_description:"dummy"
        ~mTU:1500L ~other_config:[] ~bridge:"" ~managed:true ~tags:[] in
      nws := nw_ref :: !nws;
      make_networks __context (i-1)
    end

let get_random lr =
  let l = List.length !lr in
  let n = Random.int l in
  List.nth !lr n

let rec make_vdis_and_vbds __context vmref i =
  if i=0 then ()
  else
    begin
      let uuid = Uuid.to_string (Uuid.make_uuid()) in
      let vm_uuid = Db.VM.get_uuid ~self:vmref ~__context in
      let name_label = "VDI-"^(string_of_int i)^"-for-VM-"^vm_uuid in
      let name_description = "dummy" in
      let sR = get_random srs in
      let _type = `user in
      let read_only = false in
      let other_config = [] in
      let location = vm_uuid ^ (string_of_int i) in
      let xenstore_data = [] in
      let sm_config = [] in
      let managed = true in
      let virtual_size = 1L in
      let physical_utilisation = 1L in
      let metadata_of_pool = Ref.null in
      let is_a_snapshot = false in
      let snapshot_time = Stdext.Date.never in
      let snapshot_of = Ref.null in
      let sharable = false in
      let cbt_enabled = false in
      let vdi = Xapi_vdi.pool_introduce
          ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of ~cbt_enabled in

      let _ =
        Xapi_vbd.create ~__context ~vM:vmref ~vDI:vdi ~userdevice:(string_of_int i) ~bootable:true ~mode:`RW ~_type:`Disk ~empty:false
          ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
      make_vdis_and_vbds __context vmref (i-1)
    end

let rec make_vifs __context vmref i =
  if i=0 then ()
  else
    begin
      ignore(Xapi_vif.create  ~__context ~device:(string_of_int i) ~network:(get_random nws) ~vM:vmref
               ~mAC:"de:ad:be:ef:99:88" ~mTU:Int64.zero ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~locking_mode:`network_default ~ipv4_allowed:[] ~ipv6_allowed:[]);
      make_vifs __context vmref (i-1)
    end


let rec make_vms __context template i vdis_per_vm =
  if i=0 then ()
  else
    begin
      let vmref = Xapi_vm.clone ~__context ~vm:template ~new_name:("VM-"^(string_of_int i)) in
      Db.VM.set_is_a_template ~__context ~self:vmref ~value:false;
      make_vdis_and_vbds __context vmref vdis_per_vm;
      make_vifs __context vmref 2;
      make_vms __context template (i-1) vdis_per_vm
    end

let make_tasks __context tasks =
  let create_description label = Printf.sprintf "my task is doing %s stuff" label in
  let pick_random l =
    let len = List.length l in
    let i = Random.int len in
    try List.nth l i with exn -> List.hd l
  in
  let all_vms = Db.VM.get_all ~__context in
  let all_vbds = Db.VBD.get_all ~__context in
  let all_vdis = Db.VDI.get_all ~__context in
  let all_vifs = Db.VIF.get_all ~__context in
  let all_srs = Db.SR.get_all ~__context in
  for i = 0 to tasks - 1
  do
    let mode = Random.int 6 in
    let label = match mode with
      | 0 -> "VM"
      | 1 -> "VBD"
      | 2 -> "VDI"
      | 3 -> "VIF"
      | 4 -> "SR"
      | _ -> "other" in

    let task = Xapi_task.create ~__context ~label ~description:(create_description label) in
    let taskid = Ref.string_of task in
    try
      match mode with
      | 0 ->
        let self = pick_random all_vms in
        Db.VM.add_to_current_operations ~__context ~self ~key:taskid ~value:`import
      | 1 ->
        let self = pick_random all_vbds in
        Db.VBD.add_to_current_operations ~__context ~self ~key:taskid ~value:`unplug
      | 2 ->
        let self = pick_random all_vdis in
        Db.VDI.add_to_current_operations ~__context ~self ~key:taskid ~value:`clone
      | 3 ->
        let self = pick_random all_vifs in
        Db.VIF.add_to_current_operations ~__context ~self ~key:taskid ~value:`plug
      | 4 ->
        let self = pick_random all_srs in
        Db.SR.add_to_current_operations ~__context ~self ~key:taskid ~value:`scan
      | _ ->
        ()
    with _ ->
      ()
  done

let do_populate ~vms ~vdis_per_vm ~networks ~srs ~tasks =
  Server_helpers.exec_with_new_task "populating dummy debug info"
    (fun __context ->
       debug "Populating dummy task info";
       debug "Making dummy SRs";
       make_srs __context srs;
       debug "Making dummy networks";
       make_networks __context networks;
       debug "Making dummy VMs";
       let template = List.hd (Db.VM.get_by_name_label ~__context ~label:"Debian Sarge 3.1") in
       make_vms __context template vms vdis_per_vm;
       debug "Making dummy tasks";
       make_tasks __context tasks;
       debug "Finished populating dummy task info"
    )
