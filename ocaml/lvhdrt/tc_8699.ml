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

(* Unofficial test cases for which we don't currently have a TC number *)
open Listext
open Client



let tc1 rpc session =
  (* Description:
   *
   * - Create a VDI
   * - clone it to create a tree of 3 VDIs
   * - find the parent VDI
   * - make sure we can't clone, snapshot, vbd create+plug vdi destroy *)

  let failmsg=ref "" in
  
  (* Find an LVHD SR *)
  let sr = Utils.find_lvhd_sr rpc session in

  let before_sr_vdis = Utils.get_my_vdis rpc session sr in

  let vdi = Client.VDI.create ~rpc ~session_id:session ~name_label:"LVHD test" ~name_description:"Test for cloning purposes" ~sR:sr ~virtual_size:Globs.four_megs ~_type:`ephemeral (*!*)
    ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
  let (_: API.ref_VDI) = Client.VDI.clone ~rpc ~session_id:session ~vdi ~driver_params:[] in

  let after_sr_vdis = Utils.get_my_vdis rpc session sr in

  (* Sanity: We should have 3 new VDIs *)
  let new_vdis = List.set_difference after_sr_vdis before_sr_vdis in
  if List.length new_vdis <> 3 then failwith (Printf.sprintf "Failed to get 3 new VDIs (got %d)" (List.length new_vdis));

  (* Find the one that's not 'managed' *)
  let non_managed_vdis = List.filter (fun (vdi_ref,vdi_rec) -> not vdi_rec.API.vDI_managed) new_vdis in
  if List.length non_managed_vdis <> 1 then failwith (Printf.sprintf "Failed to find non-managed VDI");

  (* Attempt to do stuff with it *)
  let (parent_vdi,parent_vdi_rec) = List.hd non_managed_vdis in 
  
  begin
    try
      ignore(Client.VDI.clone ~rpc ~session_id:session ~vdi:parent_vdi ~driver_params:[]);
      failmsg := "Was able to clone!";
    with e -> 
      Printf.printf "SUCCESS: VDI.clone failed (%s)\n" (Printexc.to_string e);
  end;
  if !failmsg <> "" then failwith !failmsg;

  begin 
    try ignore(Client.VDI.snapshot ~rpc ~session_id:session ~vdi:parent_vdi ~driver_params:[]);
      failmsg := "Was able to snapshot!";
    with e -> 
      Printf.printf "SUCCESS: VDI.snapshot failed (%s)\n" (Printexc.to_string e);
  end;
  if !failmsg <> "" then failwith !failmsg;

  begin
    try ignore(Client.VDI.destroy ~rpc ~session_id:session ~self:parent_vdi);
      failmsg := "Was able to destroy!";
    with e ->
      Printf.printf "SUCCESS: VDI.destroy failed (%s)\n" (Printexc.to_string e);
  end;
  if !failmsg <> "" then failwith !failmsg;

  let (vm,vmr) = Utils.get_control_domain rpc session in

  begin
    try
      let devices = Client.VM.get_allowed_VBD_devices ~rpc ~session_id:session ~vm in
      let vbd = Client.VBD.create ~rpc ~session_id:session ~vM:vm ~vDI:parent_vdi ~userdevice:(List.hd devices) 
	~bootable:false ~mode:`RO ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:""
	~qos_algorithm_params:[] in
      Client.VBD.plug ~rpc ~session_id:session ~self:vbd;
      failmsg := "Was able to create/plug VBD!";
    with e ->
      Printf.printf "SUCCESS: VBD.create/plug failed (%s)\n" (Printexc.to_string e);
  end;
  if !failmsg <> "" then failwith !failmsg


  
  

  
