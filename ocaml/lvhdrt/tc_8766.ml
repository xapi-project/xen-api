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
(* Refcounts check *)

open Client
open Utils

let get_refcount rpc session host sr vdi =
  let filename=Printf.sprintf "lvm-%s/%s" (Client.SR.get_uuid rpc session sr) (Client.VDI.get_uuid rpc session vdi) in
  let str = Client.Host.call_plugin 
    ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"read_refcount_file" 
    ~args:[("filename",filename)] in
  Scanf.sscanf str "%d %d" (fun a b -> (a,b)) 

let check_forest rpc session host sr f =
  Utils.debug "Checking forest...";
  let rec donode n =
    match n with Node (i,children) ->
      let uuid = i.uuid in
      let vdi = Client.VDI.get_by_uuid rpc session uuid in
      let (a,b) = try get_refcount rpc session host sr vdi with _ -> (0,0) in
      if a<>i.refa || b<>i.refb then 
	failwith (Printf.sprintf "Failure: uuid=%s expected (%d,%d) got (%d,%d)" uuid i.refa i.refb a b);
      List.iter donode children
  in
  List.iter donode f;
  Utils.debug "Finished - OK"

let run rpc session =
  let sr = Utils.find_lvhd_sr rpc session in
  let master = Utils.get_master rpc session in

  let (vdi,vdi2,vdi3) = Utils.create_vdi_tree rpc session sr "refcount_check" Globs.eight_megs ~resize:Globs.sixteen_megs Globs.eight_megs in
  let (vdi4,vdi5,vdi6) = Utils.create_vdi_tree rpc session sr "refcount_check" Globs.eight_megs ~resize:Globs.sixteen_megs Globs.eight_megs in
  let (vdi7,vdi8,vdi9) = Utils.create_vdi_tree rpc session sr "refcount_check" Globs.eight_megs ~resize:Globs.sixteen_megs Globs.eight_megs in
  
  let vdis = [vdi; vdi2; vdi3; vdi4; vdi5; vdi6; vdi7; vdi8; vdi9] in

  let (dom0,dom0_rec) = Utils.get_control_domain rpc session in

  let devices = Client.VM.get_allowed_VBD_devices rpc session dom0 in

  let (_,vbds) = List.fold_left (fun (devices,vbds) vdi ->
    let device = List.hd devices in
    (List.tl devices, 
    (Client.VBD.create ~rpc ~session_id:session ~vM:dom0 ~vDI:vdi ~userdevice:device
	~bootable:false ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:""
	~qos_algorithm_params:[])::vbds)) (devices,[]) vdis in

  List.iter (fun vbd -> Client.VBD.plug rpc session vbd) vbds;

  let toggle_vbd vbd = 
    if Client.VBD.get_currently_attached rpc session vbd then
      (Utils.debug "Unplugging VBD device %s" (Client.VBD.get_device rpc session vbd);
       Client.VBD.unplug rpc session vbd)
    else
      (Utils.debug "Plugging VBD device %s" (Client.VBD.get_device rpc session vbd);
       Client.VBD.plug rpc session vbd)
  in

  Random.self_init ();

  Pervasiveext.finally
    (fun () ->
       let vdis = Client.SR.get_VDIs rpc session sr in
       
       let rec dostuff m =
	 if m>100 then () else begin
	   Utils.debug "%d/100" m;
	   let f = construct_forest rpc session vdis in
	   (try
	       check_forest rpc session master sr f;
	     with e -> 
	       dump_forest f;
	       raise e);
	   let n = Random.int (List.length vbds) in
	   let vbd = List.nth vbds n in
	   toggle_vbd vbd;
	   dostuff (m+1)
	 end
       in
       dostuff 0
    )
    (fun () -> 
       List.iter (Utils.safe_unplug rpc session) vbds;
       List.iter (fun vbd -> Client.VBD.destroy rpc session vbd) vbds;
    )

