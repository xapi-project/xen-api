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
(** Types which represent VM configuration independent of the pool database. Rather than pass around
   database references and use expensive lookups we can pass around these types instead. 
 * @group Virtual-Machine Management
 *)

(* This is a work in progress: as the code evolves we can shuffle data between the records *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

type vif = {
  domid: int; (** current domain id of the VM *)
  vif_ref: API.ref_VIF;
  network_ref: API.ref_network;
  devid: int;
  protocol: Device_common.protocol; 
  mac: string;
  mtu: int;
  bridge: string; (** name of the bridge in domain 0 *)
  rate: (int64 * int64) option;
  other_config: (string * string) list;
}

(* Note: we always pass in the VM's domid rather than use the one from the VM record because
   during migration the VM actually has two domids. *)

let vif_of_vif ~__context ~vm vm_r domid protocol vif = 
  let vif_r = Db.VIF.get_record ~__context ~self:vif in
  let log_qos_failure reason = 
    warn "vif QoS failed: %s (vm=%s,vif=%s)" reason vm_r.API.vM_uuid vif_r.API.vIF_uuid in
  
  let mac = vif_r.API.vIF_MAC in
  let vif_mtu = if (List.mem_assoc "mtu" vif_r.API.vIF_other_config) then
    Some (int_of_string (List.assoc "mtu" vif_r.API.vIF_other_config)) else None in

  let qos_type = vif_r.API.vIF_qos_algorithm_type in
  let qos_params = vif_r.API.vIF_qos_algorithm_params in
  let rate = match qos_type with
    | "ratelimit" -> 
	let timeslice =
	  try Int64.of_string (List.assoc "timeslice_us" qos_params)
	  with _ -> 0L in
	begin
	  try
	    let rate = Int64.of_string (List.assoc "kbps" qos_params) in
	    Some (rate, timeslice)
	  with
	  | Failure "int_of_string" ->
	      log_qos_failure "parameter \"kbps\" not an integer"; None
	  | Not_found               ->
	      log_qos_failure "necessary parameter \"kbps\" not found"; None
	  | e                       ->
	      log_qos_failure (Printf.sprintf "unexpected error: %s" (Printexc.to_string e)); None
	end
    | ""          -> None
    | _           -> log_qos_failure (Printf.sprintf "unknown type: %s" qos_type); None in
    
  (* If we fail to find the network or bridge then assume this VIF is a dangling reference *)
  try
    let network_ref = vif_r.API.vIF_network in
    let bridge = Db.Network.get_bridge ~__context ~self:network_ref in
    let mtu = match vif_mtu with
      | Some mtu -> mtu
      | None -> Int64.to_int (Db.Network.get_MTU ~__context ~self:network_ref) in
    Some {
      domid = domid;
      vif_ref = vif;
      network_ref = network_ref;
      devid = int_of_string vif_r.API.vIF_device;
      protocol = protocol;
      mac = mac;
      mtu = mtu;
      bridge = bridge;
      rate = rate;
      other_config = vif_r.API.vIF_other_config
    }
  with _ -> None 
    
(** Return a list of vif records corresponding to all the non-dangling VIF references *)
let vifs_of_vm ~__context ~vm domid = 
  let vm_r = Db.VM.get_record ~__context ~self:vm in
  let protocol = Helpers.device_protocol_of_string vm_r.API.vM_domarch in
  
  List.concat (List.map Opt.to_list (List.map (vif_of_vif ~__context ~vm vm_r domid protocol) vm_r.API.vM_VIFs))

let device_of_vif vif = 
  let backend = { Device_common.domid = 0; 
		  kind = Device_common.Vif;
		  devid = vif.devid } in
  Device_common.device_of_backend backend vif.domid

