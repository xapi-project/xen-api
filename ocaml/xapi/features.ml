(* (C) 2006-2010 Citrix Systems Inc.
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

module D = Debug.Make(struct let name="license" end)
open D

(* Features and restrictions *)

type feature =
	| VLAN
	| QoS
	| Shared_storage
	| Netapp
	| Equalogic
	| Pooling
	| HA
	| Marathon
	| Email
	| Performance
	| WLB
	| RBAC
	| DMC
	| Checkpoint
	| CPU_masking
	| Connection
	| No_platform_filter
	| No_nag_dialog
	| VMPR
	| IntelliCache
	| GPU
	| DR
	| VIF_locking
	| Storage_motion
	| VGPU
	| Integrated_GPU
	| VSS
	| Guest_agent_auto_update
	| PCI_device_for_auto_update
	| Xen_motion
	| Guest_ip_setting
	| AD
	| Ssl_legacy_switch
	with rpc

type orientation = Positive | Negative

let keys_of_features =
	[
		VLAN, ("restrict_vlan", Negative, "VLAN");
		QoS, ("restrict_qos", Negative, "QoS");
		Shared_storage, ("restrict_pool_attached_storage", Negative, "SStorage");
		Netapp, ("restrict_netapp", Negative, "NTAP");
		Equalogic, ("restrict_equalogic", Negative, "EQL");
		Pooling, ("restrict_pooling", Negative, "Pool");
		HA, ("enable_xha", Positive, "XHA");
		Marathon, ("restrict_marathon", Negative, "MTC");
		Email, ("restrict_email_alerting", Negative, "email");
		Performance, ("restrict_historical_performance", Negative, "perf");
		WLB, ("restrict_wlb", Negative, "WLB");
		RBAC, ("restrict_rbac", Negative, "RBAC");
		DMC, ("restrict_dmc", Negative, "DMC");
		Checkpoint, ("restrict_checkpoint", Negative, "chpt");
		CPU_masking, ("restrict_cpu_masking", Negative, "Mask");
		Connection, ("restrict_connection", Negative, "Cnx");
		No_platform_filter, ("platform_filter", Negative, "Plat");
		No_nag_dialog, ("regular_nag_dialog", Negative, "nonag");
		VMPR, ("restrict_vmpr", Negative, "VMPR");
		IntelliCache, ("restrict_intellicache", Negative, "IntelliCache");
		GPU, ("restrict_gpu", Negative, "GPU");
		DR, ("restrict_dr", Negative, "DR");
		VIF_locking, ("restrict_vif_locking", Negative, "VIFLock");
		Storage_motion, ("restrict_storage_xen_motion", Negative, "SXM");
		VGPU, ("restrict_vgpu", Negative, "vGPU");
		Integrated_GPU, ("restrict_integrated_gpu_passthrough", Negative, "iGPU");
		VSS, ("restrict_vss", Negative, "VSS");
		Guest_agent_auto_update, ("restrict_guest_agent_auto_update", Negative, "GAAU");
		PCI_device_for_auto_update, ("restrict_pci_device_for_auto_update", Negative, "PciAU");
		Xen_motion, ("restrict_xen_motion", Negative, "XenMotion");
		Guest_ip_setting, ("restrict_guest_ip_setting", Negative, "GuestIP");
		AD, ("restrict_ad", Negative, "AD");
		Ssl_legacy_switch, ("disable_ssl_legacy_switch", Negative, "Ssl_legacy_switch");
	]

let name_of_feature f =
	rpc_of_feature f |> Rpc.string_of_rpc

let string_of_feature f =
	let str, o, _ = List.assoc f keys_of_features in
	str, o
	
let feature_of_string str =
	let f, (_, o, _) = List.find (fun (_, (k, _, _)) -> str = k) keys_of_features in
	f, o

let tag_of_feature f =
	let _, _, tag = List.assoc f keys_of_features in
	tag
	
let all_features =
	List.map (fun (f, _) -> f) keys_of_features

let to_compact_string (s: feature list) =
	let get_tag f =
		let tag = tag_of_feature f in
		if List.mem f s then
			tag
		else
			String.make (String.length tag) ' '
	in
	let tags = List.map get_tag all_features in
	String.concat " " tags
	
let to_assoc_list (s: feature list) =
	let get_map f =
		let str, o = string_of_feature f in
		let switch = List.mem f s in
		let switch = string_of_bool (if o = Positive then switch else not switch) in
		str, switch
	in
	List.map get_map all_features

let of_assoc_list l =
	let get_feature (k, v) =
		try
			let v = bool_of_string v in
			let f, o = feature_of_string k in
			let v = if o = Positive then v else not v in
			if v then Some f else None
		with _ ->
			None
	in
	let features = List.map get_feature l in
	List.fold_left (function ac -> function Some f -> f :: ac | None -> ac) [] features

