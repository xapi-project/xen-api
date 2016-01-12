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
(** Code for dealing with data read from the guest agent (via xenstore).
    Note this only deals with relatively static data (like version numbers) and
    not dynamic performance data. *)

open Fun
open Xstringext
open Threadext

module D=Debug.Make(struct let name="xapi_guest_metrics" end)
open D

module IntSet = Set.Make(struct type t=int let compare=compare end)

(* A fixed set of standard keys are placed in the PV_drivers_version map.
   NB each key is annotated with whether it appears in windows and/or linux *)
let pv_drivers_version = 
  [ "drivers/xenevtchn",          "xenevtchn";   (* windows *)
    "drivers/xenvbd",             "xenvbd";      (* windows *)
    "drivers/xennet",             "xennet";      (* windows *)
    "attr/PVAddons/MajorVersion", "major";       (* linux + windows *)
    "attr/PVAddons/MinorVersion", "minor";       (* linux + windows *)
    "attr/PVAddons/MicroVersion", "micro";       (* linux + windows -- added in Miami Beta2 *)
    "attr/PVAddons/BuildVersion", "build";       (* windows *)
    "attr/PVAddons/Application",  "application"; (* linux *)
  ]

(* A fixed set of standard keys placed in the os_version map. *)
let os_version = 
  [ "data/os_name",               "name";        (* linux + windows *)
    "data/os_uname",              "uname";       (* linux *)
    "data/os_distro",             "distro";      (* linux + windows *)
    "data/os_majorver",           "major";       (* linux + windows *)
    "data/os_minorver",           "minor";       (* linux + windows *)
    "attr/os/spmajor",            "spmajor";     (* windows *)
    "attr/os/spminor",            "spminor";     (* windows *)
  ]

let memory = 
  [ "data/meminfo_free", "free";
    "data/meminfo_total", "total"
  ]

let device_id = [ "data/device_id", "device_id"]

let extend base str = Printf.sprintf "%s/%s" base str

(* This function is passed the 'attr' node and a function it can use to
 * find the directory listing of sub-nodes. It will return a map where the
 * keys are the xenstore paths of the VM's IP addresses, and the values are
 * the intended keys which will go into the VM_guest_metrics.networks field.
 * Example output for a single vif might be:
 * attr/eth0/ip -> 0/ip
 * attr/eth0/ipv6/0/addr -> 0/ipv6/0
 * attr/eth0/ipv6/1/addr -> 0/ipv6/1
 * *)
let networks path (list: string -> string list) =
	(* Find all ipv6 addresses under a path. *)
	let find_ipv6 path prefix = List.map
		(fun str -> (extend (extend path str) "addr", extend prefix str))
		(list path)
	in
	(* Find the ipv4 address under a path, and the ipv6 addresses if they exist. *)
	let find_all_ips path prefix =
		let ipv4 = (extend path "ip", extend prefix "ip") in
		if List.mem "ipv6" (list path) then
			ipv4 :: (find_ipv6 (extend path "ipv6") (extend prefix "ipv6"))
		else
			[ipv4]
	in
	(* Find all "ethn" or "xenbrn" under a path. *)
	let find_eths path = List.fold_left
		(fun acc eth ->
			if String.startswith "eth" eth then
				let n = String.sub eth 3 (String.length eth - 3) in
				(extend path eth, n) :: acc
			else if String.startswith "xenbr" eth then
				let n = String.sub eth 5 (String.length eth - 5) in
				(extend path eth, n) :: acc
			else
				acc)
		[] (list path)
	in
	path
		|> find_eths
		|> List.map (fun (path, prefix) -> find_all_ips path prefix)
		|> List.concat

(* This function is passed the "device/vif" node, a function it can use to
 * find the directory listing of sub-nodes and a function to retrieve the value
 * with the given path.
 * If "state" of all VIFs are "4", the return value is true
 * which means the network paths are optimized.
 * Or else the return value is false.
 *)
let network_paths_optimized path (list: string -> string list) (lookup: string -> string option) =
	List.fold_left (fun result vif_id ->
		let vif_state = lookup (extend (extend path vif_id) "state") in
		result && (vif_state = Some "4")
	) true (list path)

(* This function is passed the "device/vbd" node, a function it can use to
 * find the directory listing of sub-nodes and a function to retrieve the value
 * with the given path.
 * If "state" of all VBDs (except cdrom) are "4", the return value is true
 * which means the storage paths are optimized.
 * Or else the return value is false.
 *)
let storage_paths_optimized path (list: string -> string list) (lookup: string -> string option) =
	List.fold_left (fun result vbd_id ->
		let vbd_state = lookup (extend (extend path vbd_id) "state") in
		let vbd_type = lookup (extend (extend path vbd_id) "device-type") in
		result && (vbd_state = Some "4" || vbd_type = Some "cdrom")
	) true (list path)

(* One key is placed in the other map per control/* key in xenstore. This
   catches keys like "feature-shutdown" "feature-hibernate" "feature-reboot"
   "feature-sysrq" *)
let other all_control = 
  List.map (fun x -> "control/" ^ x, x) all_control

(* There are two memory keys: data/meminfo_free and data/meminfo_total. These are *)
(* inserted into the memory map with the keys 'free' and 'total' *)

(** Cache the results so that we only actually update the master's database when
    the results of these lookups differ *)

type m = (string * string) list
let cache : (int, (m*m*m*m*m*m*bool*bool*bool*float*API.tristate_type*API.tristate_type)) Hashtbl.t = Hashtbl.create 20
let memory_targets : (int, int64) Hashtbl.t = Hashtbl.create 20
let dead_domains : IntSet.t ref = ref IntSet.empty
let mutex = Mutex.create ()

(** Reset all the guest metrics for a particular VM. 'lookup' reads a key from xenstore
    and 'list' reads a directory from xenstore. Both are relative to the guest's 
    domainpath. *)
let all (lookup: string -> string option) (list: string -> string list) ~__context ~domid ~uuid =
  let all_control = list "control" in
  let to_map kvpairs = List.concat (List.map (fun (xskey, mapkey) -> match lookup xskey with
    | Some xsval -> [ mapkey, xsval ]
    | None -> []) kvpairs) in

  let get_tristate xskey =
	  match lookup xskey with
		  | Some "0" -> `no
		  | Some "1" -> `yes
		  | _ -> `unspecified in

  let ts = match lookup "data/ts" with
  	| Some value -> ["data-ts",value]
  	| None -> [] in
  	
  let pv_drivers_version = to_map pv_drivers_version
  and os_version = to_map os_version
  and device_id = to_map device_id
  and networks = to_map (networks "attr" list)
  and other = List.append (to_map (other all_control)) ts
  and memory = to_map memory
  and network_paths_optimized = network_paths_optimized "device/vif" list lookup
  and storage_paths_optimized = storage_paths_optimized "device/vbd" list lookup
  and last_updated = Unix.gettimeofday () in
  let can_use_hotplug_vbd = get_tristate "feature/hotplug/vbd" in
  let can_use_hotplug_vif = get_tristate "feature/hotplug/vif" in

  let pv_drivers_up_to_date = network_paths_optimized && storage_paths_optimized in
  
  (* let num = Mutex.execute mutex (fun () -> Hashtbl.fold (fun _ _ c -> 1 + c) cache 0) in 
  debug "Number of entries in hashtbl: %d" num; *)

  (* to avoid breakage whilst 'micro' is added to linux and windows agents, default this field
     to -1 if it's not present in xenstore *)
  let pv_drivers_version =
    if List.mem_assoc "micro" pv_drivers_version then pv_drivers_version (* already there; do nothing *)
    else ("micro","-1")::pv_drivers_version in

  let self = Db.VM.get_by_uuid ~__context ~uuid in

  let (
    pv_drivers_version_cached,
    os_version_cached,
    networks_cached,
    other_cached,
    memory_cached,
    device_id_cached,
    network_paths_optimized_cached,
    storage_paths_optimized_cached,
    pv_drivers_up_to_date_cached,
    last_updated_cached,
    can_use_hotplug_vbd_cached,
    can_use_hotplug_vif_cached
  ) = Mutex.execute mutex (fun () -> try
       Hashtbl.find cache domid 
    with _ -> 
      (* Make sure our cached idea of whether the domain is live or not is correct *)
      let vm_guest_metrics = Db.VM.get_guest_metrics ~__context ~self in
	  let live = true
		&& Db.is_valid_ref __context vm_guest_metrics 
		&& Db.VM_guest_metrics.get_live ~__context ~self:vm_guest_metrics in
      if live then
	dead_domains := IntSet.remove domid !dead_domains
      else
	dead_domains := IntSet.add domid !dead_domains;
      ([],[],[],[],[],[],false,false,false,0.0,`unspecified,`unspecified)) in

  (* Consider the data valid IF the data/updated key exists *)
  let data_updated = lookup "data/updated" <> None in
  if data_updated
  then begin

      (* Only if the data is valid, cache it (CA-20353) *)
      Mutex.execute mutex (fun () -> Hashtbl.replace cache domid (pv_drivers_version,os_version,networks,other,memory,device_id,network_paths_optimized,storage_paths_optimized,pv_drivers_up_to_date,last_updated,can_use_hotplug_vbd,can_use_hotplug_vif));

      (* We update only if any actual data has changed *)
      if ( pv_drivers_version_cached <> pv_drivers_version 
	   ||
	   os_version_cached <> os_version
	   ||
	   networks_cached <> networks 
	   ||
	   other_cached <> other
     ||
     device_id_cached <> device_id
     ||
     network_paths_optimized_cached <> network_paths_optimized
     ||
     storage_paths_optimized_cached <> storage_paths_optimized
     ||
     pv_drivers_up_to_date_cached <> pv_drivers_up_to_date)
     ||
     can_use_hotplug_vbd_cached <> can_use_hotplug_vbd
     ||
     can_use_hotplug_vif_cached <> can_use_hotplug_vif
(* Nb. we're ignoring the memory updates as far as the VM_guest_metrics API object is concerned. We are putting them into an RRD instead *)
(*	   ||
	   memory_cached <> memory)*)
      then 
	begin
     	  let gm = 
	    let existing = Db.VM.get_guest_metrics ~__context ~self in
	    if (try ignore(Db.VM_guest_metrics.get_uuid ~__context ~self:existing); true with _ -> false)
	    then existing
	    else 
	      (* if it doesn't exist, make a fresh one *)
	      let new_ref = Ref.make () and new_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	      Db.VM_guest_metrics.create ~__context ~ref:new_ref ~uuid:new_uuid
		~os_version:os_version ~pV_drivers_version:pv_drivers_version ~pV_drivers_up_to_date:false ~memory:[] ~disks:[] ~networks:networks ~other:other
		~storage_paths_optimized:false ~network_paths_optimized:false ~last_updated:(Date.of_float last_updated) ~other_config:[] ~live:true ~can_use_hotplug_vbd:`unspecified ~can_use_hotplug_vif:`unspecified;
	      Db.VM.set_guest_metrics ~__context ~self ~value:new_ref; 
	      (* We've just set the thing to live, let's make sure it's not in the dead list *)
		  let sl xs = String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) xs) in
		  info "Received initial update from guest agent in VM %s; os_version = [ %s]; pv_drivers_version = [ %s ]; networks = [ %s ]" (Ref.string_of self) (sl os_version) (sl pv_drivers_version) (sl networks);
	      Mutex.execute mutex (fun () -> dead_domains := IntSet.remove domid !dead_domains);
	      new_ref in

	  (* We unconditionally reset the database values but observe that the database
	     checks whether a value has actually changed before doing anything *)
	  if(pv_drivers_version_cached <> pv_drivers_version) then
	    Db.VM_guest_metrics.set_PV_drivers_version ~__context ~self:gm ~value:pv_drivers_version;
	  if(os_version_cached <> os_version) then
	    Db.VM_guest_metrics.set_os_version ~__context ~self:gm ~value:os_version;
	  if(networks_cached <> networks) then
	    Db.VM_guest_metrics.set_networks ~__context ~self:gm ~value:networks;
	  if(other_cached <> other) then begin
	    Db.VM_guest_metrics.set_other ~__context ~self:gm ~value:other;
	    Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Client.VM.update_allowed_operations rpc session_id self);
	  end;
	  if(network_paths_optimized_cached <> network_paths_optimized) then begin
	  	Db.VM_guest_metrics.set_network_paths_optimized ~__context ~self:gm ~value:network_paths_optimized;
	  end;
	  if(storage_paths_optimized_cached <> storage_paths_optimized) then begin
	  	Db.VM_guest_metrics.set_storage_paths_optimized ~__context ~self:gm ~value:storage_paths_optimized;
	  end;
	  if(pv_drivers_up_to_date_cached <> pv_drivers_up_to_date) then begin
	  	Db.VM_guest_metrics.set_PV_drivers_up_to_date ~__context ~self:gm ~value:pv_drivers_up_to_date;
	  end;
	  if(can_use_hotplug_vbd_cached <> can_use_hotplug_vbd) then begin
	  	Db.VM_guest_metrics.set_can_use_hotplug_vbd ~__context ~self:gm ~value:can_use_hotplug_vbd;
	  end;
	  if(can_use_hotplug_vif_cached <> can_use_hotplug_vif) then begin
	  	Db.VM_guest_metrics.set_can_use_hotplug_vif ~__context ~self:gm ~value:can_use_hotplug_vif;
	  end;
(*	  if(memory_cached <> memory) then
	    Db.VM_guest_metrics.set_memory ~__context ~self:gm ~value:memory; *)
	  
	  Db.VM_guest_metrics.set_last_updated ~__context ~self:gm ~value:(Date.of_float last_updated);
	  
    if(device_id_cached <> device_id) then begin
      if(List.mem_assoc Xapi_globs.device_id_key_name device_id) then begin
        let value = List.assoc Xapi_globs.device_id_key_name device_id in
        let platform = Db.VM.get_platform ~__context ~self in
				info "Updating VM %s platform:%s <- %s" (Ref.string_of self) Xapi_globs.device_id_key_name value;
				if List.mem_assoc Xapi_globs.device_id_key_name platform then
					(try
						Db.VM.remove_from_platform ~__context ~self ~key:Xapi_globs.device_id_key_name
					with _ -> ());
				try
					Db.VM.add_to_platform ~__context ~self ~key:Xapi_globs.device_id_key_name ~value:value;
				with _ -> ()
      end
    end;

	  (* Update the 'up to date' flag afterwards *)
	  let gmr = Db.VM_guest_metrics.get_record_internal ~__context ~self:gm in

	  (* CA-18034: If viridian flag isn't in there and we have Orlando-or-newer Windows PV drivers then shove it in the metadata for next boot... *)
	  if Xapi_pv_driver_version.is_windows_and_orlando_or_newer gmr then begin
		  let platform = Db.VM.get_platform ~__context ~self in
		  if not(List.mem_assoc Xapi_globs.viridian_key_name platform) then begin
			  info "Setting VM %s platform:%s <- %s" (Ref.string_of self) Xapi_globs.viridian_key_name Xapi_globs.default_viridian_key_value;
			  try
				  Db.VM.add_to_platform ~__context ~self ~key:Xapi_globs.viridian_key_name ~value:Xapi_globs.default_viridian_key_value;
			  with _ -> ()
		  end
	  end;

	  (* We base some of our allowed-operations decisions on these advertised features and the presence/absence of PV drivers. *)
	  if pv_drivers_version_cached <> pv_drivers_version
	    || can_use_hotplug_vbd_cached <> can_use_hotplug_vbd
	    || can_use_hotplug_vif_cached <> can_use_hotplug_vif
	  then begin
	    Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Client.VM.update_allowed_operations rpc session_id self);
	  end;	  
	end (* else debug "Ignored spurious guest agent update" *)
  end

(* XXX This function was previously called in the monitoring loop of the
 * RRD-related code, but that code has now been moved into rrdd, so there
 * is currently no caller. It probably needs to be called from whereever
 * the fields in this file are used. *)
let sync_cache valid_domids =
	Mutex.execute mutex
		(fun _ ->
			let stored_domids = Hashtbl.fold (fun k v acc -> k::acc) cache [] in
			List.iter (fun domid -> if not (List.mem domid valid_domids) then dead_domains := IntSet.remove domid !dead_domains) stored_domids;
			Hashtblext.remove_other_keys cache valid_domids;
		)
