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
 * @group Performance Monitoring
 *)
 
open Listext
open Threadext
open Monitor_types

module D=Debug.Debugger(struct let name="monitormaster" end)
open D

(***************** settings stuffs *)
let set_vm_metrics ~__context ~vm ~memory ~cpus =
	(* if vm metrics don't exist then make one *)
	let metrics = Db.VM.get_metrics ~__context ~self:vm in
	if not (Db.is_valid_ref __context metrics) then
	  begin
	    let ref = Ref.make() in
	    Db.VM_metrics.create ~__context ~ref ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
	      ~memory_actual:0L ~vCPUs_number:0L
	      ~vCPUs_utilisation:[]
	      ~vCPUs_CPU:[]
	      ~vCPUs_params:[]
	      ~vCPUs_flags:[]
	      ~state:[]
	      ~start_time:Date.never
	      ~install_time:Date.never
	      ~last_updated:Date.never
	      ~other_config:[];
	    Db.VM.set_metrics ~__context ~self:vm ~value:ref
	  end;

	let metrics = Db.VM.get_metrics ~__context ~self:vm in
	let v = List.mapi (fun i e -> (Int64.of_int i), e) (Array.to_list cpus.vcpu_vcpus) in
	Db.VM_metrics.set_VCPUs_utilisation ~__context ~self:metrics ~value:v;
	Db.VM_metrics.set_last_updated ~__context ~self:metrics ~value:(Date.of_float (Unix.gettimeofday ()));
	match memory with 
	  | Some memory ->
	      Db.VM_metrics.set_memory_actual ~__context ~self:metrics ~value:memory.memory_mem
	  | None -> ()

(* Update the VM's vCPU, VIF stats *)
let update_vm_stats ~__context uuid cpus vbds vifs memory =
	try
	  let vm = Db.VM.get_by_uuid ~__context ~uuid:uuid in
 	    set_vm_metrics ~__context ~vm ~memory ~cpus;
	    let vm_vifs = Db.VM.get_VIFs ~__context ~self:vm in
	      List.iter (fun self ->
			   let num = int_of_string (Db.VIF.get_device ~__context ~self) in
			   let io_write,io_read =
			     try 
			       let vif = List.find (fun vif -> vif.vif_n=num) vifs in
			       vif.vif_tx, vif.vif_rx
			     with _ -> 
			       0., 0. in

			   (* if vif metrics don't exist then make one *)
			   let metrics = Db.VIF.get_metrics ~__context ~self in
			   if not (Db.is_valid_ref __context metrics) then
			     begin
			       let ref = Ref.make() in
			       Db.VIF_metrics.create ~__context ~ref ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
				 ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.) ~other_config:[];
			       Db.VIF.set_metrics ~__context ~self ~value:ref
			     end;

			   let metrics = Db.VIF.get_metrics ~__context ~self in
			   begin
			     Db.VIF_metrics.set_io_write_kbs ~__context ~self:metrics ~value:io_write;
			     Db.VIF_metrics.set_io_read_kbs ~__context ~self:metrics ~value:io_read;
			     Db.VIF_metrics.set_last_updated ~__context ~self:metrics
			       ~value:(Date.of_float (Unix.gettimeofday ()));
			   end
			) vm_vifs;
	    let vm_vbds = Db.VM.get_VBDs ~__context ~self:vm in
	      List.iter (fun self ->
	                  let num = try Device.Vbd.device_number (Db.VBD.get_device ~__context ~self) with _ -> -1 in
			  let io_write, io_read =
			    try
			      let vbd = List.find (fun vbd -> vbd.vbd_device_id = num) vbds in
			      vbd.vbd_io_write, vbd.vbd_io_read
			    with _ -> 0., 0. in

			  (* if vbd metrics don't exist then make one *)
			   let metrics = Db.VBD.get_metrics ~__context ~self in
			   if not (Db.is_valid_ref __context metrics) then
			     begin
			       let ref = Ref.make() in
			       Db.VBD_metrics.create ~__context ~ref ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
				 ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.) ~other_config:[];
			       Db.VBD.set_metrics ~__context ~self ~value:ref
			     end;
			   
			   let metrics = Db.VBD.get_metrics ~__context ~self in
			   Db.VBD_metrics.set_io_write_kbs ~__context ~self:metrics ~value:io_write;
			   Db.VBD_metrics.set_io_read_kbs ~__context ~self:metrics ~value:io_read;
			   Db.VBD_metrics.set_last_updated ~__context ~self:metrics
			     ~value:(Date.of_float (Unix.gettimeofday ()));
	                ) vm_vbds
	with e ->
	  begin
	    debug "Caught exception updating stats for vm (uuid: %s) -- %s" uuid (Printexc.to_string e);
	    log_backtrace()
	  end

let update_host_cpu ~__context host cpus' =
        let cpus = cpus'.pcpus_usage in
	let all = Db.Host.get_host_CPUs ~__context ~self:host in
	if Array.length cpus < List.length all then
		debug "Monitor update_host_cpu got Array.length cpus = %d; num host_cpus = %d"
		      (Array.length cpus) (List.length all)
	else
	  begin
	    (* If Host_cpu objects are missing, fill 'em in with temporary random data.
	       This is needed to make sure Rio/Miami migrate succeeds *)
	    if List.length all < Array.length cpus then
	      begin
		let numbers = List.map (fun self -> Int64.to_int (Db.Host_cpu.get_number ~__context ~self)) all in
		for i = 0 to Array.length cpus - 1 do
		  if not(List.mem i numbers)
		  then 
		    let () = Db.Host_cpu.create ~__context ~ref:(Ref.make()) 
		      ~uuid:(Uuid.string_of_uuid (Uuid.make_uuid ())) ~host ~number:(Int64.of_int i)
		      ~vendor:"unknown" ~speed:0L ~modelname:"unknown"
		      ~utilisation:cpus.(i) ~flags:"unknown" ~stepping:"unknown" ~model:(-1L) ~family:(-1L)
                      ~features:"unknown" ~other_config:[] in 
		    ()
		done
	      end;
	    let all = Db.Host.get_host_CPUs ~__context ~self:host in
	    List.iter (fun self ->
			 let num = Int64.to_int (Db.Host_cpu.get_number ~__context ~self) in
			 let value = cpus.(num) in
			 Db.Host_cpu.set_utilisation ~__context ~self ~value
		      ) all
	  end
	    
let update_host_metrics ~__context h =
	let bytes_of_kib x = Int64.shift_left x 10 in

	Xapi_host_helpers.update_host_metrics ~__context ~host:h.host_ref
	                                      ~memory_total:(bytes_of_kib h.total_kib)
	                                      ~memory_free:(bytes_of_kib h.free_kib)

let get_pciids vendor device =
	(* FIXME : put a lazy cache *)
	let v, d = Pciutil.parse vendor device in
	(match v with None -> "" | Some x -> x),
	(match d with None -> "" | Some x -> x)

let set_pif_metrics ~__context ~self ~vendor ~device
                    ~carrier ~speed ~duplex ~pcibuspath ~io_write ~io_read pmr =
	(* don't update & and reread pciids if db already contains same value *)
	if pmr.API.pIF_metrics_vendor_id <> vendor
	|| pmr.API.pIF_metrics_device_id <> device then (
		let vendor_str, device_str = get_pciids vendor device in
		Db.PIF_metrics.set_vendor_id ~__context ~self ~value:vendor;
		Db.PIF_metrics.set_device_id ~__context ~self ~value:device;
		Db.PIF_metrics.set_vendor_name ~__context ~self ~value:vendor_str;
		Db.PIF_metrics.set_device_name ~__context ~self ~value:device_str;
	);
	if pmr.API.pIF_metrics_carrier <> carrier then
		Db.PIF_metrics.set_carrier ~__context ~self ~value:carrier;
	if pmr.API.pIF_metrics_speed <> speed then
		Db.PIF_metrics.set_speed ~__context ~self ~value:speed;
	if pmr.API.pIF_metrics_duplex <> duplex then
		Db.PIF_metrics.set_duplex ~__context ~self ~value:duplex;
	if pmr.API.pIF_metrics_pci_bus_path <> pcibuspath then
		Db.PIF_metrics.set_pci_bus_path ~__context ~self ~value:pcibuspath;
	if io_write >= 0.0 then
	  Db.PIF_metrics.set_io_write_kbs ~__context ~self ~value:io_write;
	if io_read >= 0.0 then
	  Db.PIF_metrics.set_io_read_kbs ~__context ~self ~value:io_read;
	Db.PIF_metrics.set_last_updated ~__context ~self
	                                ~value:(Date.of_float (Unix.gettimeofday ()))

(* Nb, the following function is actually called on the slave most of the time now - 
   but only when the PIF information changes. *)
let update_pifs ~__context host pifs =
	let pifdevs = Db.Host.get_PIFs ~__context ~self:host in
	List.iter (fun pifdev ->
		(* Compute the name of the physical interface in dom0 *)
		let physical_device_name = 
			let dev = Db.PIF.get_device ~__context ~self:pifdev in
			let vlan = Db.PIF.get_VLAN ~__context ~self:pifdev in
			Helpers.get_dom0_network_device_name dev vlan in
		
		(* The names of bridges which have tunnels via this physical device *)
		let tunnel_bridges = 
			let tunnels = Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:pifdev in
			let bridge_of_tunnel tunnel = 
				try
					let access_pif = Db.Tunnel.get_access_PIF ~__context ~self:tunnel in
					let network = Db.PIF.get_network ~__context ~self:access_pif in
					[ Db.Network.get_bridge ~__context ~self:network ] 
				with _ -> [] in
			List.concat (List.map bridge_of_tunnel tunnels) in

		(* 1. Update corresponding VIF carrier flags *)
		if !Monitor_rrds.pass_through_pif_carrier
		then begin
			try
				let pif_stats=List.find (fun p -> p.pif_name = physical_device_name) pifs in
				(* go from physical interface -> bridge -> vif devices *)
				let n = Netdev.network in
				let physical_bridge = n.Netdev.get_bridge physical_device_name in
				let ifs = List.concat (List.map n.Netdev.intf_list (physical_bridge :: tunnel_bridges)) in

				let set_carrier xs vif = 
					if vif.Monitor.pv
					then
						let device = 
							let frontend = { Device_common.domid = vif.Monitor.domid; kind = Device_common.Vif; devid = vif.Monitor.devid } in
							let backend = { Device_common.domid = 0; kind = Device_common.Vif; devid = vif.Monitor.devid } in
							{ Device_common.backend = backend; frontend = frontend } in
						Device.Vif.set_carrier ~xs device pif_stats.pif_carrier in

				Vmopshelpers.with_xs
					(fun xs ->
						List.iter (set_carrier xs) (List.filter_map Monitor.vif_device_of_string ifs)
					)
							
			with e ->
				debug "Failed to update VIF carrier flags for PIF: %s" (ExnHelper.string_of_exn e)
		end;
		(* 2. Update database *)
		begin
		try
		        let pif_stats=List.find (fun p -> p.pif_name = physical_device_name) pifs in
			let metrics = Db.PIF.get_metrics ~__context ~self:pifdev in
			(* if PIF metrics don't exist then create one: *)
			if not (Db.is_valid_ref __context metrics) then
			  begin
			    let ref = Ref.make() in
			    Db.PIF_metrics.create ~__context ~ref ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~carrier:false
			      ~device_name:"" ~vendor_name:"" ~device_id:"" ~vendor_id:""
			      ~speed:0L ~duplex:false ~pci_bus_path:""
			      ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.) 
			      ~other_config:[];
			    Db.PIF.set_metrics ~__context ~self:pifdev ~value:ref
			  end;
			
			let metrics = Db.PIF.get_metrics ~__context ~self:pifdev in
			let pmr = Db.PIF_metrics.get_record ~__context ~self:metrics in
			let speed_db = Int64.of_int (Netdev.Link.int_of_speed pif_stats.pif_speed) in
			let duplex_db = match pif_stats.pif_duplex with
			  | Netdev.Link.Duplex_full    -> true
			  | Netdev.Link.Duplex_half    -> false
			  | Netdev.Link.Duplex_unknown -> false in
			set_pif_metrics ~__context ~self:metrics
			  ~vendor:pif_stats.pif_vendor_id ~device:pif_stats.pif_device_id ~carrier:pif_stats.pif_carrier
			  ~speed:speed_db ~duplex:duplex_db
			  ~pcibuspath:pif_stats.pif_pci_bus_path ~io_write:pif_stats.pif_tx ~io_read:pif_stats.pif_rx pmr;
			  
		with Not_found -> ()
		end
) pifdevs

let update_all ~__context host_stats =
	(* update monitor events for specified domain  *)
	let hostref = host_stats.host_ref in
	let hostuuid = Db.Host.get_uuid ~__context ~self:hostref in

	update_host_metrics ~__context host_stats;
	update_pifs ~__context hostref host_stats.pifs;
	update_host_cpu ~__context hostref host_stats.pcpus;

	List.iter (fun uuid ->
	  try
	        let vcpus = List.assoc uuid host_stats.vcpus in
		let vifs = List.map snd (List.filter (fun d -> fst d = uuid) host_stats.vifs) in
		let vbds = List.map snd (List.filter (fun d -> fst d = uuid) host_stats.vbds) in
		let memory = try Some (List.assoc uuid host_stats.mem) with _ -> None in

		update_vm_stats ~__context uuid vcpus vbds vifs memory
	  with e ->
	    debug "Caught exception: '%s' (uuid=%s)" (Printexc.to_string e) uuid
	) host_stats.registered
