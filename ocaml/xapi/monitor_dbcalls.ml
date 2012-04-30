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
(** Monitor DB calls
 * @group Performance Monitoring
 *)

(** Here is where all of the calls that touch the database live. There
 * is a thread that sits waiting for the monitor_rrd module (or
 * xapi_pif module) to wake it up so that it writes into the
 * database. We don't particularly care if the db calls block for a
 * while as if the master isn't responding, your problems are larger
 * than having an out-of-date value for the host's memory.
 * 
 * The key thing in this module is to know when to push data from the
 * cache into the database. This is usually triggered by edge detection
 * code in monitor_rrds which notices when a value has changed between
 * the last call and the current one. This falls apart when e.g. we've
 * gathered PIF info for an interface that isn't in the database - we
 * try try to update the database with the information and the object
 * isn't there. We don't try to re-issue the db call, so if the PIF is
 * later introduced, it will never get updated with the cached
 * information. In this particular case, we fix this by forcing an
 * update from xapi_pif.ml - but it's important to keep this in mind
 * because it will be quite easy for this code to get out of sync.
 *)

module D = Debug.Debugger(struct let name="monitor_dbcalls" end)
open D

open Rrd_shared
open Monitor_types
open Stringext
open Listext
open Db_filter_types

(** Called with the Rrd_shared mutex held *)
(* This is the legacy update function that constructs a bunch of xml and
 * copies it to the master to update all of the metrics objects. Not used
 * by default. *)
let full_update_fn () =
	()
(* XXX TODO FIXME : temporarily disabled legacy RRD code
  let i64_of_float f =
    match classify_float f with
      | FP_nan -> 0L
      | FP_infinite -> 0L
      | _ -> Int64.of_float f
  in

  let avg_rra_idx = Rrdd.Deprecated.get_full_update_avg_rra_idx () in
  let last_rra_idx = Rrdd.Deprecated.get_full_update_last_rra_idx () in


  (* The following function constructs a bolus of data from the shared
     rrds in order to pass it over the wire to the master. There
     shouldn't be anything here that will take time to process, and by
     the end of the function all of the dirty bits should be marked as
     clean. Whatever happens, we need to unlock the mutex at the end of
     this! *)
  let host_stats = 
    Pervasiveext.finally (fun () ->
      (* We go through the vm rrds and regenerate a host_stats structure *)
      let rrd = match !host_rrd with Some r -> r.rrd | None -> failwith "No host rrd!" in

      let timestamp = rrd.Rrd.last_updated in
      
      let (total_kib,free_kib,pcpus,pifs) = 
	match !host_rrd with 
	  | None -> (0L,0L,{pcpus_usage=[||]},[])
	  | Some rrdi ->
	      let rrd = rrdi.rrd in
	      let values = Rrd.get_last_values rrd avg_rra_idx in 
	      let lastvalues = Rrd.get_last_values rrd last_rra_idx in
	      let total_kib = try i64_of_float (List.assoc "memory_total_kib" lastvalues) with _ -> 0L in
	      let free_kib = try i64_of_float (List.assoc "memory_free_kib" lastvalues) with _ -> 0L in
	      let cpus = List.sort (fun (c1,_) (c2,_) -> 
		let n1 = Scanf.sscanf c1 "cpu%d" (fun n -> n) in
		let n2 = Scanf.sscanf c2 "cpu%d" (fun n -> n) in
		compare n1 n2) (List.filter (fun (s,_) -> String.startswith "cpu" s) values)
	      in
	      let cpus = Array.of_list (List.map (fun (n,v) -> v) cpus) in
	      let pifs = List.filter (fun (n1,_) -> String.startswith "pif_" n1) values in
	      let pif_names = List.setify (List.map (fun (s,_) -> List.nth (String.split '_' s) 1) pifs) in
	      let pifs = List.filter_map (fun name ->
		try
		  let pif_stats = List.find (fun pif -> pif.pif_name=name) !pif_stats in
		  Some {pif_stats with 
		    pif_tx=List.assoc ("pif_"^name^"_tx") values;
		    pif_rx=List.assoc ("pif_"^name^"_rx") values;
		  }
		with _ -> None) pif_names
	      in
	      (total_kib, free_kib, {pcpus_usage=cpus}, pifs)
      in	
      
      let kvs = Hashtbl.fold (fun k v acc -> (k,Rrd.get_last_values v.rrd avg_rra_idx)::acc) vm_rrds [] in
      
      let vbds = List.flatten (List.map (fun (uuid,values) ->
	let vbds = List.filter (fun (s,_) -> String.startswith "vbd_" s) values in
	let vbds_devices = List.setify (List.map (fun (s,_) -> List.nth (String.split '_' s) 1) vbds) in
	let vbds = List.filter_map 
	  (fun device ->
	    try
			(* NB we only get stats from PV devices *)
	      let device_id = Device_number.to_xenstore_key (Device_number.of_string false device) in
	      let read = List.assoc ("vbd_"^device^"_read") values in
	      let write = List.assoc ("vbd_"^device^"_write") values in
	      Some (uuid,{vbd_device_id=device_id;
			  vbd_io_read=read;
			  vbd_io_write=write;
			  vbd_raw_io_read=0L;
			  vbd_raw_io_write=0L})
	    with _ -> 
	      error "Bizarre error in monitor_rrds.ml - key dump: ";
	      List.iter (fun (k,v) -> error "key: %s" k) vbds;
	      None
	  ) vbds_devices in
	vbds) kvs)
      in
      
      let vifs = List.flatten (List.map (fun (uuid,values) ->
	let vifs = List.filter (fun (s,_) -> String.startswith "vif_" s) values in
	let vif_devices = List.setify (List.map (fun (s,_) -> List.nth (String.split '_' s) 1) vifs) in
	let vifs = List.map (fun device ->
	  let dev=int_of_string device in
	  let name="" in
	  let tx = List.assoc ("vif_"^device^"_tx") values in
	  let rx = List.assoc ("vif_"^device^"_rx") values in
	  (uuid,{vif_n=dev;
		 vif_name=name;
		 vif_tx=tx;
		 vif_rx=rx;
		 vif_raw_tx=0L;
		 vif_raw_rx=0L;
		 vif_raw_tx_err=0L;
		 vif_raw_rx_err=0L;})
	) vif_devices in
	vifs) kvs)
      in
      
      let vcpus = List.map (fun (uuid,values) ->
	let vcpus = List.filter (fun (s,_) -> String.startswith "cpu" s) values in
	let cpunums = List.map (fun (s,v) -> (Scanf.sscanf s "cpu%d" (fun x -> x),v)) vcpus in
	(uuid,{vcpu_sumcpus=0.0; vcpu_vcpus=Array.of_list (List.map snd (List.sort (fun a b -> compare (fst a) (fst b)) cpunums));
	       vcpu_rawvcpus=[| |]; vcpu_cputime=0L})) kvs
      in
      
      let mem = [] in (* This gets updated below instead *)
      
      let registered = List.map fst kvs in

      (* Now clear all the dirty bits *)
      dirty_memory := StringSet.empty;
      dirty_pifs := StringSet.empty;
      full_update := false;
      dirty_host_memory := false;

      {timestamp=timestamp;
       host_ref=Ref.null;
       total_kib=total_kib;
       free_kib=free_kib;
       pifs=pifs;
       pcpus=pcpus;
       vbds=vbds;
       vifs=vifs;
       vcpus=vcpus;
       mem=mem;
       registered=registered;})
      
      (fun () -> Mutex.unlock mutex);
  in

  (* This is the bit that might block for some time, but by now we've released the mutex *)
  Server_helpers.exec_with_new_task "updating host stats"
    (fun __context ->   
      let host = Helpers.get_localhost ~__context in
      let host_stats = {host_stats with host_ref=host} in

      if Pool_role.is_master () then 
	Monitor_master.update_all ~__context host_stats (* read stats locally and update locally *)
      else
	ignore (Master_connection.execute_remote_fn (Xml.to_string_fmt (Monitor_transfer.marshall host_stats)) Constants.remote_stats_uri);
    )
(* End of legacy function *)
*)



(* This function updates anything that's marked as dirty in the
 * rrd_shared module. This is the way that the database fields get
 * updated with information from the rrds.  (and other info that's not
 * in rrds, e.g. the PIF status) 
 *)
let pifs_and_memory_update_fn () =
()
(* TODO FIXME XXX
  let value_to_int64 v =
    match v with 
      | Rrd.VT_Int64 x -> x
      | Rrd.VT_Float x -> Int64.of_float x 
      | Rrd.VT_Unknown -> failwith "No last value!"
  in

  (* Nb, this bit must not take much time to execute - we're holding the lock! *)
  let (memories,host_memories,pifs,bonds) =
    Pervasiveext.finally (fun () ->
      let memories = 
	try
	  StringSet.fold (fun uuid acc -> 
			    let vm_rrd = (Hashtbl.find vm_rrds uuid).rrd in
			    let ds_values = Rrd.get_last_ds_values vm_rrd in
			    let memory = List.assoc "memory" ds_values in
			    try
			      let memi = value_to_int64 memory in
			      (uuid,memi)::acc
			    with _ -> acc) !dirty_memory [] 
	with e -> (error "Unexpected exn in memory computation: %s" (Printexc.to_string e); log_backtrace(); raise e)
      in
      let pifs = List.filter (fun pif -> StringSet.mem pif.pif_name !dirty_pifs) !pif_stats in
	  (* let's copy the list to avoid blocking during the update *)
	  let bonds = Monitor.copy_bonds_status () in
      let host_memories = if !dirty_host_memory then begin
	try
	  match !host_rrd with None -> None | Some rrdi ->
	    let ds_values = Rrd.get_last_ds_values rrdi.rrd in
	    try
	      let bytes_of_kib x = Int64.shift_left x 10 in
	      let memory_free = value_to_int64 (List.assoc "memory_free_kib" ds_values) in
	      let memory_tot = value_to_int64 (List.assoc "memory_total_kib" ds_values) in
	      Some (bytes_of_kib memory_free,bytes_of_kib memory_tot) 
	    with _ -> None
	with e -> (error "Unexpected exn in host computation: %s" (Printexc.to_string e); log_backtrace(); raise e)
      end else None
      in
	  (memories,host_memories,pifs,bonds))
      (fun () -> 
	dirty_pifs := StringSet.empty;
	dirty_memory := StringSet.empty;
	dirty_host_memory := false;
	Mutex.unlock mutex)
  in
  
  (* This is the bit that might block for some time, but by now we've released the mutex *)
  Server_helpers.exec_with_new_task "updating VM_metrics.memory_actual fields and PIFs"
    (fun __context ->       	
      let host = Helpers.get_localhost ~__context in
      List.iter (fun (uuid,memory) ->
	let vm = Db.VM.get_by_uuid ~__context ~uuid in
	let vmm = Db.VM.get_metrics ~__context ~self:vm in
	Db.VM_metrics.set_memory_actual ~__context ~self:vmm ~value:memory) memories;
      Monitor_master.update_pifs ~__context host pifs;
      match host_memories with None -> () | Some (free,total) ->
	let localhost = Helpers.get_localhost ~__context in
	let metrics = Db.Host.get_metrics ~__context ~self:localhost in
	Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total;
	Db.Host_metrics.set_memory_free ~__context ~self:metrics ~value:free;	
	List.iter (fun (bond,links_up) ->
	  let my_bond_pifs = Db.PIF.get_records_where ~__context
		~expr:(And (And (Eq (Field "host", Literal (Ref.string_of localhost)),
						 Not (Eq (Field "bond_master_of", Literal "()"))),
					Eq(Field "device", Literal bond))) in
	  let my_bonds = List.map (fun (_, pif) -> List.hd pif.API.pIF_bond_master_of) my_bond_pifs in
	  if(List.length my_bonds) <> 1 then
		debug "Error: bond %s cannot be found" bond
	  else
		Db.Bond.set_links_up ~__context ~self:(List.hd my_bonds)
		  ~value:(Int64.of_int links_up)) bonds
    )
*)


let monitor_dbcall_thread () =
	()
(* TODO XXX FIXME : temporarily disabled the monitor db thread
    while true do 
      Mutex.lock mutex;
	  while (StringSet.is_empty !dirty_memory) && (StringSet.is_empty !dirty_pifs) && (not !full_update) && (not !dirty_host_memory)
		(*XXX FIXME && ((List.length !Monitor.bonds_status_update) = 0)*) do
        Condition.wait condition mutex
      done;
      
      try
	(* Mutex locked at this point - gather what we need for the update, then do it *)
	if !full_update then
	  (* A full update is required - this does everything *)
	  full_update_fn ()
	else 
	  pifs_and_memory_update_fn ()
      with e ->
	debug "monitor_dbcall_thread would have died from: %s; restarting in 30s" (ExnHelper.string_of_exn e);
	Thread.delay 30.
    done
*)
