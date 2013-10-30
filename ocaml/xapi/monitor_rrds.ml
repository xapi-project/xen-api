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
(** RRD maintainence code
 * @group Performance Monitoring
 *)

(**
 * This module is primarily concerned with the lifecycle of RRDs. They
 * are created here, stored to disk, retrieved from disk, and sent
 * amongst the pool.
 * 
 * The entry point in terms of the monitor thread is function
 * 'update_rrds' which is called directly from monitor.ml. This updates
 * the VMs and the hosts RRDs. It also removes any RRDs for VMs that
 * have disappeared (unless they're marked as rebooting) and copies
 * these back to to the master. It's important that as far as possible
 * we don't access the database in this thread - not only because it's
 * potentially expensive as we run every 5 seconds, but also because we
 * want to carry on recording data if the master goes offline.  For the
 * purposes of doing database write (for example, for host memory), we
 * have another thread run in the module Monitor_dbcalls.
 * 
 * These two threads need access to the same data, so the shared stuff
 * is all in rrd_shared.ml, and all protected by 1 big mutex for
 * simplicity.
 * 
 * If the PIFs have changed, or the memory of any VM, or the memory of
 * the host has changed, they are marked as dirty by the update_rrds
 * function via the keys in rrd_shared. If any one of these is true, we
 * signal the monitor_dbcalls thread to wake up via the condition
 * variable in rrd_shared.ml
 * 
 * 
 * RRD lifecycle:
 * 
 * Host RRDs are pulled from the master on boot by dbsync_slave
 * (calling 'pull_rrd' in this module), and pushed out to the master by
 * xapi_fuse which calls 'backup' in this module.
 * 
 * VM RRDs are created afresh whenever a VM is resident that doesn't
 * already have an RRD in the cache (defined in rrd_shared). When a VM
 * is started/resumed/migrated then the RRD is pushed along with it,
 * and replaces whatever is currently in the cache. When the VM
 * disappears, the RRD is streamed back to the master for writing to
 * disk.
 * 
 * 
 * Legacy metrics:
 * 
 * The legacy metrics are updated using the mechanism from Miami/Rio.
 * A large XML blob is constructed and streamed across to the master
 * (or updated locally). This is triggered by a hook on one of the RRAs
 * which happens when the archive gets updated with a new CDP. The hook
 * is on the host rrd, and is added either when the host rrd is received
 * by the http handler, or when the rrd is first created. The trigger
 * essentially marks another boolean in the rrd_shared module, which 
 * causes the monitor_dbcalls thread to wake up and do the marshalling. 
 *
 * 
 * Http handlers:
 * 
 * There are 3 handlers defined: /host_rrd /vm_rrd and /rrd_updates. The
 * first 2 simply retrieve the rrds (a uuid must be supplied to the vm_rrd
 * handler) and the last returns the CDPs in a specified archive between
 * a specified time and now. The first vm_rrd handler accepts both PUT
 * and GET requests (the functions receive_handler and handler below) and
 * is the PUT is used to upload RRDs from the master to the slave when
 * VMs are started/stopped
 *)

open Threadext
open Hashtblext
open Monitor_types
open Stringext
open Listext
open Ds
open Rrd_shared (* Nb this contains the mutex *)

module D = Debug.Debugger(struct let name="monitor_rrds" end)
open D

let step = 5  

let rrd_of_fd fd = 
  let ic = Unix.in_channel_of_descr fd in
  let input = Xmlm.make_input (`Channel ic) in
  Rrd.from_xml input

(** Helper function - path is the path to the file _without the extension .gz_ *)
let rrd_of_gzip path =
  let gz_path = path ^ ".gz" in
  let gz_exists = try let (_: Unix.stats) = Unix.stat gz_path in true with _ -> false in
  if gz_exists then begin
    Unixext.with_file gz_path [ Unix.O_RDONLY ] 0o0
      (fun fd -> Gzip.decompress_passive fd rrd_of_fd)
  end else begin
    (* If this fails, let the exception propagate *)
    Unixext.with_file path [ Unix.O_RDONLY ] 0 rrd_of_fd
  end


let use_min_max = ref false 

let pass_through_pif_carrier = ref false

let update_configuration_from_master () =
  Server_helpers.exec_with_new_task "update_configuration_from_master" (fun __context -> 
    let oc = Db.Pool.get_other_config ~__context ~self:(Helpers.get_pool ~__context) in
    let new_use_min_max =  (List.mem_assoc Xapi_globs.create_min_max_in_new_VM_RRDs oc) && 
      (List.assoc Xapi_globs.create_min_max_in_new_VM_RRDs oc = "true")
    in
	if !use_min_max <> new_use_min_max
    then debug "Updating use_min_max: New value=%b" new_use_min_max;
    use_min_max := new_use_min_max;

    let carrier = (List.mem_assoc Xapi_globs.pass_through_pif_carrier oc) && 
      (List.assoc Xapi_globs.pass_through_pif_carrier oc = "true") in
	if !pass_through_pif_carrier <> carrier
	then debug "Updating pass_through_pif_carrier: New value=%b" carrier;
	pass_through_pif_carrier := carrier
  )

    

(** Here is the only place where RRDs are created. The timescales are fixed. If other timescales
    are required, this could be done externally. The types of archives created are also fixed.
    Currently, we're making 4 timescales of 3 types of archive. This adds up to a total of
    (120+120+168+366)*3 doubles per field, and at 8 bytes per double this is a grand total
    of 18k per field. For a VM with 2 VBDs, 2 VCPUs and 1 VIF, this adds up to 130k of data
    per VM. This is the function where tuning could be done to change this. *)

let timescales = 
  if Xapi_fist.reduce_rra_times then 
    (* These are purely for xenrt testing *)
    [(120,1);
     (20,12);
     (15,24);
     (10,36)]
  else
    [(120,1);     (* 120 values of interval 1 step (5 secs) = 10 mins *)
     (120,12);    (* 120 values of interval 12 steps (1 min) = 2 hours *)
     (168,720);   (* 168 values of interval 720 steps (1 hr) = 1 week *)
     (366,17280)] (* 366 values of interval 17280 steps (1 day) = 1 yr *)

let create_rras use_min_max =
  (* Create archives of type min, max and average and last *)
  Array.of_list 
    (List.flatten 
	(List.map (fun (n,ns) -> 
	  if ns > 1 && use_min_max then
	    [ Rrd.rra_create Rrd.CF_Average n ns 1.0;
	      Rrd.rra_create Rrd.CF_Min n ns 1.0;
	      Rrd.rra_create Rrd.CF_Max n ns 1.0 ]
	  else 
	    [ Rrd.rra_create Rrd.CF_Average n ns 0.5 ]) timescales))

(** Create a rrd *)
let create_fresh_rrd use_min_max dss =
  let rras = create_rras use_min_max in
  let dss = Array.of_list (List.filter_map (fun ds ->
    if ds.ds_default then
      Some (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 ~max:ds.ds_max ~min:ds.ds_min Rrd.VT_Unknown)
    else None) dss)
  in
  let rrd = Rrd.rrd_create dss rras (Int64.of_int step) (Unix.gettimeofday()) in
  rrd

let send_rrd address to_archive uuid rrd =
  debug "Sending RRD for object uuid=%s archiving=%b to address: %s" uuid to_archive address;
  let pool_secret = !Xapi_globs.pool_secret in
  let request = Xapi_http.http_request ~cookie:[ "pool_secret", pool_secret ]
    Http.Put (Constants.rrd_put_uri^"?uuid="^uuid^(if to_archive then "&archive=true" else "")) in
  let open Xmlrpcclient in
  let transport = SSL(SSL.make (), address, !Xapi_globs.https_port) in
  with_transport transport
	  (with_http request
		  (fun (response, fd) ->
			  try
				  Rrd.to_fd rrd fd
			  with e ->
				  debug "Caught exception: %s" (ExnHelper.string_of_exn e);
				  log_backtrace ();
		  )
	  )

(* never called with the mutex held *)
let archive_rrd ?(save_stats_locally = Pool_role.is_master ()) uuid rrd =
  debug "Archiving RRD for object uuid=%s %s" uuid (if save_stats_locally then "to local disk" else "to remote master");
  if save_stats_locally then begin
    try
      (* Stash away the rrd onto disk *)
      let exists =
	try 
	  let (_: Unix.stats) = Unix.stat Xapi_globs.xapi_blob_location in
	  true
	with _ -> false
      in
      if exists then begin
      	Unixext.mkdir_safe Xapi_globs.xapi_rrd_location 0o755;
	let base_filename = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
	Unixext.atomic_write_to_file (base_filename ^ ".gz") 0o644
	  (fun fd -> Gzip.compress fd (Rrd.to_fd rrd));
	Unixext.unlink_safe base_filename (* If there's an uncompressed one hanging around, remove it *)
      end else begin
	debug "No local storage: not persisting RRDs"
      end
    with e ->      
      debug "Caught exception: %s" (ExnHelper.string_of_exn e);
      log_backtrace();
  end else begin
    debug "About to get address";
    (* Stream it to the master to store, or maybe to a host in the migrate case *)
    let master_address = Pool_role.get_master_address () in
    debug "About to send";
    send_rrd master_address true uuid rrd
  end

(* This is where we add the update hook that updates the metrics classes every
   so often. Called with the lock held. *)
let add_update_hook ~__context rrd =
  let host=Helpers.get_localhost ~__context in
  let other_config = Db.Host.get_other_config ~__context ~self:host in
  let timescale = 
    try 
      int_of_string (List.assoc Constants.rrd_update_interval other_config)
    with _ -> 0 
  in
  (* Clear any existing ones *)
  debug "clearing existing update hooks";
  Array.iter (fun rra -> rra.Rrd.rra_updatehook <- None) rrd.Rrd.rrd_rras;
  if timescale > 0 && timescale < 3 then (* Only allow timescales 1 and 2 - that is 5 seconds and 60 seconds respectively *)
    begin
      debug "Timescale OK";
      let (n,ns) = List.nth timescales (timescale-1) in
      debug "(n,ns)=(%d,%d)" n ns;
      let rras = List.filter (fun (_,rra) -> rra.Rrd.rra_pdp_cnt=ns) (Array.to_list (Array.mapi (fun i x -> (i,x)) rrd.Rrd.rrd_rras)) in
      try
	debug "Found some RRAs (%d)" (List.length rras);
	(* Add the update hook to the last RRA at this timescale to be updated. That way we know that all of the
	   RRAs will have been updated when the hook is called. Last here means two things: the last RRA of this
	   timescale, and also that it happens to be (coincidentally) the one with the CF_LAST consolidation function.
	   We rely on this, as well as the first one being the CF_AVERAGE one *)
	let (new_last_rra_idx, last_rra) = List.hd (List.rev rras) in
	let (new_avg_rra_idx, avg_rra) = List.hd rras in
	debug "Got rra - cf=%s row_cnt=%d pdp_cnt=%d" (Rrd.cf_type_to_string last_rra.Rrd.rra_cf) last_rra.Rrd.rra_row_cnt last_rra.Rrd.rra_pdp_cnt;
	full_update_avg_rra_idx := new_avg_rra_idx;
	full_update_last_rra_idx := new_last_rra_idx;
 	last_rra.Rrd.rra_updatehook <- Some (fun _ _ -> full_update := true; Condition.broadcast condition);
      with _ -> ()
    end

(** Cleanup functions *)

(* Called on host shutdown/reboot to send the Host RRD to the master for backup. Note all VMs will have
   been shutdown by now. *)
let send_host_rrd_to_master () = 
  match !host_rrd with 
    | Some rrdi -> 
        debug "sending host RRD to master"; 
        let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
        archive_rrd (Helpers.get_localhost_uuid ()) ~save_stats_locally:false rrd
    | None -> ()

(** Cleanup - called on xapi exit. 
    We save our host RRD and running VM RRDs on the local filesystem and pick them up when we restart. *)
let backup ?(save_stats_locally=true) () =
  debug "backup safe_stats_locally=%b" save_stats_locally;
  let total_cycles = 5 in
  let cycles_tried = ref 0 in
  while !cycles_tried < total_cycles do
    if Mutex.try_lock mutex then begin
      cycles_tried := total_cycles;
      let vrrds = 
        try 
          Hashtbl.fold (fun k v acc -> (k,v.rrd)::acc) vm_rrds []
        with exn ->
          Mutex.unlock mutex;
          raise exn
      in
      Mutex.unlock mutex;
      List.iter (fun (uuid,rrd) -> 
                  debug "Backup: saving RRD for VM uuid=%s to local disk" uuid; 
                  let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrd) in
                  archive_rrd uuid ~save_stats_locally rrd) 
        vrrds;
      match !host_rrd with 
        | Some rrdi -> 
            debug "Backup: saving RRD for host to local disk";
            let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
            archive_rrd (Helpers.get_localhost_uuid ()) ~save_stats_locally rrd
        | None -> ()
    end else begin
      cycles_tried := 1 + !cycles_tried;
      if !cycles_tried >= total_cycles
      then debug "Could not acquire RRD lock, skipping RRD backup"
      else Thread.delay 1.
    end
  done

(** Maybe_remove_rrd - remove an RRD from the local filesystem, if it exists *)
let maybe_remove_rrd uuid =
  let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
  let gz_path = path ^ ".gz" in
  begin 
    try
      Unix.unlink path
    with _ -> ()
  end;
  begin
    try
      Unix.unlink gz_path
    with _ -> ()
  end

(** Migrate_push - used by the migrate code to push an RRD directly to a remote host 
    without going via the master *)
let migrate_push ~__context vm_uuid host =
  let address = Db.Host.get_address ~__context ~self:host in
  let rrdi = Mutex.execute mutex (fun () ->
    let rrdi = Hashtbl.find vm_rrds vm_uuid in
    debug "Sending RRD for VM uuid=%s to remote host for migrate" vm_uuid;
    Hashtbl.remove vm_rrds vm_uuid;
    rrdi) 
  in
  send_rrd address false vm_uuid rrdi.rrd
    
    

(** Push function to push the archived RRD to the appropriate host 
    (which might be us, in which case, pop it into the hashtbl *)
let push_rrd ~__context uuid =
  try
    let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
    let rrd = rrd_of_gzip path in
    debug "Pushing RRD for VM uuid=%s" uuid;
    let vm = Db.VM.get_by_uuid ~__context ~uuid in
		let domid = Db.VM.get_domid ~__context ~self:vm in
    let host = Db.VM.get_resident_on ~__context ~self:vm in
    if host = Helpers.get_localhost ~__context then begin
		Mutex.execute mutex (fun () -> Hashtbl.replace vm_rrds uuid
			{rrd=rrd; dss=[]; domid=(Int64.to_int domid)})
    end else begin
      (* Host might be OpaqueRef:null, in which case we'll fail silently *)
      let address = Db.Host.get_address ~__context ~self:host in
      send_rrd address false uuid (Rrd.copy_rrd rrd)
    end
  with _ -> ()

(* Load an RRD from the local filesystem. Will return an RRD or throw an exception. *)
let load_rrd_from_local_filesystem ~__context uuid =
  debug "Loading RRD from local filesystem for object uuid=%s" uuid;
  let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
  rrd_of_gzip path

(* Fetch an RRD from the master *)
let pull_rrd_from_master ~__context uuid is_host = 
  let pool_secret = !Xapi_globs.pool_secret in
  let address = Pool_role.get_master_address () in
  (* Add in dbsync=true to the query to make sure the master
     doesn't try to redirect here! *)
  let uri = 
    if is_host 
    then Constants.host_rrd_uri
    else Constants.vm_rrd_uri
  in 
  let uri = uri ^"?uuid="^uuid^"&dbsync=true" in    
  let request = Xapi_http.http_request ~cookie:[ "pool_secret", pool_secret ] 
    Http.Get uri in
  let open Xmlrpcclient in
  let transport = SSL(SSL.make (), address, !Xapi_globs.https_port) in
  with_transport transport
	  (with_http request
		  (fun (response, s) ->
			  match response.Http.Response.content_length with
				  | None -> failwith "pull_rrd_from_master needs a content-length"
				  | Some l ->
					  let body = Unixext.really_read_string s (Int64.to_int l) in
					  let input = Xmlm.make_input (`String (0, body)) in
					  debug "Pulled rrd for vm uuid=%s" uuid;
					  Rrd.from_xml input
		  )
	  )

(** Only called from dbsync in two cases:
    1. for the local host after a xapi restart or host restart
    2. for running VMs after a xapi restart
    Note we aren't called looking for running VMs after a host restart.
    We assume that the RRDs were stored locally and fall back to asking the master if we can't find them. *)
let load_rrd ~__context uuid is_host =
  try
    let rrd = 
      try
	let rrd = load_rrd_from_local_filesystem ~__context uuid in
	debug "RRD loaded from local filesystem for object uuid=%s" uuid;
	rrd
      with e ->
	if Pool_role.is_master () then begin
	  info "Failed to load RRD from local filesystem: metrics not available for uuid=%s" uuid;
	  raise e
	end else begin
	  debug "Failed to load RRD from local filesystem for object uuid=%s; asking master" uuid;
	  try
	    let rrd = pull_rrd_from_master ~__context uuid is_host in
	    debug "RRD pulled from master for object uuid=%s" uuid;
	    rrd
	  with e ->
	    info "Failed to fetch RRD from master: metrics not available for uuid=%s" uuid;
	    raise e
	end 
    in
    Mutex.execute mutex 
      (fun () -> 
	  if is_host 
	  then begin
	    add_update_hook ~__context rrd;
			host_rrd := Some {rrd=rrd; dss=[]; domid=0}
		end else
			let vm = Db.VM.get_by_uuid ~__context ~uuid in
			let domid = Db.VM.get_domid ~__context ~self:vm in
			Hashtbl.replace vm_rrds uuid {rrd=rrd; dss=[]; domid=(Int64.to_int domid)})
  with _ -> ()

(** Receive handler, for RRDs being pushed onto us *)
exception Invalid_RRD

let receive_handler (req: Http.Request.t) (bio: Buf_io.t) =
  debug "Monitor_rrds.receive_handler";
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  let fd = Buf_io.fd_of bio in (* fd only used for writing *)
  if not(List.mem_assoc "uuid" query) then begin
    error "HTTP request for RRD lacked 'uuid' parameter";
    Http_svr.headers fd (Http.http_400_badrequest ());
    failwith "Monitor_rrds.receive_handler: Bad request"
  end;
  Xapi_http.with_context ~dummy:true "Receiving VM rrd" req fd
    (fun __context ->
	let uuid = List.assoc "uuid" query in

	(* Check to see if it's a valid uuid for a host or VM *)
	let (ty,domid) =
	  begin
			try
				let vm = Db.VM.get_by_uuid ~__context ~uuid in
				let domid = Db.VM.get_domid ~__context ~self:vm in
				VM uuid,(Int64.to_int domid)
	    with _ -> begin
				try ignore(Db.Host.get_by_uuid ~__context ~uuid); Host,0
	      with _ ->
		Http_svr.headers fd (Http.http_404_missing ());
		failwith (Printf.sprintf "Monitor_rrds.receive_handler: UUID %s neither host nor VM" uuid)
	    end
	  end
	in
	(* Tell client we're good to receive *)
	Http_svr.headers fd (Http.http_200_ok ());

	(* Now we know what sort of RRD it is, read it in and validate it *)
	let rrd = rrd_of_fd fd in

	(* By now we know it's a valid RRD *)
	let to_archive = List.mem_assoc "archive" query in
	if not to_archive
	then begin
	  match ty with
	    | VM uuid -> 
		debug "Receiving RRD for resident VM uuid=%s. Replacing in hashtable" uuid;	  
		Mutex.execute mutex (fun () -> Hashtbl.replace vm_rrds uuid {rrd=rrd; dss=[]; domid=domid})
	    | _ -> raise Invalid_RRD	
	end else begin
	  debug "Receiving RRD for archiving, type=%s" 
	    (match ty with Host -> "Host" | VM uuid -> Printf.sprintf "VM uuid=%s" uuid | _ -> "Unknown");
	  archive_rrd uuid (Rrd.copy_rrd rrd)
	end;

    )

(** Send handler, for sending out requested RRDs *)
let handler (req: Http.Request.t) s =
  debug "Monitor_rrds.handler";
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  if not(List.mem_assoc "ref" query) && not(List.mem_assoc "uuid" query) then begin
    error "HTTP request for RRD lacked 'uuid' parameter";
    failwith "Bad request"
  end;
  Xapi_http.with_context ~dummy:true "Obtaining the RRD statistics" req s
    (fun __context ->
      let uuid = List.assoc "uuid" query in
      (* If the uuid is in our hashtbl, we own the data and we'll just respond with it *)
      try
	let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd (Hashtbl.find vm_rrds uuid).rrd) in
	Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
	Rrd.to_fd rrd s
      with
	| Not_found ->
	    (* If we're not the master, redirect to the master *)
	    if not (Pool_role.is_master ()) then
	      let url = Printf.sprintf "https://%s%s?%s" 
		(Pool_role.get_master_address ()) req.Http.Request.uri (String.concat "&" (List.map (fun (a,b) -> a^"="^b) query)) in
	      Http_svr.headers s (Http.http_302_redirect url);
	    else
	      begin
		(* Three options here - either the VM is resident on a
		   slave, in which case we redirect, or the VM is
		   powered off, or a slave's xapi is in dbsync, in
		   which case we look for a local copy of the rrd and
		   send that off *)
		let _ref = Db.VM.get_by_uuid ~__context ~uuid in
		let host = Db.VM.get_resident_on ~__context ~self:_ref in
		
		(* If the resident_on field is valid, or the request isn't 
		   from dbsync, then redirect *)
		if Db.is_valid_ref __context host &&
		  (not (List.mem_assoc "dbsync" query)) then
		  let address = Db.Host.get_address ~__context ~self:host in
		  let url = Printf.sprintf "https://%s%s?%s" address req.Http.Request.uri (String.concat "&" (List.map (fun (a,b) -> a^"="^b) query)) in
		  Http_svr.headers s (Http.http_302_redirect url);
		else
		  (* it's off, and we're the master, so unarchive the rrd and
		     send it off (if it's there) *)
		  let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
                  let rrd = rrd_of_gzip path in
		  Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
		  Rrd.to_fd rrd s
	      end)

(** Send handler, for sending out requested host RRDs *)
let handler_host (req: Http.Request.t) s =
  debug "Monitor_rrds.handler_host";
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  Xapi_http.with_context ~dummy:true "Obtaining the Host RRD statistics" req s
    (fun __context ->
      (* This is only used by hosts when booting - not for public use! *)
      if List.mem_assoc "dbsync" query then begin
        if not(List.mem_assoc "uuid" query) then begin
          Http_svr.headers s (Http.http_400_badrequest ());
          error "HTTP request for RRD dbsync lacked 'uuid' parameter"
        end else begin
	  let uuid = List.assoc "uuid" query in
	  let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
	  let rrd = rrd_of_gzip path in
	  Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
	  Rrd.to_fd rrd s;
        end
      end else begin
	let rrd = Mutex.execute mutex 
	  (fun () -> 
	    debug "Received request for Host RRD";
	    Rrd.copy_rrd (match !host_rrd with Some rrdi -> rrdi.rrd | None -> failwith "No host RRD available")
	  ) in
	Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
	Rrd.to_fd ~json:(List.mem_assoc "json" query) rrd s
      end)





(** Get as a string an XML document representing the updates since the specified start time *)
let get_host_stats ?(json=false) start interval cfopt host uuid =
  Mutex.execute mutex (fun () -> 
    let prefixandrrds = 
      let vmsandrrds = Hashtbl.fold (fun k v acc -> (k,v)::acc) vm_rrds [] in
      let vmsandrrds = match uuid with None -> vmsandrrds | Some uuid -> List.filter (fun (k,v) -> k=uuid) vmsandrrds in
      let vm_rrds = List.map (fun (k,v) -> ("vm:"^k^":",v.rrd)) vmsandrrds in
      if host then 
	match !host_rrd with
	  | None -> vm_rrds
	  | Some rrdi -> ("host:"^(Helpers.get_localhost_uuid ())^":",rrdi.rrd)::vm_rrds
      else vm_rrds
    in
    Rrd.export ~json prefixandrrds start interval cfopt)

let handler_rrd_updates (req: Http.Request.t) s =
  (* This is commonly-called: not worth logging *)
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  Xapi_http.with_context ~dummy:true "Obtaining the Host RRD statistics" req s
    (fun __context ->
      if not(List.mem_assoc "start" query) then begin
        let headers = Http.http_400_badrequest () in
        Http_svr.headers s headers;
        error "HTTP request for Host RRD statistics lacked the 'start' parameter"
      end else begin
        let start=Int64.of_string (List.assoc "start" query) in
        let cfopt = try Some (Rrd.cf_type_of_string (List.assoc "cf" query)) with _ -> None in
        let interval=try Int64.of_string (List.assoc "interval" query) with _ -> 0L in
        let host=List.mem_assoc "host" query in
        let uuid=try Some (List.assoc "vm_uuid" query) with _ -> None in
        let json=List.mem_assoc "json" query in
        let xml = get_host_stats ~json start interval cfopt host uuid in
        let headers = 	
	  (Http.http_200_ok_with_content 
	       (Int64.of_int (String.length xml))
	       ~version:"1.1" ~keep_alive:false ())
        in
        let headers = if json then headers else (headers@[Http.Hdr.content_type ^ ": text/xml"]) in
        Http_svr.headers s headers;
        ignore(Unix.write s xml 0 (String.length xml))
      end)

let sent_clock_went_backwards_alert = ref false

(* Updates all of the hosts rrds. We are passed a list of uuids that
 * is used as the primary source for which VMs are resident on us.
 * When a new uuid turns up that we haven't got an RRD for in our
 * hashtbl, we create a new one. When a uuid for which we have an RRD
 * for doesn't appear to have any stats this update, we assume that the
 * domain has gone and we stream the RRD to the master. We also have a
 * list of the currently rebooting VMs to ensure we don't accidentally
 * archive the RRD *)
let update_rrds ~__context timestamp dss uuids pifs rebooting_vms paused_vms =
  (* Here we do the synchronising between the dom0 view of the world
     and our Hashtbl. By the end of this execute block, the Hashtbl
     correctly represents the world *)
  let to_send_back = Mutex.execute mutex 
    (fun () -> 
      let out_of_date, by_how_much =
		match !host_rrd with 
		| None -> false, 0.
		| Some rrdi -> rrdi.rrd.Rrd.last_updated > timestamp, abs_float (timestamp -. rrdi.rrd.Rrd.last_updated)
      in
      
	  if out_of_date then begin
		warn "Clock just went backwards by %.0f seconds: RRD data may now be unreliable" by_how_much;
		if not(!sent_clock_went_backwards_alert) then begin
		  Xapi_alert.add ~name:Api_messages.host_clock_went_backwards ~priority:Api_messages.host_clock_went_backwards_priority 
			  ~cls:`Host ~obj_uuid:(Xapi_inventory.lookup Xapi_inventory._installation_uuid) ~body:"";
		  sent_clock_went_backwards_alert := true; (* send at most one *)
		end;
	  end;

	let registered = Hashtbl.fold (fun k _ acc -> k::acc) vm_rrds [] in
	let my_vms = uuids in
	let gone_vms = List.filter (fun vm -> not (List.mem_assoc vm my_vms)) registered in
	let to_send_back = List.map (fun uuid -> 
	  let elt = (uuid,Hashtbl.find vm_rrds uuid) in
	  elt) gone_vms in

	(* Don't send back rebooting VMs! *)
	let to_send_back = List.filter (fun (uuid,_) ->
	  let rebooting = (List.exists (fun uuid' -> uuid=uuid') rebooting_vms) in
	  if rebooting then 
	    debug "Ignoring disappeared VM which is rebooting";
	  not rebooting) to_send_back 
	in

	List.iter (fun (uuid,_) ->  Hashtbl.remove vm_rrds uuid) to_send_back;

	let do_vm (vm_uuid,domid) =
	  try
	    let dss = List.filter_map (fun (ty,ds) -> match ty with | VM x -> if x=vm_uuid then Some ds else None | _ -> None) dss in

	    begin
	      try
		(* First, potentially update the rrd with any new default dss *)
		let rrdi = Hashtbl.find vm_rrds vm_uuid in
		let default_dss = List.filter (fun ds -> ds.ds_default) dss in
		let current_dss = Rrd.ds_names rrdi.rrd in
		let new_defaults = List.filter (fun ds -> not (List.mem ds.ds_name current_dss)) default_dss in
		let rrd = 
		  if List.length new_defaults > 0 then
		    let rrd = List.fold_left (fun rrd ds -> Rrd.rrd_add_ds rrd (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)) rrdi.rrd new_defaults in
				Hashtbl.replace vm_rrds vm_uuid {rrd=rrd; dss=dss; domid=domid};
		    rrd
		  else
		    rrdi.rrd
		in
		(* CA-34383:
		 * Memory updates from paused domains serve no useful purpose.
		 * During a migrate such updates can also cause undesirable
		 * discontinuities in the observed value of memory_actual.
		 * Hence we ignore changes from paused domains:
		 *)
		if not (List.mem vm_uuid paused_vms) then begin
		  (* Check whether the memory ds has changed since last update *)
		  let last_values = Rrd.get_last_ds_values rrd in
		  let changed = 
			begin try
			  let old_mem = List.assoc "memory" last_values in
			  let cur_mem_ds = List.find (fun ds -> ds.ds_name = "memory") dss in
			  let cur_mem = cur_mem_ds.ds_value in
			  cur_mem <> old_mem
			with _ -> true end in
		  if changed then
			let vm_ref = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
			if (Db.VM.get_resident_on ~__context ~self:vm_ref = Helpers.get_localhost ~__context)
			then dirty_memory := StringSet.add vm_uuid !dirty_memory;
		  
		  (* Now update the rras/dss *)
			Rrd.ds_update_named rrd timestamp (domid <> rrdi.domid)
			  (List.map (fun ds -> (ds.ds_name,(ds.ds_value,ds.ds_pdp_transform_function))) dss);
		  rrdi.dss <- dss;
			rrdi.domid <- domid;
		end
	      with
		| Not_found ->
		    debug "Creating fresh RRD for VM uuid=%s" vm_uuid;

		    let rrd = create_fresh_rrd (!use_min_max) dss in
				Hashtbl.replace vm_rrds vm_uuid {rrd=rrd; dss=dss; domid=domid}
		| e ->
		    raise e
	    end
	  with e -> 
	    debug "Error: caught exception %s" (ExnHelper.string_of_exn e);
	    log_backtrace ()
	in

	List.iter do_vm uuids;

	(* Check to see if any of the PIFs have changed *)
	if pifs <> !pif_stats then
	  List.iter (fun pif -> 
	    if (try pif <> List.find (fun p -> p.pif_name = pif.pif_name) !pif_stats with _ -> true) then
	      dirty_pifs := StringSet.add pif.pif_name !dirty_pifs) pifs;

	pif_stats := pifs;

	let host_dss = List.filter_map (fun (ty,ds) -> match ty with | Host -> Some ds | _ -> None) dss in
	begin
	  match !host_rrd with 
	    | None -> 
		begin
		  debug "Creating fresh RRD for localhost";
		  let rrd = create_fresh_rrd true host_dss in (* Always always create localhost rrds with min/max enabled *)
		  add_update_hook ~__context rrd;
			host_rrd := Some {rrd=rrd; dss=host_dss; domid=0}
		end
	    | Some rrdi -> 
		rrdi.dss <- host_dss;
		let default_dss = List.filter (fun ds -> ds.ds_default) host_dss in
		let current_dss = Rrd.ds_names rrdi.rrd in
		let new_defaults = List.filter (fun ds -> not (List.mem ds.ds_name current_dss)) default_dss in
		let rrd = 
		  if List.length new_defaults > 0 then
		    let rrd = List.fold_left (fun rrd ds -> Rrd.rrd_add_ds rrd (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)) rrdi.rrd new_defaults in
				host_rrd := Some {rrd=rrd; dss=host_dss; domid=0};
		    rrd
		  else
		    rrdi.rrd
		in

		let last_values = Rrd.get_last_ds_values rrd in
		let changed = 
		  try
		    let old_mem_tot = List.assoc "memory_total_kib" last_values in
		    let old_mem_free = List.assoc "memory_free_kib" last_values in
		    let cur_mem_tot_ds = List.find (fun ds -> ds.ds_name = "memory_total_kib") host_dss in
		    let cur_mem_free_ds = List.find (fun ds -> ds.ds_name = "memory_free_kib") host_dss in
		    let cur_mem_tot = cur_mem_tot_ds.ds_value in
		    let cur_mem_free = cur_mem_free_ds.ds_value in
		    cur_mem_tot <> old_mem_tot || cur_mem_free <> old_mem_free
		  with _ -> true
		in
		if changed then begin
		  dirty_host_memory := true
		end;

		Rrd.ds_update_named rrd timestamp false
		  (List.map (fun ds -> (ds.ds_name,(ds.ds_value,ds.ds_pdp_transform_function))) host_dss)
	end;

	(* If we've got something different to worry about then wake up the monitor_dbcalls thread *)
	if (not (StringSet.is_empty !dirty_pifs)) || (not (StringSet.is_empty !dirty_memory)) || (!dirty_host_memory) then
	  Condition.broadcast condition;

	to_send_back
    )
  in
  
  List.iter (fun (uuid,rrd) -> debug "Sending back RRD for VM uuid=%s" uuid; archive_rrd uuid rrd.rrd) to_send_back
      

let query_possible_dss rrdi =
  let enabled_dss = Rrd.ds_names rrdi.rrd in
  List.map (fun ds -> {API.data_source_name_label=ds.ds_name;
		       API.data_source_name_description=ds.ds_description;
		       API.data_source_enabled=(List.mem ds.ds_name enabled_dss);
		       API.data_source_standard=ds.ds_default;
		       API.data_source_units=ds.ds_units;
		       API.data_source_min=ds.ds_min;
		       API.data_source_max=ds.ds_max;
		       API.data_source_value=0.0;}) rrdi.dss

let add_ds rrdi ds_name =
  let ds = List.find (fun ds -> ds.ds_name=ds_name) rrdi.dss in
  Rrd.rrd_add_ds rrdi.rrd (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)
  
let query_possible_vm_dss uuid =
  Mutex.execute mutex (fun () ->
    let rrdi = Hashtbl.find vm_rrds uuid in
    query_possible_dss rrdi)

let add_vm_ds uuid domid ds_name =
  Mutex.execute mutex (fun () ->
    let rrdi = Hashtbl.find vm_rrds uuid in
    let rrd = add_ds rrdi ds_name in
		Hashtbl.replace vm_rrds uuid {rrd=rrd; dss=rrdi.dss; domid=domid})

let query_vm_dss uuid ds_name =
  Mutex.execute mutex (fun () ->
    let rrdi = Hashtbl.find vm_rrds uuid in
    Rrd.query_named_ds rrdi.rrd ds_name Rrd.CF_Average)

let forget_vm_ds uuid ds_name =
  Mutex.execute mutex (fun () ->
    let rrdi = Hashtbl.find vm_rrds uuid in
    let rrd = rrdi.rrd in
    Hashtbl.replace vm_rrds uuid {rrdi with rrd=Rrd.rrd_remove_ds rrd ds_name})

let query_possible_host_dss () =
  Mutex.execute mutex (fun () ->
    match !host_rrd with
      | None -> []
      | Some rrdi -> query_possible_dss rrdi)

let add_host_ds ds_name =
  Mutex.execute mutex (fun () ->
    match !host_rrd with
      | None -> ()
      | Some rrdi -> 
	  let rrd = add_ds rrdi ds_name in
	  host_rrd := Some {rrdi with rrd=rrd})

let query_host_dss ds_name =
  Mutex.execute mutex (fun () ->
    match !host_rrd with
      | None -> failwith "No DS!"
      | Some rrdi -> 
	  Rrd.query_named_ds rrdi.rrd ds_name Rrd.CF_Average)

let forget_host_ds ds_name =
  Mutex.execute mutex (fun () ->
    match !host_rrd with
      | None -> ()
      | Some rrdi -> 
	  host_rrd := Some {rrdi with rrd=Rrd.rrd_remove_ds rrdi.rrd ds_name})
