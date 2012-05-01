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
(*open Rrd_shared*) (* Nb this contains the mutex *)

let step = 5

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

(** Send rrds to a remote host. If the host is on another pool, you
    must pass the session_id parameter, and optionally the __context. *)
let send_rrd ?session_id address to_archive uuid rrd =
	debug "Sending RRD for object uuid=%s archiving=%b to address: %s"
		uuid to_archive address;
	let arch_query = if to_archive then ["archive", "true"] else [] in
	let sid_query = match session_id with
		| None -> [] | Some id -> [ "session_id", Ref.string_of id ] in
	let query = sid_query @ arch_query @ [ "uuid", uuid ] in
	let cookie = if sid_query = []
		then [ "pool_secret", !Xapi_globs.pool_secret ] else [] in
	let request =
		Xapi_http.http_request ~query ~cookie Http.Put Constants.rrd_put_uri in
	let open Xmlrpc_client in
		let transport = SSL(SSL.make (), address, !Xapi_globs.https_port) in
		with_transport transport
			(with_http request (fun (response, fd) ->
				try Rrd.to_fd rrd fd
				with e ->
					debug "Caught exception: %s" (ExnHelper.string_of_exn e) ;
					log_backtrace () ))

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




(** Cleanup functions *)

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
  let open Xmlrpc_client in
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
	    host_rrd := Some {rrd=rrd; dss=[]} 
	  end else Hashtbl.replace vm_rrds uuid {rrd=rrd; dss=[]})
  with _ -> ()

module StringSet = Set.Make(String)
let pif_stats : Monitor_types.pif list ref = ref []
let dirty_pifs = ref StringSet.empty
let dirty_memory = ref StringSet.empty
let dirty_host_memory = ref false

(** Receive handler, for RRDs being pushed onto us *)
exception Invalid_RRD

let receive_handler (req: Http.Request.t) (bio: Buf_io.t) _ = ()
(* TODO XXX FIXME
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
	let ty = 
	  begin
	    try ignore(Db.VM.get_by_uuid ~__context ~uuid); VM uuid
	    with _ -> begin
	      try ignore(Db.Host.get_by_uuid ~__context ~uuid); Host
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
		Mutex.execute mutex (fun () -> Hashtbl.replace vm_rrds uuid {rrd=rrd; dss=[]})
	    | _ -> raise Invalid_RRD	
	end else begin
	  debug "Receiving RRD for archiving, type=%s" 
	    (match ty with Host -> "Host" | VM uuid -> Printf.sprintf "VM uuid=%s" uuid | _ -> "Unknown");
	  archive_rrd uuid (Rrd.copy_rrd rrd)
	end;

    )
*)

(** Send handler, for sending out requested RRDs *)
let handler (req: Http.Request.t) s _ =
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
		  Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false () @ ["Access-Control-Allow-Origin: *"]);
		  Rrd.to_fd rrd s
	      end)

(** Send handler, for sending out requested host RRDs *)
let handler_host (req: Http.Request.t) s _ =
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
	Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false () @ ["Access-Control-Allow-Origin: *"]);
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

let handler_rrd_updates (req: Http.Request.t) s _ =
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
	let gone_vms = List.filter (fun vm -> not (List.mem vm my_vms)) registered in
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

	let do_vm vm_uuid =
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
		    Hashtbl.replace vm_rrds vm_uuid {rrd=rrd; dss=dss};
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
			dirty_memory := StringSet.add vm_uuid !dirty_memory;
		  
		  (* Now update the rras/dss *)
		  Rrd.ds_update_named rrd timestamp 
			  (List.map (fun ds -> (ds.ds_name,(ds.ds_value,ds.ds_pdp_transform_function))) dss);
		  rrdi.dss <- dss;
		end
	      with
		| Not_found ->
		    debug "Creating fresh RRD for VM uuid=%s" vm_uuid;

		    let rrd = create_fresh_rrd (!use_min_max) dss in
		    Hashtbl.replace vm_rrds vm_uuid {rrd=rrd; dss=dss}
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
		  host_rrd := Some {rrd=rrd; dss=host_dss}
		end
	    | Some rrdi -> 
		rrdi.dss <- host_dss;
		let default_dss = List.filter (fun ds -> ds.ds_default) host_dss in
		let current_dss = Rrd.ds_names rrdi.rrd in
		let new_defaults = List.filter (fun ds -> not (List.mem ds.ds_name current_dss)) default_dss in
		let rrd = 
		  if List.length new_defaults > 0 then
		    let rrd = List.fold_left (fun rrd ds -> Rrd.rrd_add_ds rrd (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)) rrdi.rrd new_defaults in
		    host_rrd := Some {rrd=rrd; dss=host_dss};
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

		Rrd.ds_update_named rrd timestamp 
		  (List.map (fun ds -> (ds.ds_name,(ds.ds_value,ds.ds_pdp_transform_function))) host_dss)
	end;

	(* If we've got something different to worry about then wake up the monitor_dbcalls thread *)
(* TODO FIXME XXX: temporarily disabled broadcasting.
	if (not (StringSet.is_empty !dirty_pifs)) || (not (StringSet.is_empty !dirty_memory)) || (!dirty_host_memory) then
	  Condition.broadcast condition;
*)
	to_send_back
    )
  in
  
  List.iter (fun (uuid,rrd) -> debug "Sending back RRD for VM uuid=%s" uuid; archive_rrd uuid rrd.rrd) to_send_back
