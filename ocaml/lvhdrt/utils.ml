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
let stdout_m = Mutex.create () 

let debug (fmt: ('a , unit, string, unit) format4) =
  (* Convert calendar time, x, to tm in UTC *)
  let of_float x = 
    let time = Unix.gmtime x in
    Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
      (time.Unix.tm_year+1900)
      (time.Unix.tm_mon+1)
      time.Unix.tm_mday
      time.Unix.tm_hour
      time.Unix.tm_min
      time.Unix.tm_sec in

  Threadext.Mutex.execute stdout_m
    (fun () ->
       Printf.kprintf (fun s -> Printf.printf "%s [%d] %s\n" (of_float (Unix.gettimeofday ())) (Thread.id (Thread.self ())) s; flush stdout) fmt 
    )



open Client
open Lvhdrt_exceptions

let is_lvhd t =
	t = "lvm" || t = "lvmohba" || t = "lvmoiscsi"

(* find shared lvhd storage *)
let find_shared_lvhd_sr rpc session =
  let srs = Client.SR.get_all_records rpc session in
  let lvhds = List.filter (fun (sr_ref,sr_rec) -> is_lvhd sr_rec.API.sR_type && sr_rec.API.sR_shared) srs in
  try
    fst (List.hd lvhds)
  with _ ->
    raise (Test_error "No shared LVM SR present!")

let find_lvhd_sr rpc session =
  let srs = Client.SR.get_all_records rpc session in
  let lvhds = List.filter (fun (sr_ref,sr_rec) -> is_lvhd sr_rec.API.sR_type) srs in
  try
    fst (List.hd lvhds)
  with _ ->
    raise (Test_error "No LVM SR present!")


let get_my_vdis rpc session sr =
  let vdis = Client.VDI.get_all_records rpc session in
  let sr_vdis = List.filter (fun (vdi_ref, vdi_rec) -> vdi_rec.API.vDI_SR=sr) vdis in
  sr_vdis


let get_master rpc session =
  let pool = List.hd (Client.Pool.get_all rpc session) in
  let master = Client.Pool.get_master rpc session pool in
  master

(* return the list of all control domains of the pool *)
let get_all_control_domains rpc session =
  let vms = Client.VM.get_all_records rpc session in
  List.filter (fun (vm,vmr) -> vmr.API.vM_is_control_domain) vms
  
let get_control_domain rpc session =
  let master = get_master rpc session in
  let dom0 = List.filter (fun (vm,vmr) -> vmr.API.vM_resident_on=master) (get_all_control_domains rpc session) in
  List.hd dom0

(* Management of the test succes/failure *)
type concurrent_test = { 
	continue : unit -> bool;
	success : unit -> unit;
	failure : unit -> unit;
	is_success : unit -> bool }

let manage_concurrent_tests () =
	let status = ref None in
	let lock = Mutex.create () in
	let continue () =
		Mutex.lock lock;
		let b = !status = None in
		Mutex.unlock lock;
		b
	and success () =
		Mutex.lock lock;
		status := Some true;
		Mutex.unlock lock
	and failure () =
		Mutex.lock lock;
		status := Some false;
		Mutex.unlock lock
	and is_success () =
		Mutex.lock lock;
		let r = match !status with
			| Some true -> true
			| _ -> false
		in
		Mutex.unlock lock;
		r
	in
	{ continue = continue;
	  success = success;
	  failure = failure;
	  is_success = is_success }

(* Remotely create fistpoint *)
let with_fistpoint rpc session host name f x =
  let (_: string) = Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"mkfistpoint" 
       ~args:[("fist_point",name)] in
  Pervasiveext.finally 
    (fun () -> f x) 
    (fun () ->
      Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"rmfistpoint" 
       ~args:[("fist_point",name)])

(* Copied from attach_helpers: *)
let timeout = 300. (* 5 minutes, should never take this long *)

(** Attempt an unplug, and if it fails because the device is in use, wait for it to 
    detach by polling the currently-attached field. *)
let safe_unplug rpc session_id self = 
  try
    Client.VBD.unplug rpc session_id self
  with 
  | Api_errors.Server_error(error, _) as e when error = Api_errors.device_detach_rejected ->
    debug "safe_unplug caught device_detach_rejected: polling the currently_attached flag of the VBD";
    let start = Unix.gettimeofday () in
    let unplugged = ref false in
    while not(!unplugged) && (Unix.gettimeofday () -. start < timeout) do
      Thread.delay 5.;
      unplugged := not(Client.VBD.get_currently_attached rpc session_id self)
    done;
    if not(!unplugged) then begin
      debug "Timeout waiting for dom0 device to be unplugged";
      raise e
    end
  | Api_errors.Server_error(error, _) when error = Api_errors.device_already_detached ->
      debug "safe_unplug caught device_already_detached: ignoring"

(* Attach a vdi to dom0 and call the function f with the device name *)
let with_attached_vdi ?dom0 ?(mode=`RW) rpc session vdi f =
  let dom0 = match dom0 with
     | None -> fst (get_control_domain rpc session)
     | Some d -> d in

  let rec getvbd n =
    let devices = Client.VM.get_allowed_VBD_devices ~rpc ~session_id:session ~vm:dom0 in

    try
      let vbd = Client.VBD.create ~rpc ~session_id:session ~vM:dom0 ~vDI:vdi ~userdevice:(List.hd devices) 
	~bootable:false ~mode ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:""
	~qos_algorithm_params:[] in
      vbd 
    with e ->
      (* There's a race between xapi telling us a device to use and us trying to use it, so retry here *)
      if n>1 then raise e else getvbd (n+1)
  in

  let vbd=getvbd 0 in

  Client.VBD.plug ~rpc ~session_id:session ~self:vbd;

  let real_device = Client.VBD.get_device ~rpc ~session_id:session ~self:vbd in
  
  Pervasiveext.finally 
    (fun () -> f real_device vbd)
    (fun () ->
       safe_unplug rpc session vbd;
       Client.VBD.destroy ~rpc ~session_id:session ~self:vbd)

(* Create a new VDI; call the function f with the VDI; destroy the VDI *)
let with_new_vdi rpc session sr size name_label name_description f =
  (* Create VDI *)
  let vdi = Client.VDI.create ~rpc ~session_id:session ~name_label
    ~name_description ~sR:sr ~virtual_size:size ~_type:`ephemeral (*!*)
    ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[]
    ~sm_config:[] ~tags:[] in

  Pervasiveext.finally
    (fun () -> f vdi)
    (fun () -> Client.VDI.destroy ~rpc ~session_id:session ~self:vdi)

let time f x =
  let before = Unix.gettimeofday () in
  let result = f x in
  let after = Unix.gettimeofday () in
  (result, after -. before)

(* Clone an existing VM, pass it to the function and destroy it on the way out *)
let with_sacrificial_vm rpc session f =
	let sr = find_lvhd_sr rpc session in

	(* Shamefully stolen from quicktest *)
	let cli_cmd args =
		debug "$ xe %s" (String.concat " " args);
		try
			let output = Stringext.String.rtrim (fst(Forkhelpers.execute_command_get_output Xapi_globs.xe_path args)) in
			debug "%s" output;
			output
		with
			| Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED n) ->
				failwith "CLI failed"
			| Forkhelpers.Spawn_internal_error(log, output, _) ->
				failwith "CLI failed"
			| e ->
				failwith "CLI failed" in

	let vm_install session_id template name sr =
		let sr_uuid = Client.SR.get_uuid rpc session_id sr in
		let newvm_uuid = cli_cmd [ "vm-install"; "template=" ^ template; "new-name-label=" ^ name; "sr-uuid=" ^ sr_uuid ] in
		let vm =
			Client.VM.get_by_uuid rpc session_id newvm_uuid
		in
		Client.VM.set_PV_args rpc session_id vm "noninteractive";
		vm
	in
	let vm' = vm_install session "Demo Linux VM" "lvhdrt sacrificial VM" sr in

	Pervasiveext.finally
		(fun () -> f vm')
		(fun () ->
			if Client.VM.get_power_state rpc session vm' <> `Halted then
				Client.VM.hard_shutdown rpc session vm';
			List.iter
				(fun vbd ->
					let vdi = Client.VBD.get_VDI rpc session vbd in
					if not (Client.VBD.get_empty rpc session vbd) then Client.VDI.destroy rpc session vdi)
				(Client.VM.get_VBDs rpc session vm');
			Client.VM.destroy rpc session vm')

let get_free_space rpc session sr =
	let total = Client.SR.get_physical_size rpc session sr in
	Client.SR.update rpc session sr;
	Int64.sub total (Client.SR.get_physical_utilisation rpc session sr)

let size_of x =
	let gig = Int64.div x (Int64.mul Globs.meg 1024L) in
	Printf.sprintf "%Ld (%LdGo)" x gig

let dump_vdi_info rpc session vdi =
	let vdir = Client.VDI.get_record rpc session vdi in
	debug "VDI: name: %s; uuid: %s; size: %s" vdir.API.vDI_name_label vdir.API.vDI_uuid (size_of vdir.API.vDI_physical_utilisation)

(* Convert a list of VDI (ref, record) pairs into a comma-separated string of UUIDs *)
let vdis_to_uuids vdis =
        Printf.sprintf "[%s]" (String.concat ", " (List.map (fun (vdi, vdi_rec) -> vdi_rec.API.vDI_uuid) vdis))

(* Create a tree of vdis with the following shape:
 *
 *                   Original VDI (hidden)
 *                     /             \ 
 *                   vdi2        Another hidden VDI
 *                                /          \
 *                              vdi3         vdi
 * VDIs are created with size 'size', and data of size 'pattern_size' are written on intermediate VDIs.
 * Optinally, vdi2 is resized to 'resize'.
 * Finally, vdi2 has 'to_remove=true' in his other-config map.
 *)
let create_vdi_tree rpc session sr name_label size ?resize ?(pattern_type=1) pattern_size =

	let vdi = Client.VDI.create ~rpc ~session_id:session ~name_label
		~name_description:"Test for coalescing purposes" ~sR:sr ~virtual_size:size ~_type:`ephemeral (*!*)
		~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
	dump_vdi_info rpc session vdi;
	
	let master = get_master rpc session in

	debug "Writing pattern of size %s to VDI" (size_of pattern_size);
	(* Write pattern type=1 variant=0 to vdi *)
	let result = with_attached_vdi rpc session vdi 
		(fun real_device vbd -> 
			 Client.Host.call_plugin 
				 ~rpc ~session_id:session ~host:master ~plugin:Globs.helper_plugin ~fn:"pattern" 
				 ~args:[("dev","/dev/"^real_device); ("size",(Int64.to_string pattern_size)); ("action","write"); ("type",string_of_int pattern_type); ("variant","0")]) in
	debug "Pattern write got result: '%s'" result;
	
	debug "Cloning initial VDI";
	let vdi2 = Client.VDI.clone ~rpc ~session_id:session ~vdi:vdi ~driver_params:[] in
	Client.VDI.set_other_config rpc session vdi2 (("to_remove","true") :: Client.VDI.get_other_config rpc session vdi2);
	if resize <> None then 
		Pervasiveext.maybe (fun r -> Client.VDI.resize rpc session vdi r) resize;
	dump_vdi_info rpc session vdi2;  
  
	(* At this point, vdi and vdi2 are two leaf nodes:
	 *
	 *                   Original VDI (hidden)
	 *                     /             \ 
	 *                   vdi2           vdi
	 *)
	debug "Writing pattern to VDI";
	(* Write pattern type=1 variant=1 to vdi *)
	let result = with_attached_vdi rpc session vdi
		(fun real_device vbd -> 
			 Client.Host.call_plugin 
				 ~rpc ~session_id:session ~host:master ~plugin:Globs.helper_plugin ~fn:"pattern" 
				 ~args:[("dev","/dev/"^real_device); ("size",(Int64.to_string pattern_size)); ("action","write"); ("type","1"); ("variant","1")]) in
	debug "Pattern write got result: '%s'" result;

	debug "Cloning initial VDI again to create tree";
	let vdi3 = Client.VDI.clone ~rpc ~session_id:session ~vdi:vdi ~driver_params:[] in
	dump_vdi_info rpc session vdi3;  

	(* At this point, we have: 
	 *
	 *                   Original VDI (hidden)
	 *                     /             \ 
	 *                   vdi2        Another hidden VDI
	 *                                /          \
	 *                              vdi3         vdi
	 * 
	 * Deleting vdi2 will leave the tree in a state where the two original hidden VDIs can be coalesced
	 *)
	debug "Writing pattern to VDI";
	(* Write pattern type=1 variant=2 to vdi *)
	let result = with_attached_vdi rpc session vdi
		(fun real_device vbd -> 
			 Client.Host.call_plugin 
				 ~rpc ~session_id:session ~host:master ~plugin:Globs.helper_plugin ~fn:"pattern" 
				 ~args:[("dev","/dev/"^real_device); ("size",(Int64.to_string pattern_size)); ("action","write"); ("type","1"); ("variant","2")]) in
	debug "Pattern write got result: '%s'" result;

	(vdi,vdi2,vdi3)


let wait_for_fist rpc session sr ?(delay=90.0) fist =
  let session2 = Client.Session.login_with_password rpc !Globs.username !Globs.password "1.4" in
 
  Client.Event.register rpc session2 ["sr"];

  let finished = ref false in
  let aborted = ref false in

  let (_: Thread.t) = Thread.create (fun () -> 
    Thread.delay delay;
    debug "Timer has expired (%.0f seconds); logging out session" delay;
    Client.Session.logout rpc session2) ()
  in
  
  let check record = 
    let a = List.mem_assoc fist record.API.sR_other_config in
    finished := a || !aborted;
  in
  
  check (Client.SR.get_record rpc session2 sr);

  try
    while not !finished do
      let events = Event_types.events_of_rpc (Client.Event.next rpc session2) in
      debug "Got %d events..." (List.length events);
      let checkevent ev = 
	match Event_helper.record_of_event ev with
	  | Event_helper.SR (r,Some x) -> if r=sr then check x
	  | _ -> debug "Got irrelevant event";
      in List.iter checkevent events
    done;
    true
  with e -> 
    debug "Got exception: %s" (Printexc.to_string e);
    false

(* Wait for all the VDIs in the given list to be deleted. Note that the full
 * delay will be used if at least one VDI is not deleted (e.g. because it didn't
 * exist. *)
let wait_for_vdi_deletion rpc session sr ?(delay=90.0) vdis =
  let session2 = Client.Session.login_with_password rpc !Globs.username !Globs.password "1.4" in
 
  Client.Event.register rpc session2 ["vdi"];
  debug "Registered for vdi events";

  let finished = ref (List.length vdis = 0) in
  let aborted = ref false in

  let (_: Thread.t) = Thread.create (fun () -> 
    Thread.delay delay;
    debug "Timer has expired (%.0f seconds); logging out session" delay;
    Client.Session.logout rpc session2) ()
  in

  let vdis_remaining = ref vdis in
  debug "Initially, the VDIs we are waiting for are: %s" (vdis_to_uuids !vdis_remaining);

  try
    while not !finished do
      let events = Event_types.events_of_rpc (Client.Event.next rpc session2) in
      debug "Got %d events..." (List.length events);
      let checkevent ev = 
	match Event_helper.record_of_event ev with
	  | Event_helper.VDI (r,_) ->
              if ev.Event_types.op = `del then begin
                (match Event_helper.record_of_event ev with Event_helper.VDI(vdi,_) -> debug "Received a VDI deletion event concerning VDI with ref %s" (Ref.string_of vdi) | _ -> assert false);
                (* Remove it from the list of VDIs we are waiting to be deleted *)
                vdis_remaining := List.filter (fun (vdi_ref,vdi_rec) -> vdi_ref <> r) !vdis_remaining;
                debug "We are still waiting for VDIs %s" (vdis_to_uuids !vdis_remaining);
                (* We've finished if we're not waiting for any more VDIs *)
                finished := (List.length !vdis_remaining = 0) || !aborted
              end
	  | _ -> debug "Got irrelevant event";
      in List.iter checkevent events
    done;
    true
  with e -> 
    debug "Got exception: %s" (Printexc.to_string e);
    false

(* ---------------------------------------------------- *)
(* Printing out forests of VDIs, for debugging purposes *)

type info = {
  uuid: string;
  name_label: string;
  refa: int; (* Calculated by us - to check against the SR backend's refcounts *) 
  refb: int; (* Calculated by us *)
}

type tree = Node of info * (tree list) 

let vhd_parent = "vhd-parent"

(** VDIs is the result of a call to SR.get_VDIs *)
let construct_forest rpc session vdis =
  let vbds = Client.VBD.get_all_records rpc session in

  let vdirefrecs = List.map (fun vdi -> (vdi,Client.VDI.get_record rpc session vdi)) vdis in

  let is_currently_attached vdi =
    List.exists (fun (_,vbd) -> vbd.API.vBD_VDI=vdi && vbd.API.vBD_currently_attached) vbds
  in

  let roots = List.filter (fun (vdiref,vdirec) -> not (List.mem_assoc vhd_parent vdirec.API.vDI_sm_config)) vdirefrecs in

  (** root=(vdiref, vdirec) *)
  let rec construct_tree root = 
    let uuid = (snd root).API.vDI_uuid in
    let name_label = (snd root).API.vDI_name_label in
    let mychildren = List.filter (fun (vdiref,vdirec) ->
      List.mem_assoc vhd_parent vdirec.API.vDI_sm_config && 
	List.assoc vhd_parent vdirec.API.vDI_sm_config = uuid) vdirefrecs in
    let children_nodes = List.map construct_tree mychildren in
    let refa = List.fold_left 
      (fun refa n -> 
	let i = match n with | Node (i,_) -> i in refa + i.refb + i.refa) 0 children_nodes in
    Node ({uuid=uuid; name_label=name_label; refa=refa; refb=(if is_currently_attached (fst root) then 1 else 0)}, children_nodes)
  in

  let forest = List.map construct_tree roots in

  forest

let dump_forest forest =
  let rec dump_node prefix node =
    let Node (i,children) = node in
    Printf.printf "%suuid=%s \"%s\" refs=(%d,%d)\n" prefix i.uuid i.name_label i.refa i.refb;
    List.iter (dump_node (Printf.sprintf "%s  " prefix)) children
  in
  List.iter (dump_node "") forest

