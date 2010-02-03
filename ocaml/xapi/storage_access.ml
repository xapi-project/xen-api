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
open Threadext

module D=Debug.Debugger(struct let name="storage_access" end)
open D



(* ********************* attaching and detaching SRs ******************* *)

module SR =
  struct
    let pbd_of_sr ~__context ~self = 
      let me = Helpers.get_localhost ~__context in
      let is_host = Db_filter_types.Eq (Db_filter_types.Field "host", Db_filter_types.Literal (Ref.string_of me))
      and is_sr = Db_filter_types.Eq (Db_filter_types.Field "SR", Db_filter_types.Literal (Ref.string_of self)) in
      let expr = Db_filter_types.And(is_host, is_sr) in
      match Db.PBD.get_records_where ~__context ~expr with
      | [ pbd ] -> fst pbd
      | [] -> failwith "Failed to find PBD attaching SR to localhost"
      | _ -> failwith "Found multiple PBDs attaching SR to localhost"

    (* If the SR is not marked as sharable, reject the attach if someone else has already 
       already attached it *)
    let check_sharing_constraint ~__context ~self =
      if not(Db.SR.get_shared ~__context ~self) then begin
	let pbds = Db.SR.get_PBDs ~__context ~self in
	(* Filter out the attached PBDs which aren't connected to this host *)
	let me = Helpers.get_localhost ~__context in
	let others = List.filter (fun self -> 
				    Db.PBD.get_currently_attached ~__context ~self && 
				      Db.PBD.get_host ~__context ~self <> me) pbds in
	if others <> []
	then raise (Api_errors.Server_error(Api_errors.sr_not_sharable, 
					    [ Ref.string_of self; Ref.string_of (Db.PBD.get_host ~__context ~self:(List.hd others)) ]))
      end

    (* attach backend must be idempotent by design *)
    let attach ~__context ~self =
      check_sharing_constraint ~__context ~self;	   
      let pbd = pbd_of_sr ~__context ~self in
      Sm.call_sm_functions ~__context ~sR:self
	(fun device_config _type ->
	   try
	     Sm.sr_attach device_config _type self;
	     Xapi_local_pbd_state.add pbd;
	     Db.PBD.set_currently_attached ~__context ~self:pbd ~value:true;
	   with e ->
	     debug "Failed to attach SR %s (exception %s)" (Db.SR.get_name_label ~__context ~self) (ExnHelper.string_of_exn e);
	     Xapi_local_pbd_state.remove pbd;
	     Db.PBD.set_currently_attached ~__context ~self:pbd ~value:false;
	     raise e)

    (* always attempt detach -- may throw an error *)
    let detach ~__context ~self =
      let pbd = pbd_of_sr ~__context ~self in
      Sm.call_sm_functions ~__context ~sR:self
	(fun device_config _type ->
	   Sm.sr_detach device_config _type self;
	   Xapi_local_pbd_state.remove pbd;
	   Db.PBD.set_currently_attached ~__context ~self:pbd ~value:false)

  end

(* ********************* attaching and detaching VDIs ******************* *)

module VDI =
  struct

    type lock = string (* token passed to the backend *)
    let fresh_lock () = Uuid.string_of_uuid (Uuid.make_uuid ())

    (* When VDI attached, record path returned from attach fn in SM backend in hashtable *)
    let vdi_tbl_mutex = Mutex.create()
    let with_vdi_lock f = Mutex.execute vdi_tbl_mutex f
      (* Record VDI->path mapping derived from attach calls *)
    let vdi_tbl:([`VDI] Uuid.t, string) Hashtbl.t = Hashtbl.create 100
      (* Refcount for VDI activates *)
    let vdi_activate_refcount_tbl : ([`VDI] Ref.t, int) Hashtbl.t = Hashtbl.create 100
      (* Refcount for VDI attaches *)
    let vdi_attach_refcount_tbl : ([`VDI] Ref.t, int) Hashtbl.t = Hashtbl.create 100

	(* Record for VDI-> attach mode *)
	let vdi_mode:([`VDI] Uuid.t, [`RW|`RO]) Hashtbl.t = Hashtbl.create 100
	let is_already_attached vdi =
		Hashtbl.mem vdi_tbl vdi
	let get_mode vdi =
		Hashtbl.find vdi_mode vdi

    let check_enclosing_sr_for_capability __context capability vdi =
      let sr = Db.VDI.get_SR ~__context ~self:vdi in
      let sr_type = Db.SR.get_type ~__context ~self:sr in
      List.mem capability (Sm.capabilities_of_driver sr_type)

    let increment_refcount tbl self =
      try
	let current = Hashtbl.find tbl self in
	Hashtbl.replace tbl self (current+1);
	current+1
      with _ ->
	(Hashtbl.replace tbl self 1; (* not in there so assume "current=0" *)
	 1)
    let decrement_refcount tbl self =
      let cap_at_zero x = if x<0 then 0 else x in
      try
	let current = Hashtbl.find tbl self in
	let newval = cap_at_zero (current-1) in
	Hashtbl.replace tbl self newval;
	newval
      with _ ->
	(Hashtbl.replace tbl self 0; (* not in there so assume "current=0" and then cap at 0 *)
	 0)
	  
    let read_refcount tbl self =
      try Hashtbl.find tbl self with _ -> 0

    let increment_activate_refcount self = increment_refcount vdi_activate_refcount_tbl self
    let decrement_activate_refcount self = decrement_refcount vdi_activate_refcount_tbl self
    let read_activate_refcount self = read_refcount vdi_activate_refcount_tbl self

    let increment_attach_refcount self = increment_refcount vdi_attach_refcount_tbl self
    let decrement_attach_refcount self = decrement_refcount vdi_attach_refcount_tbl self
    let read_attach_refcount self = read_refcount vdi_attach_refcount_tbl self

    (* Initialises this host's refcount table from the master database *)
    let initialise_refcounts_from_db() =
      with_vdi_lock
	(fun () ->
	   Server_helpers.exec_with_new_task "initialising vdi refcounts from db"
	     (fun __context ->		
		debug "Initialising attach and activate refcounts from database";
		
		let this_host = Helpers.get_localhost __context in
		let all_vms = Db.VM.get_records_where ~__context ~expr:(Db_filter_types.Eq(Db_filter_types.Field "resident_on", Db_filter_types.Literal (Ref.string_of this_host))) in  

		let my_running_vms_according_to_db =
		  (* find VMs resident on me who are running or paused - we specifically excluded suspended VMs, since
		     their VBDs will have currently_attached=true even though disks are not actually attached in the "vdi_attach sense" *)
		  List.filter (fun (_,vmrec) -> (vmrec.API.vM_resident_on = this_host) && 
				 (vmrec.API.vM_power_state=`Running || vmrec.API.vM_power_state=`Paused)) all_vms in
		(* For each VM running on me, get its VBDs and sync vdi hashtable from the "currently_attached" field accordingly;
		   when resyncing we assume that if the VDI's enclosing SR supports activate that we should increment both the
		   attach and activate refcounts *)
		List.iter
		  (fun (vmref,vmrec) ->
		     let vbds = vmrec.API.vM_VBDs in
		     let currently_attached_vbds =
		       List.filter (fun vbdref -> Db.VBD.get_currently_attached ~__context ~self:vbdref) vbds in
		     (* for each VBD, find the VDI and increment the refcounts *)
		     List.iter
		       (fun vbdref ->
			  if not (Db.VBD.get_empty ~__context ~self:vbdref) then
			    let vdi = Db.VBD.get_VDI ~__context ~self:vbdref in
			    let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vdi in
			    debug "Incrementing attach refcount for VDI: %s" vdi_uuid;
			    ignore (increment_attach_refcount vdi);
			    if (check_enclosing_sr_for_capability __context Smint.Vdi_activate vdi) then
			      begin
				debug "Incrementing activate refcount for VDI: %s" vdi_uuid;
				ignore (increment_activate_refcount vdi)
			      end
		       )
		       currently_attached_vbds
		  )
		  my_running_vms_according_to_db
	     )
	)

    let attach ~__context ~self ~mode =
      let string_of_mode = function `RW -> "RW" | `RO -> "RO" in
      let uuid = Uuid.of_string (Db.VDI.get_uuid ~__context ~self) in
      with_vdi_lock
	(fun () ->
           (* MTC: A protected VM needs to have its disks mounted into two VMs: one as R+W and another as RO *)
	   if is_already_attached uuid && (mode <> get_mode uuid) && 
             not (Mtc.is_vdi_accessed_by_protected_VM ~__context ~vdi:self) then
	     failwith (Printf.sprintf "The VDI %s is already attached in %s mode; it can't be attached in %s mode!" (Uuid.to_string uuid) (string_of_mode (get_mode uuid)) (string_of_mode mode));
	   let attach_path = 
	     Sm.call_sm_vdi_functions ~__context ~vdi:self
	       (fun device_config _type sr ->
	     Sm.vdi_attach device_config _type sr self (mode = `RW)) in
	   Hashtbl.replace vdi_tbl uuid attach_path;
	   Hashtbl.replace vdi_mode uuid mode;
	   let newval = increment_attach_refcount self in
	   debug "Executed %s attach succesfully on VDI '%s'; attach refcount now: %d" (string_of_mode mode) (Uuid.to_string uuid) newval
	)

    let detach ~__context ~self =
      let uuid = Uuid.of_string (Db.VDI.get_uuid ~__context ~self) in
      with_vdi_lock
	(fun () ->
	   let attach_refcount = read_attach_refcount self in
	   if attach_refcount<=1 then (* do detach on 1->0 transition, but also allow detach through if we're already at 0.. *) 
	     begin
	       if attach_refcount<1 then
	         debug "vdi refcount violation (on detach): hashtbl reports '%d' for VDI '%s'" attach_refcount (Db.VDI.get_uuid ~__context ~self);
	       Sm.call_sm_vdi_functions ~__context ~vdi:self
	         (fun device_config _type sr ->
	           Sm.vdi_detach device_config _type sr self);
	       Hashtbl.remove vdi_tbl uuid;
	       Hashtbl.remove vdi_mode uuid;
	     end
	   else
	     debug "Not passing detach to backend: refcount (before decrement I'm about to do) is %d" attach_refcount;
	   let newval = decrement_attach_refcount self (* caps at 0 *) in
	   debug "Executed detach succesfully on VDI '%s'; attach refcount now: %d" (Uuid.to_string uuid) newval
	)

    let activate ~__context ~self =
    with_vdi_lock
      (fun () ->
	 if (check_enclosing_sr_for_capability __context Smint.Vdi_activate self) then
	   begin
	     Sm.call_sm_vdi_functions ~__context ~vdi:self
	       (fun device_config sr_type sr ->
		  Sm.vdi_activate device_config sr_type sr self);
	     let newval = increment_activate_refcount self in
	     debug "Executed activate succesfully on VDI '%s'; activate refcount now: %d" (Ref.string_of self) newval
	   end
      )

    let deactivate ~__context ~self =
    with_vdi_lock
      (fun () ->
	 if (check_enclosing_sr_for_capability __context Smint.Vdi_deactivate self) then
	   begin
	     (* read current value of refcount *)
	     let activate_refcount = read_activate_refcount self in

	     (* CA-15824: we decrement refcount whether or not deactivate call is succesful *)
	     ignore (decrement_activate_refcount self); (* caps at 0 *)
	     debug "Considering execute deactive on VDI '%s'; activate refcount now: %d" (Ref.string_of self) activate_refcount;
	     if activate_refcount<=1 then (* do deactivate on 1->0 transition, but also allow detach through if we're already at 0.. *) 
	       begin
		 if activate_refcount<1 then
		   warn "vdi refcount violation (on deactivate): hashtbl reports '%d' for VDI '%s'" activate_refcount (Db.VDI.get_uuid ~__context ~self);
		 Sm.call_sm_vdi_functions ~__context ~vdi:self
		   (fun device_config sr_type sr ->
		      Sm.vdi_deactivate device_config sr_type sr self);
		 debug "Executed deactivate in backend succesfully"
	       end
	     else
	       debug "Not passing deactivate to backend: refcount was at is %d" activate_refcount;
	   end
      )
      
    exception CannotReadVDIPath of string
    let get_physical_path vdi_uuid =
      with_vdi_lock (fun ()->try Hashtbl.find vdi_tbl vdi_uuid
		     with _ -> raise (CannotReadVDIPath (Uuid.to_string vdi_uuid)))
 
  end

(* given VDI, attach it; we know that the VDI's SR will already have been attached since xapi guarantees that no
   operations that require access to a VDI, v, can proceed unless v's SR is already attached on this host
   (i.e. the relevant PBD is plugged in)
 *)
let use_vdi ~__context ~vdi ~mode =
  VDI.attach ~__context ~self:vdi ~mode;
  try
    VDI.activate ~__context ~self:vdi
  with e ->
    (* if activate fails then best effort detach VDI before propogating original exception *)
    begin
      (try VDI.detach ~__context ~self:vdi with _ -> ());
      raise e
    end

let deactivate_and_detach ~__context ~vdi =
  VDI.deactivate ~__context ~self:vdi;
  VDI.detach ~__context ~self:vdi

(* Given vbd, lookup vdi and signal that we're about to use it. 
   Allow any backend SM exception to propagate upwards *)
let use_vdi_from_vbd ~__context vbd =
  if not(Db.VBD.get_empty ~__context ~self:vbd)
  then 
    let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
    let mode = Db.VBD.get_mode ~__context ~self:vbd in
    use_vdi ~__context ~vdi ~mode

(* We attach and activate VDIs specified in list before executing f. If any exception is
   thrown, either during activate/attach or during f, then we attempt to deactivate/detach
   any VDIs that we _did_ manage to succesfully activate/attach and then propogate original
   exception. If leave_activated is false then, after executing f (or if an exception is
   thrown from f) then we detach/deactivate disks.
*)
let with_careful_attach_and_activate ~__context ~vdis ~leave_activated f =

  (* Phase 1: attach/activate and execute f - if exception arises then detach/deactivate where
     possible and propogate original exception *)
  let size = List.length vdis in
  (* record which of the VDIs we manage to attach *)
  let attached : ([`VDI] Ref.t, unit) Hashtbl.t = Hashtbl.create size in
  (* record which of the VDIs we manage to activate *)
  let activated : ([`VDI] Ref.t, unit) Hashtbl.t = Hashtbl.create size in
  let result_of_f =
    try
      let do_single_attach (vdi,mode) =
	VDI.attach ~__context ~self:vdi ~mode;
	Hashtbl.replace attached vdi ();
	VDI.activate ~__context ~self:vdi;
	Hashtbl.replace activated vdi () in
      (* Attach/activate vbds recording what we've done *)
      List.iter do_single_attach vdis;
      (* Execute function *)
      f()      
	(* if anything went wrong be careful to best effort deactivate/detach *)
    with e ->
      begin
	(* Best effort de-activate for VDIs that we just activated *)
	Hashtbl.iter
	  (fun vdi _ ->
	     Helpers.log_exn_continue ("failed to de-activate vdi: " ^ (Ref.string_of vdi))
	       (fun ()->VDI.deactivate ~__context ~self:vdi) ()) activated;
	(* Best effort detach for VDIs that we just detached *)
	Hashtbl.iter
	  (fun vdi _ ->
	     Helpers.log_exn_continue ("failed to detach vdi: " ^ (Ref.string_of vdi))
	       (fun ()->VDI.detach ~__context ~self:vdi) ()) attached;
	
	raise e (* propogate original exception *)
      end in
  (* Phase 2: if leave_activated=false then detach/deactivate vdis *)
  if not leave_activated then
    List.iter
      (fun (vdi,_) ->
	 Helpers.log_exn_continue ("failed to de-activate vdi: " ^ (Ref.string_of vdi))
	   (fun ()->VDI.deactivate ~__context ~self:vdi) ();
	 Helpers.log_exn_continue ("failed to detach vdi: " ^ (Ref.string_of vdi))
	   (fun ()->VDI.detach ~__context ~self:vdi) ()
      )
      vdis;
  result_of_f

