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
(** Storage manager interface
 * @group Storage
 *)

open Stringext
open Smint
open Printf
open Pervasiveext

module D=Debug.Debugger(struct let name="sm" end)
open D


(*****************************************************************************)
let driver_info_cache : (string, sr_driver_info) Hashtbl.t = Hashtbl.create 10

exception Unknown_driver of string

(** Scans the plugin directory and registers everything it finds there *)
let register () = 
  let add_entry driver info =
    let name = String.lowercase driver in
    Hashtbl.replace driver_info_cache name info;
    debug "Registered driver %s under name %s" driver name
  in
  Sm_exec.get_supported add_entry

let supported_drivers () =
  Hashtbl.fold (fun name _ acc -> name :: acc) driver_info_cache [] 


let info_of_driver (name: string) =
	let name = String.lowercase name in
	if not(Hashtbl.mem driver_info_cache name)
	then raise (Unknown_driver name)
	else (Hashtbl.find driver_info_cache name)

let capabilities_of_driver (name: string) = (info_of_driver name).sr_driver_capabilities

let driver_filename driver = 
  let info=info_of_driver driver in
  info.sr_driver_filename

let driver_type driver = 
  let info=info_of_driver driver in
  info.sr_driver_type

(*****************************************************************************)

(* Cache the result of sr_content_type since it never changes and we need it for
   stuff like resynchronising devices at start-of-day *)
let sr_content_type_cache : (API.ref_SR, string) Hashtbl.t = Hashtbl.create 10
let sr_content_type_cache_m = Mutex.create ()

(*****************************************************************************)

let debug operation driver msg =
  debug "SM %s %s %s" driver operation msg

let sr_create dconf driver sr size =
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_create" [ Int64.to_string size ] in
  debug "sr_create" driver (sprintf "sr=%s size=%Ld" (Ref.string_of sr) size);
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let sr_delete dconf driver sr =
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_delete" [] in
  debug "sr_delete" driver (sprintf "sr=%s" (Ref.string_of sr));
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

(* Mutex for sr_attach, sr_detach, and sr_probe *)
let serialize_attach_detach = Mutex.create()

let sr_attach dconf driver sr =
  Threadext.Mutex.execute serialize_attach_detach
    (fun ()->
       debug "sr_attach" driver (sprintf "sr=%s" (Ref.string_of sr));
       let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_attach" [] in
       Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call))

let sr_detach dconf driver sr =
  Threadext.Mutex.execute serialize_attach_detach
    (fun ()->
       debug "sr_detach" driver (sprintf "sr=%s" (Ref.string_of sr));
       let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_detach" [] in
       Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call));
  Threadext.Mutex.execute sr_content_type_cache_m
    (fun () -> Hashtbl.remove sr_content_type_cache sr)
	 
let sr_probe dconf driver sr_sm_config =
  if List.mem Sr_probe (capabilities_of_driver driver)
  then
    Threadext.Mutex.execute serialize_attach_detach
      (fun ()->
	 debug "sr_probe" driver (sprintf "sm_config=[%s]" (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) sr_sm_config)));
	 let call = Sm_exec.make_call ~sr_sm_config dconf "sr_probe" [] in
	 (* sr_probe returns an XML document marshalled within an XMLRPC string *)
	 XMLRPC.From.string (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call))
  else
    raise (Api_errors.Server_error (Api_errors.sr_backend_failure, [ ("Operation 'sr_probe' not supported by this SR type"); ""; ""]))

let sr_scan dconf driver sr = 
  debug "sr_scan" driver (sprintf "sr=%s" (Ref.string_of sr));
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_scan" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let sr_content_type dconf driver sr =
  debug "sr_content_type" driver (sprintf "sr=%s" (Ref.string_of sr));
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_content_type" [] in
  Sm_exec.parse_sr_content_type (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let sr_update dconf driver sr = 
  debug "sr_update" driver (sprintf "sr=%s" (Ref.string_of sr));
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_update" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_create dconf driver sr sm_config vdi_type size name_label name_description =
  debug "vdi_create" driver (sprintf "sr=%s sm_config=[%s] type=[%s] size=%Ld" (Ref.string_of sr) (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) sm_config)) vdi_type size);
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_sm_config:sm_config ~vdi_type dconf "vdi_create" [ sprintf "%Lu" size; name_label ; name_description ] in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_update dconf driver sr vdi = 
  debug "vdi_update" driver (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_update" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_introduce dconf driver sr new_uuid sm_config location = 
  debug "vdi_introduce" driver (sprintf "sr=%s new_uuid=%s sm_config=[%s] location=%s" (Ref.string_of sr) new_uuid (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) sm_config)) location);
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_location:location ~vdi_sm_config:sm_config ~new_uuid:new_uuid dconf "vdi_introduce" [] in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_delete dconf driver sr vdi =
  debug "vdi_delete" driver (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_delete" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_attach dconf driver sr vdi writable =
  debug "vdi_attach" driver (sprintf "sr=%s vdi=%s writable=%b" (Ref.string_of sr) (Ref.string_of vdi) writable);
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_attach" [ sprintf "%b" writable ] in
  Sm_exec.parse_string (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_detach dconf driver sr vdi =
  debug "vdi_detach" driver (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_detach" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_activate dconf driver sr vdi writable =
  debug "vdi_activate" driver (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_activate" [ sprintf "%b" writable ] in
  let Some loc = call.Sm_exec.vdi_location in 
  Xapi_local_vdi_state.activate loc writable;
  try 
	  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)
  with e ->
	  Xapi_local_vdi_state.deactivate loc;
	  raise e
			
let vdi_deactivate dconf driver sr vdi =
  debug "vdi_deactivate" driver (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_deactivate" [] in
  let Some loc = call.Sm_exec.vdi_location in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call);
  Xapi_local_vdi_state.deactivate loc

let vdi_snapshot dconf driver driver_params sr vdi =
  debug "vdi_snapshot" driver (sprintf "sr=%s vdi=%s driver_params=[%s]" (Ref.string_of sr) (Ref.string_of vdi) (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) driver_params)));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi ~driver_params dconf "vdi_snapshot" [] in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)
	
let vdi_clone dconf driver driver_params context sr vdi =
  debug "vdi_clone" driver (sprintf "sr=%s vdi=%s driver_params=[%s]" (Ref.string_of sr) (Ref.string_of vdi) (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) driver_params)));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi ~driver_params dconf "vdi_clone" [] in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_resize dconf driver sr vdi newsize =
  debug "vdi_resize" driver (sprintf "sr=%s vdi=%s newsize=%Ld" (Ref.string_of sr) (Ref.string_of vdi) newsize);
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_resize" [ sprintf "%Lu" newsize ] in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_resize_online dconf driver sr vdi newsize =
  debug "vdi_resize_online" driver (sprintf "sr=%s vdi=%s newsize=%Ld" (Ref.string_of sr) (Ref.string_of vdi) newsize);
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_resize_online" [ sprintf "%Lu" newsize ] in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let vdi_generate_config dconf driver sr vdi = 
  debug "vdi_generate_config" driver (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi));
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_generate_config" [] in
  Sm_exec.parse_string (Sm_exec.exec_xmlrpc (driver_type driver)  (driver_filename driver) call)

let session_has_internal_sr_access ~__context ~sr = 
  let session_id = Context.get_session_id __context in
  (* XXX: need to move this somewhere else eventually *)
  let other_config = Db.Session.get_other_config ~__context ~self:session_id in
  List.mem_assoc Xapi_globs._sm_session other_config
  && (List.assoc Xapi_globs._sm_session other_config) = Ref.string_of sr

let assert_session_has_internal_sr_access ~__context ~sr = 
  if not(session_has_internal_sr_access ~__context ~sr)
  then raise (Api_errors.Server_error(Api_errors.permission_denied, [""]))

(*****************************************************************************)
(* Higher-level functions                                                    *)

let get_my_pbd_for_sr __context sr_id =
  let me = Helpers.get_localhost __context in
  let pbd_ref_and_record = Db.PBD.get_records_where ~__context 
    ~expr:(Db_filter_types.And (
      Db_filter_types.Eq (Db_filter_types.Field "host", Db_filter_types.Literal (Ref.string_of me)),
      Db_filter_types.Eq (Db_filter_types.Field "SR", Db_filter_types.Literal (Ref.string_of sr_id))))
  in
  match pbd_ref_and_record with
    | [] -> raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of sr_id ]))
    | (x::_) -> x

let assert_pbd_is_plugged ~__context ~sr = 
  let _, pbd_r = get_my_pbd_for_sr __context sr in
  if not(pbd_r.API.pBD_currently_attached)
  then raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of sr ]))

let sm_master x = ("SRmaster", string_of_bool x)

(* Internal only function - use 'call_sm_functions' and 'call_sm_vdi_functions' *)
let __get_my_devconf_for_sr __context sr_id =
  let srmaster = Helpers.i_am_srmaster ~__context ~sr:sr_id in
  let (pbdref,pbd) = get_my_pbd_for_sr __context sr_id in
  (sm_master srmaster) :: pbd.API.pBD_device_config 

(** Make it easier to call SM backend functions on an SR *)
let call_sm_functions ~__context ~sR f = 
    let srtype = Db.SR.get_type ~__context ~self:sR
    and srconf = __get_my_devconf_for_sr __context sR in
    let subtask_of = Some (Context.get_task_id __context) in
    f (subtask_of,srconf) srtype

(** Make it easier to call SM backend functions on a VDI directly *)
let call_sm_vdi_functions ~__context ~vdi f = 
    let sr = Db.VDI.get_SR ~__context ~self:vdi in
    let srtype = Db.SR.get_type ~__context ~self:sr
    and srconf = __get_my_devconf_for_sr __context sr in
    let subtask_of = Some (Context.get_task_id __context) in
    f (subtask_of,srconf) srtype sr

(* Use the sr_content_type cache *)
let sr_content_type ~__context ~sr = 
  Threadext.Mutex.execute sr_content_type_cache_m
    (fun () ->
       if Hashtbl.mem sr_content_type_cache sr
       then Hashtbl.find sr_content_type_cache sr
       else 
	 let ty = call_sm_functions ~__context ~sR:sr (fun srconf srtype -> (sr_content_type srconf srtype sr)) in
	 Hashtbl.replace sr_content_type_cache sr ty;
	 ty)

(*****************************************************************************)
(* Storage-related utility functions                                         *)

open Client

(** Call a function 'f' after pausing all VBDs which are currently_attached to a particular VDI.
    Take care to unpause everything on the way out of the function. 
    Notes on how the locking is supposed to work:
    1. The whole disk should be locked against concurrent hotplug attempts
    2. A disk might be spontaneously unplugged between the point where we example 'currently_attached'
    and the point where we call VBD.pause -- it is safe to skip over these.
    3. In Orlando concurrent VM.snapshot calls are allowed (as part of the snapshot-with-quiesce work)
    we must pause VBDs in order of device number to avoid a deadlock where 'VM.get_VBDs' returns the
    devices in a different order in a parallel call
*)
let with_all_vbds_paused ~__context ~vdis f = 
  (* We need to keep track of the VBDs we've paused so we can go back and unpause them *)
  let paused_so_far = ref [] in
  let vbds = List.concat (List.map (fun vdi -> Db.VDI.get_VBDs ~__context ~self:vdi) vdis) in  
  let vbds = List.filter 
    (fun self -> Db.VBD.get_currently_attached ~__context ~self 
                 && Db.VM.get_power_state ~__context ~self:(Db.VBD.get_VM ~__context ~self) <> `Suspended) 
    vbds in
  (* CA-24232: prevent concurrent snapshots of the same VM leading to deadlock since the database
     doesn't guarantee to return records in the same order. *)
  let vbds = List.sort (fun a b ->
			  let a_device = Db.VBD.get_device ~__context ~self:a 
			  and b_device = Db.VBD.get_device ~__context ~self:b in
			  compare a_device b_device) vbds in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       finally
	 (fun () ->
	    (* Attempt to pause all the VBDs *)
	    List.iter
	      (fun vbd ->
		 (* NB it is possible for a disk to spontaneously unplug itself: 
		    we are safe to skip these VBDs. *)
		 (* NB it is possible for a VM to suddenly migrate; if so we retry *)
		 let finished = ref false in
		 while not !finished do
		   try
			   let token = Client.VBD.pause rpc session_id vbd in
			   paused_so_far := (vbd, token) :: !paused_so_far;
			   finished := true
		   with 
		   | Api_errors.Server_error(code, _) when code = Api_errors.device_not_attached ->
		       warn "Not pausing VBD %s because it appears to have spontaneously unplugged" (Ref.string_of vbd);
		       finished := true
		   | Api_errors.Server_error(code, _) when code = Api_errors.vm_bad_power_state ->
		       warn "Not pausing VBD %s because the VM has shutdown" (Ref.string_of vbd);
		       finished := true
		   | Api_errors.Server_error(code, [ cls; reference ]) when code = Api_errors.handle_invalid && reference = Ref.string_of vbd ->
		       warn "Not pausing VBD %s because it has been deleted" reference;
		       finished := true
		   | Api_errors.Server_error(code, _) when code = Api_errors.vm_not_resident_here ->
		       warn "Pausing VBD %s temporarily failed because VM has just migrated; retrying" (Ref.string_of vbd);
		       (* !finished = false *)
		   | e ->
		       error "Error pausing VBD %s: %s" (Ref.string_of vbd) (ExnHelper.string_of_exn e);
		       raise e (* let this propagate *)
		 done
	      ) vbds;
	    (* Now all the VBDs are paused we can call the main function *)
	    f ()
	 )
	 (fun () ->
	    (* I would have used Helpers.log_exn_continue but I wanted to log the errors as warnings
	       and not as only debug messages *)
	    List.iter
	      (fun (vbd, token) ->
		 try Client.VBD.unpause rpc session_id vbd token
		 with e ->
		   warn "Failed to unpause VBD %s: %s" (Ref.string_of vbd) (ExnHelper.string_of_exn e))
	      !paused_so_far
	 )
    )
	 

