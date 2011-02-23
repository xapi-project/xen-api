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
(** Common code between the fake and real servers for dealing with VBDs
 * @group Storage
 *)

open Threadext
open Stringext

module D=Debug.Debugger(struct let name="xapi" end)
open D

(**************************************************************************************)
(* current/allowed operations checking                                                *)

open Record_util

let all_ops : API.vbd_operations_set = [ `attach; `eject; `unplug; `unplug_force; `insert; `plug;
					 `pause; `unpause ]

type table = (API.vbd_operations, ((string * (string list)) option)) Hashtbl.t

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

(** Returns a table of operations -> API error options (None if the operation would be ok)
    The flag 'expensive_sharing_checks' indicates whether to perform the VDI sharing checks. 
    We avoid performing these calculations on server start (CA-20808) and therefore end up with
    a slightly underconservative allowed_operations. This is still safe because we always perform
    these calculations in the error path.
 *)
let valid_operations ~expensive_sharing_checks ~__context record _ref' : table = 
  let _ref = Ref.string_of _ref' in
  let current_ops = Listext.List.setify (List.map snd record.Db_actions.vBD_current_operations) in
  (* Policy:
     * current_ops must be empty [ will make exceptions later for eg eject/unplug of attached vbd ]
     * referenced VDI must be exclusively locked for any other task (eg clone)
     * Look up every other VBD pointing to the same VDI as this one and generate the subset
       for which either currently-attached is true or current_operations is non-empty.
       With reference to the VDI.sharable and VDI.read_only flags, perform the sharing
       check. 
     * Consider the powerstate of the VM
     * Exempt the control domain from current-operations checking
   * NB must skip empty VBDs *)
  let table : table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_ops;
  let set_errors (code: string) (params: string list) (ops: API.vbd_operations_set) =
    List.iter (fun op ->
		 if Hashtbl.find table op = None
		 then Hashtbl.replace table op (Some(code, params))) ops in

  let vm = Db.VBD.get_VM ~__context ~self:_ref' in
  let is_control_domain = Db.VM.get_is_control_domain ~__context ~self:vm in

  let safe_to_parallelise = [ `pause; `unpause ] in

  (* Any current_operations preclude everything that isn't safe to parallelise *)
  if current_ops <> [] then begin
    let concurrent_op = List.hd current_ops in
    set_errors Api_errors.other_operation_in_progress 
      [ "VBD"; _ref; vbd_operation_to_string concurrent_op ] 
      (set_difference all_ops safe_to_parallelise);
  end;
  (* If not all operations are parallisable then preclude pause *)
  let all_are_parallelisable = List.fold_left (&&) true 
    (List.map (fun op -> List.mem op safe_to_parallelise) current_ops) in
  (* If not all are parallelisable, ban the otherwise 
     parallelisable operations too *)
  if not(all_are_parallelisable)
  then set_errors  Api_errors.other_operation_in_progress
    [ "VBD"; _ref; vbd_operation_to_string (List.hd current_ops) ]
    [ `pause ];
  (* If something other than `pause `unpause *and* `attach (for VM.reboot, see CA-24282) then disallow unpause *)
  if set_difference current_ops (`attach :: safe_to_parallelise) <> []
  then set_errors  Api_errors.other_operation_in_progress
    [ "VBD"; _ref; vbd_operation_to_string (List.hd current_ops) ]
    [ `unpause ];    


  (* Drives marked as not unpluggable cannot be unplugged *)
  if not(record.Db_actions.vBD_unpluggable)
  then set_errors Api_errors.vbd_not_unpluggable [ _ref ] [ `unplug; `unplug_force ];

  (* Non CD drives cannot be inserted or ejected *)
  if record.Db_actions.vBD_type <> `CD
  then set_errors Api_errors.vbd_not_removable_media [ _ref ] [ `insert; `eject ];

  (* Empty devices cannot be ejected *)
  let empty = Db.VBD.get_empty ~__context ~self:_ref' in
  if empty 
  then set_errors Api_errors.vbd_is_empty [ _ref ] [ `eject ]
  else set_errors Api_errors.vbd_not_empty [ _ref ] [ `insert ]; 

  (* VM must be online to support plug/unplug *)
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  let plugged = record.Db_actions.vBD_currently_attached || record.Db_actions.vBD_reserved in
  (match power_state, plugged with
  | `Running, true -> set_errors Api_errors.device_already_attached [ _ref ] [ `plug ]
  | `Running, false -> set_errors Api_errors.device_already_detached [ _ref ] [ `unplug; `unplug_force ]
  | _, _ -> 
      let actual = Record_util.power_to_string power_state in
      let expected = Record_util.power_to_string `Running in
	  (* If not Running, always block these operations: *)
	  let bad_ops = [ `plug; `unplug; `unplug_force ] in
	  (* However allow VBD pause and unpause if the VM is paused: *)
	  let bad_ops' = if power_state = `Paused then bad_ops else `pause :: `unpause :: bad_ops in
      set_errors Api_errors.vm_bad_power_state [ Ref.string_of vm; expected; actual ] bad_ops');
      
  (* HVM guests only support plug/unplug IF they have recent PV drivers *)
  (* They can only eject/insert CDs not plug/unplug *)
  let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in
  let vm_gmr = try Some (Db.VM_guest_metrics.get_record_internal ~__context ~self:vm_gm) with _ -> None in  
  if power_state = `Running && Helpers.has_booted_hvm ~__context ~self:vm then begin
    (match Xapi_pv_driver_version.make_error_opt (Xapi_pv_driver_version.of_guest_metrics vm_gmr) vm vm_gm with
     | Some(code, params) -> set_errors code params [ `plug; `unplug; `unplug_force ]
     | None -> ());
    if record.Db_actions.vBD_type = `CD
    then set_errors Api_errors.operation_not_allowed 
      [ "HVM CDROMs cannot be hotplugged/unplugged, only inserted or ejected" ] [ `plug; `unplug; `unplug_force ]
  end;

  if not(empty) then begin
    (* Consider VDI operations *)
    let vdi = Db.VBD.get_VDI ~__context ~self:_ref' in
    let vdi_record = Db.VDI.get_record_internal ~__context ~self:vdi in
    if not vdi_record.Db_actions.vDI_managed
    then set_errors Api_errors.vdi_not_managed [ _ref ] all_ops;

    if vdi_record.Db_actions.vDI_current_operations <> [] then begin
      debug "VBD operation %s not allowed because VDI.current-operations = [ %s ]"
	(String.concat ";" (List.map vbd_operation_to_string current_ops))
	(String.concat "; " 
	   (List.map (fun (task, op) -> task ^ " -> " ^ (vdi_operation_to_string op)) vdi_record.Db_actions.vDI_current_operations));
      let concurrent_op = snd (List.hd vdi_record.Db_actions.vDI_current_operations) in
      set_errors Api_errors.other_operation_in_progress
	[ "VDI"; Ref.string_of vdi; vdi_operation_to_string concurrent_op] [ `attach; `plug; `insert ]
    end;
    if not record.Db_actions.vBD_currently_attached && expensive_sharing_checks 
    then begin
      (* Perform the sharing checks *)
      (* Careful to not count this VBD and be careful to be robust to parallel deletions of unrelated VBDs *)
		let vbd_records = 
			let vbds = List.filter (fun vbd -> vbd <> _ref') vdi_record.Db_actions.vDI_VBDs in
			List.concat (List.map (fun self -> try [ Db.VBD.get_record_internal ~__context ~self ] with _ -> []) vbds) in	
	(* Any VBD with a current_operation is said to conflict
	   EXCEPT if I'm a control_domain (running pygrub) and the VBD is being 'attach'ed for booting *)
	let any p xs = try ignore(List.find p xs); true with Not_found -> false in
	let conflicting (_, op) = not (is_control_domain && op = `attach) in
	let pointing_to_a_suspended_VM vbd =
		Db.VM.get_power_state ~__context ~self:(vbd.Db_actions.vBD_VM) = `Suspended in

	let vbds_to_check = List.filter 
	  (fun self -> not (pointing_to_a_suspended_VM self) && (
	     self.Db_actions.vBD_currently_attached || 
	       self.Db_actions.vBD_reserved || (* happens during reboots *)
	       (any conflicting self.Db_actions.vBD_current_operations))) vbd_records in
	let someones_got_rw_access = 
	  try List.find (fun vbd -> vbd.Db_actions.vBD_mode = `RW) vbds_to_check; true with _ -> false
	in
	let need_write = record.Db_actions.vBD_mode = `RW in
	(* Read-only access doesn't require VDI to be marked sharable *)
	if not(vdi_record.Db_actions.vDI_sharable) && someones_got_rw_access
	then set_errors Api_errors.vdi_in_use [ Ref.string_of vdi ] [ `attach; `insert; `plug ];
	if need_write && vdi_record.Db_actions.vDI_read_only 
	then set_errors Api_errors.vdi_readonly [ Ref.string_of vdi ] [ `attach; `insert; `plug ]
      end
  end; (* empty *)
  table

let throw_error (table: table) op = 
  if not(Hashtbl.mem table op)
  then raise (Api_errors.Server_error(Api_errors.internal_error, [ Printf.sprintf "xapi_vbd_helpers.assert_operation_valid unknown operation: %s" (vbd_operation_to_string op) ]));

  match Hashtbl.find table op with
  | Some (code, params) -> raise (Api_errors.Server_error(code, params))
  | None -> ()

let assert_operation_valid ~__context ~self ~(op:API.vbd_operations) = 
  let all = Db.VBD.get_record_internal ~__context ~self in
  let table = valid_operations ~expensive_sharing_checks:true ~__context all self in
  throw_error table op
    
let assert_attachable ~__context ~self = 
  let all = Db.VBD.get_record_internal ~__context ~self in
  let table = valid_operations ~expensive_sharing_checks:true ~__context all self in
  throw_error table `attach

let assert_doesnt_make_vm_non_agile ~__context ~vm ~vdi =
  let pool = Helpers.get_pool ~__context in
  let properly_shared = Helpers.is_sr_properly_shared ~__context ~self:(Db.VDI.get_SR ~__context ~self:vdi) in
  if true
    && Db.Pool.get_ha_enabled ~__context ~self:pool
    && not(Db.Pool.get_ha_allow_overcommit ~__context ~self:pool)
    && Helpers.is_xha_protected ~__context ~self:vm
    && not properly_shared then begin
      warn "Attaching VDI %s makes VM %s not agile" (Ref.string_of vdi) (Ref.string_of vm);
      raise (Api_errors.Server_error(Api_errors.ha_operation_would_break_failover_plan, []))
    end

let update_allowed_operations ~__context ~self : unit =
  let all = Db.VBD.get_record_internal ~__context ~self in
  let valid = valid_operations ~expensive_sharing_checks:false ~__context all self in
  let keys = Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid [] in
  Db.VBD.set_allowed_operations ~__context ~self ~value:keys

(** Someone is cancelling a task so remove it from the current_operations *)
let cancel_task ~__context ~self ~task_id = 
  let all = List.map fst (Db.VBD.get_current_operations ~__context ~self) in
  if List.mem task_id all then
    begin
      Db.VBD.remove_from_current_operations ~__context ~self ~key:task_id;
      update_allowed_operations ~__context ~self
    end

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.VBD.get_current_operations ~__context ~self in
  let set = (fun value -> Db.VBD.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

let clear_current_operations ~__context ~self = 
  if (Db.VBD.get_current_operations ~__context ~self)<>[] then
    begin
      Db.VBD.set_current_operations ~__context ~self ~value:[];
      update_allowed_operations ~__context ~self
    end

(**************************************************************************************)


(** Check if the device string has the right form *)
let valid_device dev =
  let check_rest rest = (* checks the rest of the device name = [] is ok, or a number is ok *)
    if rest=[] 
    then true 
    else
      try ignore(int_of_string (String.implode rest)); true
      with _ -> false
  in
    dev = "autodetect" ||
      match String.explode dev with
	| 's' :: 'd' :: ('a'..'p') :: rest -> check_rest rest
	| 'x' :: 'v' :: 'd' :: ('a'..'p') :: rest -> check_rest rest
	| 'h' :: 'd' :: ('a'..'p') :: rest -> check_rest rest
	| _ -> try let n = int_of_string dev in n >= 0 || n <16 with _ -> false

(** Hold this mutex while resolving the 'autodetect' device names to prevent two concurrent
    VBD.creates racing with each other and choosing the same device. For simplicity keep this
    as a global lock rather than a per-VM one. Rely on the fact that the message forwarding layer
    always runs this code on the master. *)
let autodetect_mutex = Mutex.create ()

(** VBD.create doesn't require any interaction with xen *)
let create  ~__context ~vM ~vDI ~userdevice ~bootable ~mode ~_type ~unpluggable ~empty
           ~qos_algorithm_type ~qos_algorithm_params ~other_config : API.ref_VBD =

	if not empty then begin
	  let sr = Db.VDI.get_SR ~__context ~self:vDI in
	  let vdi_type = Db.VDI.get_type ~__context ~self:vDI in
	  if not(List.mem vdi_type [ `system; `user; `ephemeral; `suspend; `crashdump; `metadata])
	  then raise (Api_errors.Server_error(Api_errors.vdi_incompatible_type, [ Ref.string_of vDI; Record_util.vdi_type_to_string vdi_type ]))
	end;
	
	(* All "CD" VBDs must be readonly *)
	if _type = `CD && mode <> `RO
	then raise (Api_errors.Server_error(Api_errors.vbd_cds_must_be_readonly, []));
	(* Only "CD" VBDs may be empty *)
	if _type <> `CD && empty
	then raise (Api_errors.Server_error(Api_errors.vbd_not_removable_media, [ "in constructor" ]));

	(* Prevent VBDs being created which are of type "CD" which are
	   not either .iso files or CD block devices *)
 	if _type = `CD && not(empty) 
	then Xapi_vdi_helpers.assert_vdi_is_valid_iso ~__context ~vdi:vDI;
	(* Prevent RW VBDs being created pointing to RO VDIs *)
	if mode = `RW && Db.VDI.get_read_only ~__context ~self:vDI
	then raise (Api_errors.Server_error(Api_errors.vdi_readonly, [ Ref.string_of vDI ]));

	Mutex.execute autodetect_mutex
	  (fun () ->
	     let possibilities = Xapi_vm_helpers.allowed_VBD_devices ~__context ~vm:vM in

             if not (valid_device userdevice) || (userdevice = "autodetect" && possibilities = []) then 
               raise (Api_errors.Server_error (Api_errors.invalid_device,[userdevice]));
	     
	     (* Resolve the "autodetect" into a fixed device name now *)
	     let userdevice = if userdevice = "autodetect" 
	     then List.hd possibilities (* already checked for [] above *)
	     else userdevice in
	     
	     let uuid = Uuid.make_uuid () in
	     let ref = Ref.make () in
	     debug "VBD.create (device = %s; uuid = %s; ref = %s)" 
	       userdevice (Uuid.string_of_uuid uuid) (Ref.string_of ref);
	     
	     (* Check that the device is definitely unique. If the requested device is numerical
		(eg 1) then we 'expand' it into other possible names (eg 'hdb' 'xvdb') to detect
		all possible clashes. *)
	     let possible_devices = Listext.List.setify [ userdevice; Vbdops.translate_vbd_device userdevice false; Vbdops.translate_vbd_device userdevice true ] in

	     let all = Db.VM.get_VBDs ~__context ~self:vM in
	     
	     let all_devices = List.map (fun self -> Db.VBD.get_device ~__context ~self) all in
	     let all_devices2 = List.map (fun self -> Db.VBD.get_userdevice ~__context ~self) all in
	     
	     (* Fail if the device is already in use by another vbd (NB 'autodetect' was resolved above) *)
	     if Listext.List.intersect possible_devices (all_devices @ all_devices2) <> []
	     then raise (Api_errors.Server_error (Api_errors.device_already_exists, [userdevice]));

	     (* Make people aware that non-shared disks make VMs not agile *)
	     if not empty then assert_doesnt_make_vm_non_agile ~__context ~vm:vM ~vdi:vDI;
	     
	     let metrics = Ref.make () and metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	     Db.VBD_metrics.create ~__context ~ref:metrics ~uuid:metrics_uuid
	       ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.)
	       ~other_config:[];
	     
	     Db.VBD.create ~__context ~ref ~uuid:(Uuid.to_string uuid)
	       ~current_operations:[] ~allowed_operations:[] ~storage_lock:false
	       ~vM ~vDI ~userdevice ~device:"" ~bootable ~mode ~_type ~unpluggable ~empty ~reserved:false
	       ~qos_algorithm_type ~qos_algorithm_params ~qos_supported_algorithms:[]
	       ~currently_attached:false
	       ~status_code:Int64.zero ~status_detail:""
               ~runtime_properties:[] ~other_config
	       ~metrics;
	     update_allowed_operations ~__context ~self:ref;
	     ref
	  ) 

(** VBD.destroy doesn't require any interaction with xen *)
let destroy  ~__context ~self =
	debug "VBD.destroy (uuid = %s; ref = %s)" (Db.VBD.get_uuid ~__context ~self) (Ref.string_of self);
	let r = Db.VBD.get_record_internal ~__context ~self in
	let vm = r.Db_actions.vBD_VM in
	
	(* Force the user to unplug first *)
	if r.Db_actions.vBD_currently_attached || r.Db_actions.vBD_reserved
	then raise (Api_errors.Server_error(Api_errors.operation_not_allowed, 
		[Printf.sprintf "VBD '%s' still attached to '%s'" r.Db_actions.vBD_uuid (Db.VM.get_uuid __context vm)]));

	let metrics = Db.VBD.get_metrics ~__context ~self in
	(* Don't let a failure to destroy the metrics stop us *)
	Helpers.log_exn_continue "VBD_metrics.destroy" 
	  (fun self -> Db.VBD_metrics.destroy ~__context ~self) metrics;
	Db.VBD.destroy ~__context ~self

(** Type of a function which does the actual hotplug/ hotunplug *)
type do_hotplug_fn = __context:Context.t -> vbd:API.ref_VBD -> unit

(* copy a vbd *)
let copy ~__context ?vdi ~vm vbd =
	let all = Db.VBD.get_record ~__context ~self:vbd in
	let new_vbd = Ref.make () in
	let vbd_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let metrics = Ref.make () in
	let metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let vdi = Pervasiveext.default all.API.vBD_VDI vdi in 
	Db.VBD_metrics.create ~__context
		~ref:metrics
		~uuid:metrics_uuid
		~io_read_kbs:0.
		~io_write_kbs:0.
		~last_updated:(Date.of_float 0.)
		~other_config:[];
	Db.VBD.create ~__context
		~ref:new_vbd
		~uuid:vbd_uuid
		~allowed_operations:[]
		~current_operations:[]
		~storage_lock:false
		~vM:vm
		~vDI:vdi
		~empty:(all.API.vBD_empty || vdi = Ref.null)
		~reserved:false
		~userdevice:all.API.vBD_userdevice
		~device:all.API.vBD_device
		~bootable:all.API.vBD_bootable
		~mode:all.API.vBD_mode
		~currently_attached:all.API.vBD_currently_attached
		~status_code:0L
		~_type:all.API.vBD_type
		~unpluggable:all.API.vBD_unpluggable
		~status_detail:""
		~other_config:all.API.vBD_other_config
		~qos_algorithm_type:all.API.vBD_qos_algorithm_type
		~qos_algorithm_params:all.API.vBD_qos_algorithm_params
		~qos_supported_algorithms:[]
		~runtime_properties:[]
		~metrics:metrics;
	new_vbd
