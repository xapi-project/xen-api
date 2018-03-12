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

open Stdext
open Listext
open Threadext
open Xstringext

module D=Debug.Make(struct let name="xapi" end)
open D

(**************************************************************************************)
(* current/allowed operations checking                                                *)

open Record_util

let all_ops : API.vbd_operations_set = [ `attach; `eject; `unplug; `unplug_force; `insert; `plug;
                                         `pause; `unpause ]

type table = (API.vbd_operations, ((string * (string list)) option)) Hashtbl.t

(** Returns a table of operations -> API error options (None if the operation would be ok)
    The flag 'expensive_sharing_checks' indicates whether to perform the VDI sharing checks.
    We avoid performing these calculations on server start (CA-20808) and therefore end up with
    a slightly underconservative allowed_operations. This is still safe because we always perform
    these calculations in the error path.
*)
let valid_operations ~expensive_sharing_checks ~__context record _ref' : table =
  let _ref = Ref.string_of _ref' in
  let current_ops = List.setify (List.map snd record.Db_actions.vBD_current_operations) in
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
  let vm_r = Db.VM.get_record ~__context ~self:vm in
  let is_system_domain = System_domains.is_system_domain vm_r in

  let safe_to_parallelise = [ `pause; `unpause ] in

  (* Any current_operations preclude everything that isn't safe to parallelise *)
  if current_ops <> [] then begin
    let concurrent_op = List.hd current_ops in
    set_errors Api_errors.other_operation_in_progress
      [ "VBD"; _ref; vbd_operation_to_string concurrent_op ]
      (List.set_difference all_ops safe_to_parallelise);
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
  if List.set_difference current_ops (`attach :: safe_to_parallelise) <> []
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

  (* VBD plug/unplug must fail for current_operations
   * like [clean_shutdown; hard_shutdown; suspend; pause] on VM *)
  let vm_current_ops = Db.VM.get_current_operations ~__context ~self:vm in
  List.iter (fun (task,op) ->
      if List.mem op [ `clean_shutdown; `hard_shutdown; `suspend; `pause ] then begin
        let current_op_str = "Current operation on VM:" ^ (Ref.string_of vm) ^ " is "
                             ^ (Record_util.vm_operation_to_string op) in
        set_errors Api_errors.operation_not_allowed [ current_op_str ] [ `plug; `unplug ]
      end
    ) vm_current_ops;

  (* HVM guests MAY support plug/unplug IF they have PV drivers. Assume
   * all drivers have such support unless they specify that they do not. *)
  (* They can only eject/insert CDs not plug/unplug *)
  let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in
  let vm_gmr = try Some (Db.VM_guest_metrics.get_record_internal ~__context ~self:vm_gm) with _ -> None in
  let metrics = Db.VM.get_metrics ~__context ~self:vm in
  let domain_type = Db.VM_metrics.get_current_domain_type ~__context ~self:metrics in
  if power_state = `Running && (domain_type = `hvm) then begin
    let plug_ops = [ `plug; `unplug; `unplug_force ] in
    let fallback () =
      match Xapi_pv_driver_version.make_error_opt (Xapi_pv_driver_version.of_guest_metrics vm_gmr) vm with
      | Some(code, params) -> set_errors code params plug_ops
      | None -> ()
    in
    (match vm_gmr with
     | None -> fallback ()
     | Some gmr ->
       (match gmr.Db_actions.vM_guest_metrics_can_use_hotplug_vbd with
        | `yes -> () (* Drivers have made an explicit claim of support. *)
        | `no -> set_errors Api_errors.operation_not_allowed ["VM states it does not support VBD hotplug."] plug_ops
        (* according to xen docs PV drivers are enough for this to be possible *)
        | `unspecified when gmr.Db_actions.vM_guest_metrics_PV_drivers_detected -> ()
        | `unspecified -> fallback ())
    );
    if record.Db_actions.vBD_type = `CD
    then set_errors Api_errors.operation_not_allowed
        [ "HVM CDROMs cannot be hotplugged/unplugged, only inserted or ejected" ] plug_ops
  end;

  (* When a VM is suspended, no operations are allowed for CD. *)
  if record.Db_actions.vBD_type = `CD && power_state = `Suspended then begin
    let expected = String.concat ", " (List.map Record_util.power_to_string [`Halted; `Running]) in
    let error_params = [Ref.string_of vm; expected; Record_util.power_to_string `Suspended] in
    set_errors Api_errors.vm_bad_power_state error_params [`insert; `eject] (* `attach required for resume *)
  end;

  if not(empty) then begin
    (* Consider VDI operations *)
    let vdi = Db.VBD.get_VDI ~__context ~self:_ref' in
    let vdi_record = Db.VDI.get_record_internal ~__context ~self:vdi in
    if not vdi_record.Db_actions.vDI_managed
    then set_errors Api_errors.vdi_not_managed [ _ref ] all_ops;

    let vdi_operations_besides_copy =
      let is_illegal_operation = function
        | `mirror
        | `copy -> false
        | _     -> true
      in
      List.exists
        (fun (_, operation) -> is_illegal_operation operation)
        vdi_record.Db_actions.vDI_current_operations
    in
    if vdi_operations_besides_copy then begin
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
      let pointing_to_a_suspended_VM vbd =
        Db.VM.get_power_state ~__context ~self:(vbd.Db_actions.vBD_VM) = `Suspended in
      let pointing_to_a_system_domain vbd =
        System_domains.get_is_system_domain ~__context ~self:(vbd.Db_actions.vBD_VM) in

      let vbds_to_check = List.filter
          (fun self ->
             not (pointing_to_a_suspended_VM self) (* these are really offline *)
             && not (pointing_to_a_system_domain self) (* these can share the disk safely *)
             && (
               self.Db_actions.vBD_currently_attached
               || self.Db_actions.vBD_reserved
               || self.Db_actions.vBD_current_operations <> []
             )
          ) vbd_records in
      let someones_got_rw_access =
        try let (_: Db_actions.vBD_t) = List.find (fun vbd -> vbd.Db_actions.vBD_mode = `RW) vbds_to_check in true with _ -> false
      in
      let need_write = record.Db_actions.vBD_mode = `RW in
      (* Read-only access doesn't require VDI to be marked sharable *)
      if not(vdi_record.Db_actions.vDI_sharable) && (not is_system_domain)
         && (someones_got_rw_access || need_write && vbds_to_check <> [])
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
  let properly_shared = Agility.is_sr_properly_shared ~__context ~self:(Db.VDI.get_SR ~__context ~self:vdi) in
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
