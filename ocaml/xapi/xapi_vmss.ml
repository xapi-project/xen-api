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
module D = Debug.Make(struct let name="xapi" end)
open D

open Fun

let vmss_plugin = "vmss"
let vmss_username = "__dom0__vmss"
let vmss_snapshot_other_config_show_in_xencenter = "ShowInXenCenter"
let vmss_snapshot_other_config_applies_to = "applies_to"

(* Create VM snapshots just after creating a VMSS *)
let snapshot_now ~__context ~vmss =
	let vmss_uuid = Db.VMSS.get_uuid ~__context ~self:vmss in
	let args = [ "vmss_uuid", vmss_uuid ] in
	Xapi_plugins.call_plugin
	(Context.get_session_id __context)
	vmss_plugin
	"snapshot_now"
	args

(* mini datamodel for type and key value restrictions in the vmss map fields *)
type key_type = Enum of string list | EnumSet of string list | IntRange of int*int | String | ReqValue of string
let schedule_days_enum = ["Monday";"Tuesday";"Wednesday";"Thursday";"Friday";"Saturday";"Sunday"]
let schedule_frequency_hourly = "hourly"
let schedule_frequency_daily = "daily"
let schedule_frequency_weekly = "weekly"
let frequency_order = [schedule_frequency_hourly;schedule_frequency_daily;schedule_frequency_weekly]
let schedule_min_enum = ["0";"15";"30";"45"]
let schedule_field = "schedule"
let schedule_min_default = List.hd schedule_min_enum
let schedule_hour_default = "0"
let schedule_days_default = List.hd schedule_days_enum

(* relations between map types and map keys *)
let schedule_frequency_enum = [schedule_frequency_hourly;schedule_frequency_daily;schedule_frequency_weekly]
let schedule_frequency_hourly_keys = schedule_field,[schedule_frequency_hourly,[Datamodel.vmss_schedule_min, ((Enum schedule_min_enum), schedule_min_default)]]
let schedule_frequency_daily_keys = schedule_field,[schedule_frequency_daily,[Datamodel.vmss_schedule_hour, ((IntRange(0,23)), schedule_hour_default);Datamodel.vmss_schedule_min, ((Enum schedule_min_enum), schedule_min_default)]]
let schedule_frequency_weekly_keys = schedule_field,[schedule_frequency_weekly,[Datamodel.vmss_schedule_hour, ((IntRange(0,23)), schedule_hour_default);Datamodel.vmss_schedule_min, ((Enum schedule_min_enum), schedule_min_default);Datamodel.vmss_schedule_days, ((EnumSet schedule_days_enum), schedule_days_default)]]

(* look-up structures, contain allowed map keys in a specific map type *)
let schedule_keys = schedule_field, (List.map
	(function (f,[k]) -> k
		| _ -> assert false
	)
	[schedule_frequency_hourly_keys;schedule_frequency_daily_keys;schedule_frequency_weekly_keys])

(* look-up structures, contain allowed map keys in all map types *)
let schedule_all_keys = schedule_field,["",(List.fold_left (fun acc (sf,ks)->acc@ks) [] (let (f,kss)=schedule_keys in kss))]

(* functions to assert the mini datamodel above *)

let err field key value =
	let msg = if key="" then field else field^":"^key in
	raise (Api_errors.Server_error (Api_errors.invalid_value, [msg;value]))

let mem value range =
	try Some
		(List.find
			(fun r->(String.lowercase value)=(String.lowercase r))
			range
		)
	with Not_found -> None

let assert_value ~field ~key ~attr ~value =
	let err v = err field key v in
	let (ty,default) = attr in
	match ty with
		| Enum range -> (match (mem value range) with None->err value|Some v->v)
		| EnumSet range -> (* enumset is a comma-separated string *)
			let vs = Xstringext.String.split ',' value in
			List.fold_right 
				(fun v acc->match (mem v range) with
					|None->err v
					|Some v->if acc="" then v else (v^","^acc)
				)
				vs ""
		| IntRange (min,max) ->
			let v=try int_of_string value with _->err value in
			if (v<min || v>max) then err value else value
		| ReqValue required_value -> if value <> required_value then err value else value
		|String -> value

let with_ks ~kss ~fn =
	let field,kss=kss in
	let corrected_values = List.filter (fun cv->cv<>None) (List.map (fun ks-> fn field ks) kss) in
	if List.length corrected_values < 1 then []
	else (match List.hd corrected_values with None->[]|Some cv->cv)

let assert_req_values ~field ~ks ~vs =
	(* each required values in this ks must match the one in the vs map this key/value belongs to*)
	let req_values = List.fold_right
		(fun (k,attr) acc->match attr with(ReqValue rv),_->(k,rv)::acc|_->acc) ks []
	in
	(if vs<>[] then
		List.iter (fun (k,rv)->
			if (List.mem_assoc k vs) then (if rv<>(List.assoc k vs) then err field k rv)
		) req_values
	)

let merge xs ys = (* uses xs elements to overwrite ys elements *)
	let nys = List.map (fun (ky,vy)->if List.mem_assoc ky xs then (ky,(List.assoc ky xs)) else (ky,vy)) ys in
	let nxs = List.filter (fun (kx,_)->not(List.mem_assoc kx nys)) xs in
	nxs@nys

let assert_key ~field ~ks ~key ~value =
	debug "assert_key: field=%s key=[%s] value=[%s]" field key value;
	(* check if the key and value conform to this ks *)
	(if not (List.mem_assoc key ks)
		then err field key value
	else
		assert_value ~field ~key ~attr:(List.assoc key ks) ~value
	)

let assert_keys ~ty ~ks ~value ~db =
	let value = merge value db in
	with_ks ~kss:ks ~fn:
		(fun field (xt,ks) ->
			debug "assert_keys: field=%s xt=[%s] ty=[%s]" field xt ty;
			if (xt=ty) then Some
			(
				assert_req_values ~field ~ks ~vs:value;
				(* for this ks, each key value must be valid *)
				List.map (fun (k,v)-> k,(assert_key ~field ~ks ~key:k ~value:v)) value
			)
			else None
		)

let assert_all_keys ~ty ~ks ~value ~db =
	let value = merge value db in
	with_ks ~kss:ks ~fn:
		(fun field (xt,ks)->
			debug "assert_all_keys: field=%s xt=[%s] ty=[%s]" field xt ty;
			if (xt=ty) then Some
			(
				assert_req_values ~field ~ks ~vs:value;
				(* add missing keys with default values *)
				let value = List.map (fun (k,(kt,default))->if List.mem_assoc k value then (k,(List.assoc k value)) else (k,default)) ks in
				
				(* remove extra unexpected keys *)
				let value = List.fold_right (fun (k,v) acc->if List.mem_assoc k ks then (k,v)::acc else acc) value [] in
				(* for this ks, each key value must be valid *)
				List.map (fun (k,v)-> k,(assert_key ~field ~ks ~key:k ~value:v)) value
			)
			else None
		)

let assert_set_frequency ~frequency ~schedule=
	let ty = Record_util.vmss_frequency_to_string frequency in
	assert_all_keys ~ty ~ks:schedule_keys ~value:schedule ~db:schedule

let assert_retention_value ~retention_value =
	let value = retention_value in
	(if (value < 1L) || (value > 10L)
	then
		err "retention_value" "" (Printf.sprintf "%Li" value)
	)

let set_frequency ~__context ~self ~value =
	let schedule = Db.VMSS.get_schedule ~__context ~self in
	let new_schedule = assert_set_frequency ~frequency:value ~schedule in
	Db.VMSS.set_frequency ~__context ~self ~value;
	(* update dependent maps *)
	Db.VMSS.set_schedule ~__context ~self ~value:new_schedule

let set_schedule ~__context ~self ~value =
	let value = assert_keys ~ty:"" ~ks:schedule_all_keys ~value ~db:(Db.VMSS.get_schedule ~__context ~self) in
	Db.VMSS.set_schedule ~__context ~self ~value

let set_type ~__context ~self ~value =
	(* Check VMs associated to VMSS supports the new snapshot type *)
	let snapshot_type = Record_util.vmss_type_to_string value in
	let vms = Db.VMSS.get_VMs ~__context ~self in
	if not (vms = []) then
		List.iter (fun vm ->
			let allowed_operations = Db.VM.get_allowed_operations ~__context ~self:vm in
			if not (List.exists (fun ty -> (Record_util.vm_operation_to_string ty) = snapshot_type)
				allowed_operations) then
				raise (Api_errors.Server_error(Api_errors.operation_not_allowed,
					[Ref.string_of vm ; "VM doesn't support snapshot_type " ^ snapshot_type]));
		) vms;
	Db.VMSS.set_type ~__context ~self ~value

let add_to_schedule ~__context ~self ~key ~value =
	let value = List.assoc key (assert_keys ~ty:"" ~ks:schedule_all_keys ~value:[(key,value)] ~db:(Db.VMSS.get_schedule ~__context ~self)) in
	Db.VMSS.add_to_schedule ~__context ~self ~key ~value

let remove_from_schedule ~__context ~self ~key =
	Db.VMSS.remove_from_schedule ~__context ~self ~key

let set_last_run_time ~__context ~self ~value =
	Db.VMSS.set_last_run_time ~__context ~self ~value

let set_retention_value ~__context ~self ~value =
	assert_retention_value ~retention_value:value;
	Db.VMSS.set_retention_value ~__context ~self ~value

(* VMSS constructors/destructors *)

let create ~__context ~name_label ~name_description ~enabled
	~_type ~retention_value ~frequency ~schedule
	: API.ref_VMSS =
	
	(* assert all provided field values, key names and key values are valid *)
	let (_: (string*string) list) = assert_keys ~ty:(Record_util.vmss_frequency_to_string frequency) ~ks:schedule_keys ~value:schedule ~db:[] in
	
	(* assert inter-field constraints and fix values if possible *)
	let schedule = assert_set_frequency ~frequency ~schedule in
	
	(* other constraints *)
	assert_retention_value ~retention_value;
	let ref=Ref.make() in
	let uuid=Uuid.to_string (Uuid.make_uuid()) in
	Db.VMSS.create ~__context ~ref ~uuid
		~name_label ~name_description ~enabled ~_type
		~retention_value ~frequency ~schedule ~last_run_time:(Date.of_float 0.);
	ref

let destroy_all_messages ~__context ~self =
	let uuid = Db.VMSS.get_uuid ~__context ~self in
	Xapi_message.get_all_records ~__context
		|> List.filter (fun (_, record) -> record.API.message_obj_uuid = uuid)
		|> List.iter (fun (ref, _) -> Xapi_message.destroy ~__context ~self:ref)

let destroy ~__context ~self =
	let vms = Db.VMSS.get_VMs ~__context ~self in
	if List.length vms > 0
	then ( (* we can't delete a VMSS that contains VMs *)
		raise (Api_errors.Server_error (Api_errors.vmss_has_vm,[]))
	)
	else (
		destroy_all_messages ~__context ~self;
		Db.VMSS.destroy ~__context ~self
	)

(* Verify if snapshot is happening due to a VM Schedule Snapshot *)
let is_snapshot_from_vmss ~__context =
	try
		(let session = Xapi_session.get_top ~__context ~self:(Context.get_session_id __context) in
		let uname = Db.Session.get_auth_user_name ~__context ~self:session in
		let is_lsu = Db.Session.get_is_local_superuser ~__context ~self:session in
		is_lsu && (uname = vmss_username)
		)
	with e ->
		debug "Error obtaining is_snapshot_from_vmss: %s" (Printexc.to_string e);
		false

let show_task_in_xencenter ~__context ~vm =
	if is_snapshot_from_vmss ~__context then
		(
			let task = Context.get_task_id __context in
			try
				debug "show_in_xencenter: task=%s" (Ref.string_of task);
				(* this key is used to make sure the snapshotting task *)
				(* is seen from all xencenter clients *)
				Db.Task.add_to_other_config ~__context ~self:task
					~key:vmss_snapshot_other_config_show_in_xencenter
					~value:"";
				Db.Task.add_to_other_config ~__context ~self:task
					~key:vmss_snapshot_other_config_applies_to
					~value:(Ref.string_of vm)
			with e->
				debug "Error adding other_config:show_in_xencenter to task %s: %s"
					(Ref.string_of task) (Printexc.to_string e)
		)

