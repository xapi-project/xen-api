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
(** Module that defines API functions for SR objects
 * @group XenAPI functions
 *)
 
open Printf
open Threadext
open Pervasiveext
open Listext
open Db_filter_types
open API
open Client

(* internal api *)

module D=Debug.Debugger(struct let name="xapi" end)
open D

(**************************************************************************************)
(* current/allowed operations checking                                                *)

open Record_util

let all_ops : API.storage_operations_set = 
  [ `scan; `destroy; `forget; `plug; `unplug; `vdi_create; `vdi_destroy; `vdi_resize; `vdi_clone; `vdi_snapshot;
    `vdi_introduce; `update; `pbd_create; `pbd_destroy ]

let sm_cap_table = 
  [ `vdi_create, Smint.Vdi_create;
    `vdi_destroy, Smint.Vdi_delete;
    `vdi_resize, Smint.Vdi_resize;
    `vdi_introduce, Smint.Vdi_introduce;
    `update, Smint.Sr_update;
    (* We fake clone ourselves *)
    `vdi_snapshot, Smint.Vdi_snapshot ]

type table = (API.storage_operations, ((string * (string list)) option)) Hashtbl.t

let capabilities_of_sr_internal ~_type ~uuid =
	try
		Sm.capabilities_of_driver _type
	with Sm.Unknown_driver _ ->
		(* then look to see if this supports the SMAPIv2 *)
		Smint.parse_capabilities (Storage_mux.capabilities_of_sr uuid)

let capabilities_of_sr record =
	capabilities_of_sr_internal record.Db_actions.sR_type record.Db_actions.sR_uuid

(** Returns a table of operations -> API error options (None if the operation would be ok) *)
let valid_operations ~__context record _ref' : table = 
  let _ref = Ref.string_of _ref' in
  let current_ops = record.Db_actions.sR_current_operations in

  let table : table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_ops;
  let set_errors (code: string) (params: string list) (ops: API.storage_operations_set) =
    List.iter (fun op ->
		 if Hashtbl.find table op = None
		 then Hashtbl.replace table op (Some(code, params))) ops in

  (* Policy:
     Anyone may attach and detach VDIs in parallel but we serialise
     vdi_create, vdi_destroy, vdi_resize operations.
     Multiple simultaneous PBD.unplug operations are ok.
  *)

  (* First consider the backend SM capabilities *)
  let sm_caps = capabilities_of_sr record in

  info "SR %s has capabilities: [ %s ]" _ref (String.concat ", " (List.map Smint.string_of_capability sm_caps));

  (* Then filter out the operations we don't want to see for the magic tools SR *)
  let sm_caps = 
    if Helpers.is_tools_sr ~__context ~sr:_ref'
    then List.filter (fun cap -> not(List.mem cap [ Smint.Vdi_create; Smint.Vdi_delete ])) sm_caps
    else sm_caps in

  let forbidden_by_backend = 
    List.filter (fun op -> List.mem_assoc op sm_cap_table && not(List.mem (List.assoc op sm_cap_table) sm_caps))
      all_ops in
  set_errors Api_errors.sr_operation_not_supported [ _ref ] forbidden_by_backend;

  (* CA-70294: if the SR has any attached PBDs, destroy and forget operations are not allowed.*)
  let all_pbds_attached_to_this_sr =
	Db.PBD.get_records_where ~__context ~expr:(And(Eq(Field "SR", Literal _ref), Eq(Field "currently_attached", Literal "true"))) in
  if List.length all_pbds_attached_to_this_sr > 0 then
	set_errors Api_errors.sr_has_pbd [ _ref ] [ `destroy; `forget ]
  else ();

	(* If the SR has no PBDs, destroy is not allowed. *)
	if (Db.SR.get_PBDs ~__context ~self:_ref') = [] then
		set_errors Api_errors.sr_no_pbds [_ref] [`destroy];

	(* If the SR is not empty, destroy is not allowed. *)
	if (Db.SR.get_VDIs ~__context ~self:_ref') <> [] then
		set_errors Api_errors.sr_not_empty [] [`destroy];

  let safe_to_parallelise = [ ] in
  let current_ops = List.setify (List.map snd current_ops) in
  
  (* If there are any current operations, all the non_parallelisable operations
     must definitely be stopped *)
  if current_ops <> []
  then set_errors Api_errors.other_operation_in_progress
    [ "SR"; _ref; sr_operation_to_string (List.hd current_ops) ]
    (List.set_difference all_ops safe_to_parallelise);

  let all_are_parallelisable = List.fold_left (&&) true 
    (List.map (fun op -> List.mem op safe_to_parallelise) current_ops) in
  (* If not all are parallelisable (eg a vdi_resize), ban the otherwise 
     parallelisable operations too *)
  if not(all_are_parallelisable)
  then set_errors  Api_errors.other_operation_in_progress
    [ "SR"; _ref; sr_operation_to_string (List.hd current_ops) ]
    safe_to_parallelise;
  table

let throw_error (table: table) op = 
  if not(Hashtbl.mem table op)
  then raise (Api_errors.Server_error(Api_errors.internal_error, [ Printf.sprintf "xapi_sr.assert_operation_valid unknown operation: %s" (sr_operation_to_string op) ]));

  match Hashtbl.find table op with
  | Some (code, params) -> raise (Api_errors.Server_error(code, params))
  | None -> ()

let assert_operation_valid ~__context ~self ~(op:API.storage_operations) = 
  let all = Db.SR.get_record_internal ~__context ~self in
  let table = valid_operations ~__context all self in
  throw_error table op
    
let update_allowed_operations ~__context ~self : unit =
  let all = Db.SR.get_record_internal ~__context ~self in
  let valid = valid_operations ~__context all self in
  let keys = Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid [] in
  Db.SR.set_allowed_operations ~__context ~self ~value:keys

(** Someone is cancelling a task so remove it from the current_operations *)
let cancel_task ~__context ~self ~task_id = 
  let all = List.map fst (Db.SR.get_current_operations ~__context ~self) in
  if List.mem task_id all then
    begin
      Db.SR.remove_from_current_operations ~__context ~self ~key:task_id;
      update_allowed_operations ~__context ~self
    end

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.SR.get_current_operations ~__context ~self in
  let set = (fun value -> Db.SR.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set
