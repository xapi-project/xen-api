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

module D=Debug.Make(struct let name="xapi" end)
open D

open Db_filter
open Record_util
open Threadext
open Api_errors

let all_operations = [ `ha_enable; `ha_disable ]

(** Returns a table of operations -> API error options (None if the operation would be ok) *)
let valid_operations ~__context record _ref' =
	let _ref = Ref.string_of _ref' in
	let current_ops = List.map snd record.Db_actions.pool_current_operations in

	let table = Hashtbl.create 10 in
	List.iter (fun x -> Hashtbl.replace table x None) all_operations;
	let set_errors (code: string) (params: string list) (ops: API.pool_allowed_operations_set) =
		List.iter (fun op ->
			if Hashtbl.find table op = None
				then Hashtbl.replace table op (Some(code, params))) ops in

	(* HA enable or disable cannot run if HA enable is in progress *)
	if List.mem `ha_enable current_ops
		then begin
			set_errors Api_errors.ha_enable_in_progress [] [ `ha_enable ];
			set_errors Api_errors.ha_enable_in_progress [] [ `ha_disable ]
		end;
	(* HA enable or disable cannot run if HA disable is in progress *)
	if List.mem `ha_disable current_ops
		then begin
			set_errors Api_errors.ha_disable_in_progress [] [ `ha_enable ];
			set_errors Api_errors.ha_disable_in_progress [] [ `ha_disable ]
		end;
	
	(* HA disable cannot run if HA is already disabled on a pool *)
	(* HA enable cannot run if HA is already enabled on a pool *)
	let ha_enabled = Db.Pool.get_ha_enabled ~__context ~self:(Helpers.get_pool ~__context) in
	if ha_enabled then
		set_errors Api_errors.ha_is_enabled [] [ `ha_enable ]
	else
		set_errors Api_errors.ha_not_enabled [] [ `ha_disable ];
	
	table

let throw_error table op =
	if not(Hashtbl.mem table op)
		then raise (Api_errors.Server_error(Api_errors.internal_error, [ Printf.sprintf "xapi_pool_helpers.assert_operation_valid unknown operation: %s" (pool_operation_to_string op) ]));

	match Hashtbl.find table op with
	| Some (code, params) -> raise (Api_errors.Server_error(code, params))
	| None -> ()

let assert_operation_valid ~__context ~self ~(op:API.pool_allowed_operations) =
	let all = Db.Pool.get_record_internal ~__context ~self in
	let table = valid_operations ~__context all self in
	throw_error table op

let update_allowed_operations ~__context ~self : unit =
	let all = Db.Pool.get_record_internal ~__context ~self in
	let valid = valid_operations ~__context all self in
	let keys = Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid [] in
	Db.Pool.set_allowed_operations ~__context ~self ~value:keys

(* Checks whether HA enable is in progress *)
let ha_enable_in_progress ~__context =
	let pool = Helpers.get_pool ~__context in
	let current_ops = Db.Pool.get_current_operations ~__context ~self:pool in
	if List.exists (fun (_, x) -> x = `ha_enable) current_ops then true else false

(* Checks whether HA disable is in progress *)
let ha_disable_in_progress ~__context =
	let pool = Helpers.get_pool ~__context in
	let current_ops = Db.Pool.get_current_operations ~__context ~self:pool in
	if List.exists (fun (_, x) -> x = `ha_disable) current_ops then true else false

