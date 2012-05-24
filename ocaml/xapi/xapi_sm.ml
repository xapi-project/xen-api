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
(**
 * @group Storage
 *)

(* The SMAPIv1 plugins are a static set in the filesystem.
   The SMAPIv2 plugins are a dynamic set hosted in driver domains. *)

open Listext
open Stringext
open Fun

(* We treat versions as '.'-separated integer lists under the usual
   lexicographic ordering. *)
type version = int list
let version_of_string = List.map int_of_string ++ (String.split '.')
 
module D=Debug.Debugger(struct let name="xapi" end)
open D

let create_from_query_result ~__context q =
	let r = Ref.make () and u = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	let open Storage_interface in
	Db.SM.create ~__context ~ref:r ~uuid:u ~_type:(String.lowercase q.driver)
		 ~name_label:q.name
		 ~name_description:q.description
		 ~vendor:q.vendor
		 ~copyright:q.copyright
		 ~version:q.version
		 ~required_api_version:q.required_api_version
		 ~capabilities:q.features
		 ~configuration:q.configuration
		 ~other_config:[]
		 ~driver_filename:(Sm_exec.cmd_name q.driver)

let update_from_query_result ~__context (self, r) query_result =
	let open Storage_interface in
	let _type = String.lowercase query_result.driver in
	let driver_filename = Sm_exec.cmd_name query_result.driver in
	if r.API.sM_type <> _type
	then Db.SM.set_type ~__context ~self ~value:_type;
	if r.API.sM_name_label <> query_result.name
	then Db.SM.set_name_label ~__context ~self ~value:query_result.name;
	if r.API.sM_name_description <> query_result.description
	then Db.SM.set_name_description ~__context ~self ~value:query_result.description;
	if r.API.sM_vendor <> query_result.vendor
	then Db.SM.set_vendor ~__context ~self ~value:query_result.vendor;
	if r.API.sM_copyright <> query_result.copyright
	then Db.SM.set_copyright ~__context ~self ~value:query_result.copyright;
	if r.API.sM_required_api_version <> query_result.required_api_version
	then Db.SM.set_required_api_version ~__context ~self ~value:query_result.required_api_version;
	if r.API.sM_capabilities <> query_result.features
	then Db.SM.set_capabilities ~__context ~self ~value:query_result.features;
	if r.API.sM_configuration <> query_result.configuration
	then Db.SM.set_configuration ~__context ~self ~value:query_result.configuration;
	if r.API.sM_driver_filename <> driver_filename
	then Db.SM.set_driver_filename ~__context ~self ~value:driver_filename

(** Update all SMAPIv1 plugins which have been deleted from the filesystem.
    The SMAPIv2 ones are dynamically discovered so we leave those alone. *)
let on_xapi_start ~__context =
	let existing = List.map (fun (rf, rc) -> rc.API.sM_type, (rf, rc)) (Db.SM.get_all_records ~__context) in
	let drivers = Sm.supported_drivers () in
	(* Delete all SM records except those for SMAPIv1 plugins *)
	List.iter
		(fun ty ->
			let self, rc = List.assoc ty existing in
			if version_of_string rc.API.sM_version < [ 2; 0 ] then begin
				info "Unregistering SM plugin %s since version (%s) < 2.0 and executable is missing" ty rc.API.sM_version;
				Db.SM.destroy ~__context ~self
			end
		) (List.set_difference (List.map fst existing) drivers);
	(* Create all missing SMAPIv1 plugins *)
	List.iter
		(fun ty ->
			let query_result = Sm.info_of_driver ty |> Smint.query_result_of_sr_driver_info in
			create_from_query_result ~__context query_result
		) (List.set_difference drivers (List.map fst existing));
	(* Update all existing SMAPIv1 plugins *)
	List.iter
		(fun ty ->
			let query_result = Sm.info_of_driver ty |> Smint.query_result_of_sr_driver_info in
			update_from_query_result ~__context (List.assoc ty existing) query_result
		) (List.intersect drivers (List.map fst existing))

let unregister_plugin ~__context query_result =
	let open Storage_interface in
	let existing = List.map (fun (rf, rc) -> rc.API.sM_type, (rf, rc)) (Db.SM.get_all_records ~__context) in
	let driver = String.lowercase query_result.driver in
	if List.mem_assoc driver existing then begin
		info "Unregistering SM plugin %s (version %s)" driver query_result.version;
		Db.SM.destroy ~__context ~self:(fst (List.assoc driver existing))
	end

let register_plugin ~__context query_result =
	unregister_plugin ~__context query_result;
	create_from_query_result ~__context query_result
