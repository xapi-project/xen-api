(*
 * Copyright (C) Citrix Systems Inc.
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
open OUnit
open Test_common

type state = {
	master: API.ref_host;
	sr : API.ref_SR;
	master_pbd: API.ref_PBD;
	slave_pbds: API.ref_PBD list;
}

let setup ~__context =
	let __context = make_test_database () in
	let master = Helpers.get_localhost ~__context in
	let slaves =
		Array.(make 10 () |> to_list)
		|> List.map (fun () -> make_host ~__context ()) in
	let sr = make_sr ~__context () in
	let master_pbd =
		make_pbd ~__context ~sR:sr ~host:master ~currently_attached:false () in
	let slave_pbds = List.map (fun host ->
		make_pbd ~__context ~sR:sr ~host ~currently_attached:false ()
	) slaves in
	{ master; sr; master_pbd; slave_pbds }

let set_state ~__context ~plugged = List.iter (fun pbd ->
	Db.PBD.set_currently_attached ~__context ~self:pbd ~value:plugged)

let test_pbd_plug_constraint () =
	let __context = make_test_database () in
	let {master; sr; master_pbd; slave_pbds} = setup ~__context in
	(* No PBDs, plugged => constraint not met *)
	assert_raises_api_error
		~args:[Ref.string_of sr; Ref.string_of master]
		Api_errors.sr_detached_on_master
		(fun () -> Xapi_pbd.check_plugged_on_master_constraint ~__context ~sr);
	(* Doesn't matter how many slave PBDs are plugged => constraint not met *)
	List.iter (fun slave_pbd ->
		set_state ~__context ~plugged:true [slave_pbd];
		assert_raises_api_error
			~args:[Ref.string_of sr; Ref.string_of master]
			Api_errors.sr_detached_on_master
			(fun () -> Xapi_pbd.check_plugged_on_master_constraint ~__context ~sr);
	) slave_pbds;
	(* All PBDs plugged => constraint met *)
	set_state ~__context ~plugged:true [master_pbd];
	Xapi_pbd.check_plugged_on_master_constraint ~__context ~sr;
	(* Only master PBD plugged => constraint met *)
	set_state ~__context ~plugged:false slave_pbds;
	Xapi_pbd.check_plugged_on_master_constraint ~__context ~sr

let test_pbd_unplug_constraint () =
	let __context = make_test_database () in
	let {master; sr; master_pbd; slave_pbds} = setup ~__context in
	(* No PBDs, plugged => constraint met *)
	Xapi_pbd.check_unplugged_on_slaves_constraint ~__context ~sr;
	(* Master PBD plugged => constraint met *)
	set_state ~__context ~plugged:true [master_pbd];
	Xapi_pbd.check_unplugged_on_slaves_constraint ~__context ~sr;
	(* Master PBD and one slave PBD plugged => constraint not met *)
	set_state ~__context ~plugged:true [(List.hd slave_pbds)];
	assert_raises_api_error Api_errors.sr_attached_on_slave
		(fun () -> Xapi_pbd.check_unplugged_on_slaves_constraint ~__context ~sr);
	(* All PBDs plugged => constraint not met *)
	set_state ~__context ~plugged:true slave_pbds;
	assert_raises_api_error Api_errors.sr_attached_on_slave
		(fun () -> Xapi_pbd.check_unplugged_on_slaves_constraint ~__context ~sr);
	(* Only slave PBDs plugged => constraint not met (and shouldn't happen) *)
	set_state ~__context ~plugged:false [master_pbd];
	assert_raises_api_error Api_errors.sr_attached_on_slave
		(fun () -> Xapi_pbd.check_unplugged_on_slaves_constraint ~__context ~sr)

let test =
	"test_pbd" >:::
		[
			"test_pbd_plug_constraint" >:: test_pbd_plug_constraint;
			"test_pbd_unplug_constraint" >:: test_pbd_unplug_constraint;
		]
