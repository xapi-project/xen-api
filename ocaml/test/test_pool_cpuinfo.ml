(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

open Fun
open OUnit
open Test_highlevel
module PoolCpuinfo = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = (string * string) list
		type output_t = string * string

		let string_of_input_t = Test_printers.(list (pair string string))
		let string_of_output_t = Test_printers.(pair string string)
	end
	module State = XapiDb

	(* Create a host for each edition in the list. *)
	let load_input __context cpu_info =
		List.iter
			(fun (features_hvm, features_pv) ->
				let cpu_info = [
					"cpu_count", "1";
					"socket_count", "1";
					"vendor", "Abacus";
					"features_pv", features_pv;
					"features_hvm", features_hvm;
				] 
				and host = Test_common.make_host ~__context () in
				Db.Host.set_cpu_info ~__context ~self:host ~value:cpu_info)
			cpu_info;
		ignore (Test_common.make_pool ~__context
			~master:(List.hd (Db.Host.get_all ~__context)) ()); 
		Create_misc.create_pool_cpuinfo ~__context
		

	let extract_output __context _ =
		let pool = Helpers.get_pool ~__context in
		let cpu_info = Db.Pool.get_cpu_info ~__context ~self:pool in
		(List.assoc "features_hvm" cpu_info, List.assoc "features_pv" cpu_info)

	(* Tuples of ((features_hvm * features_pv) list, (expected features_hvm, expected features_pv) *)
	let tests = [
		(["0000000a", "0000000a"], ("0000000a", "0000000a"));
		([("0000000a", "0000000a");
		  ("0000000a", "0000000a")], ("0000000a", "0000000a"));
		([("0000000a", "00000002");
		  ("0000000f", "00000001")], ("0000000a", "00000000"));
		([("ffffffff-ffffffff", "ffffffff-ffffffff");
		  ("ffffffff-ffffffff", "ffffffff-ffffffff")], ("ffffffff-ffffffff", "ffffffff-ffffffff"));
		([("ffffffff", "ffffffff-ffffffff-ffffffff");
		  ("ffffffff-ffffffff", "ffffffff-ffffffff")], ("ffffffff-00000000", "ffffffff-ffffffff-00000000"));
		([("01230123-5a5a5a5a", "00000002");
		  ("ffff1111-a5a56666", "00004242")], ("01230101-00004242", "00000002"));
	]
end))

let test =
	"pool_cpuinfo" >:::
		[
			"test_pool_cpuinfo" >:: PoolCpuinfo.test;
		]
