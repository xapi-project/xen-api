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
		type input_t = (string * string) list list
		type output_t = (string * string) list

		let string_of_input_t = Test_printers.(list (assoc_list string string))
		let string_of_output_t = Test_printers.(assoc_list string string)
	end
	module State = XapiDb

	(* Create a host for each edition in the list. *)
	let load_input __context inputs =
		List.iter
			(fun cpu_info ->
				let host = Test_common.make_host ~__context () in
				Db.Host.set_cpu_info ~__context ~self:host ~value:cpu_info)
			inputs;
		ignore (Test_common.make_pool ~__context
			~master:(List.hd (Db.Host.get_all ~__context)) ()); 
		Create_misc.create_pool_cpuinfo ~__context
		

	let extract_output __context _ =
		let pool = Helpers.get_pool ~__context in
		List.sort compare (Db.Pool.get_cpu_info ~__context ~self:pool)

	let tests = [
		([["cpu_count", "1"; "features_hvm", "0000000a"; "features_pv", "0000000a";
		   "socket_count", "1"; "vendor", "Abacus"]],
		["cpu_count", "1"; "features_hvm", "0000000a"; "features_pv", "0000000a";
		  "socket_count", "1"; "vendor", "Abacus"]);

		([["cpu_count", "2"; "features_hvm", "0000000a"; "features_pv", "0000000a";
		   "socket_count", "4"; "vendor", "Abacus"];
		  ["cpu_count", "1"; "features_hvm", "0000000a"; "features_pv", "0000000a";
		   "socket_count", "1"; "vendor", "Abacus"]],
		 ["cpu_count", "3"; "features_hvm", "0000000a"; "features_pv", "0000000a";
		  "socket_count", "5"; "vendor", "Abacus"]);

		([["cpu_count", "8"; "features_hvm", "0000000a"; "features_pv", "00000002";
		   "socket_count", "2"; "vendor", "Abacus"];
		  ["cpu_count", "4"; "features_hvm", "0000000f"; "features_pv", "00000001"; 
		   "socket_count", "1"; "vendor", "Abacus"]],
		 ["cpu_count", "12"; "features_hvm", "0000000a"; "features_pv", "00000000";
		  "socket_count", "3"; "vendor", "Abacus"]);

		([["cpu_count", "24"; "features_hvm", "ffffffff-ffffffff"; "features_pv", "ffffffff-ffffffff";
		   "socket_count", "1"; "vendor", "Abacus"];
		  ["cpu_count", "24"; "features_hvm", "ffffffff-ffffffff"; "features_pv", "ffffffff-ffffffff";
		   "socket_count", "24"; "vendor", "Abacus"]],
		 ["cpu_count", "48"; "features_hvm", "ffffffff-ffffffff"; "features_pv", "ffffffff-ffffffff";
		  "socket_count", "25"; "vendor", "Abacus"]);

		([["cpu_count", "1"; "features_hvm", "ffffffff"; "features_pv", "ffffffff-ffffffff-ffffffff";
		   "socket_count", "1"; "vendor", "Abacus"];
		  ["cpu_count", "1"; "features_hvm", "ffffffff-ffffffff"; "features_pv", "ffffffff-ffffffff";
		   "socket_count", "1"; "vendor", "Abacus"]],
		 ["cpu_count", "2"; "features_hvm", "ffffffff-00000000"; "features_pv", "ffffffff-ffffffff-00000000";
		  "socket_count", "2"; "vendor", "Abacus"]);

		([["cpu_count", "10"; "features_hvm", "01230123-5a5a5a5a"; "features_pv", "00000002";
		   "socket_count", "1"; "vendor", "Abacus"];
		  ["cpu_count", "1"; "features_hvm", "ffff1111-a5a56666"; "features_pv", "00004242";
		   "socket_count", "10"; "vendor", "Abacus"]],
		 ["cpu_count", "11"; "features_hvm", "01230101-00004242"; "features_pv", "00000002";
		  "socket_count", "11"; "vendor", "Abacus"]);
	]
end))

let test =
	"pool_cpuinfo" >:::
		[
			"test_pool_cpuinfo" >::: PoolCpuinfo.tests;
		]
