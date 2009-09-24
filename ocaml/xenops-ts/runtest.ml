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
open Printf
open Stringext

let test_failure n cn s =
	printf "test %3d[%2d] (%20s):	FAILURE\n%!" n cn s

let test_succeed n cn s =
	printf "test %3d[%2d] (%20s):	SUCCEED\n%!" n cn s

let test_skipped n cn s =
	printf "test %3d[%2d] (%20s):	SKIPPED\n%!" n cn s

module D=Debug.Debugger(struct let name="testsuite" end)
open D

let generate_config_files kernel initrd outputfile no_mem_check =
	let simple_cfg = {
		Test.name = "simplePV";
		Test.kernel = kernel;
		Test.initrd = initrd;
		Test.output = outputfile;
		Test.no_mem_check = no_mem_check;
		Test.cmdline = "init 1";
		Test.disks = [];
		Test.vifs = [];
		Test.mem = 64;
		Test.vcpus = 1;
		Test.hvm = false;
	} in

	let lotscpus_cfg = { simple_cfg with Test.name = "manycpusPV"; Test.vcpus = 8; }
	and moremem_cfg = { simple_cfg with Test.name = "4cpusmemPV"; Test.vcpus = 4; Test.mem = 128; }
	and vif_cfg = { simple_cfg with Test.name = "vifPV"; Test.vifs = [ "" ] }
	in

	[
		simple_cfg;
		lotscpus_cfg;
		moremem_cfg;
		vif_cfg;
	]

let assert_xenvm_present () =
	let found = ref false in
	begin try
		let paths = String.split ':' (Unix.getenv "PATH") in
		List.iter (fun path ->
			try
				Unix.access (path ^ "/xenvm") [ Unix.X_OK ];
				found := true
			with _ -> ()) paths
	with Not_found -> ()
	end;
	if not !found then (
		eprintf "cannot run domains tests without xenvm in the PATH\n";
		failwith "xenvm not found"
	)

let _ =
	let testnb = ref 0 in
	let catch_exn = ref true in
	let no_mem_check = ref false in

	if Array.length Sys.argv < 4 then (
		eprintf "usage: %s <kernel> <initrd> <outputfile>\n" Sys.argv.(0);
		exit 1
	);
	if Array.length Sys.argv = 5 then (
		if Sys.argv.(4) = "nomemcheck" then
			no_mem_check := true
	);

	(* set logger to some file *)
	let all_cfgs = generate_config_files Sys.argv.(1) Sys.argv.(2) Sys.argv.(3) !no_mem_check in
	let logger_ts = sprintf "file:/tmp/xenops.ts.%d.exn" (Unix.getpid ()) in

	Logs.set "testsuite" Log.Error [ logger_ts ];

	let execute_test i cn name f =
		begin try
			f ();
			test_succeed i cn name
		with
		| Failure "test skipped" ->
			test_skipped i cn name
		| exn ->
			error "test %d [%2d] (%s): %s\n" i cn name (Printexc.to_string exn);
			test_failure i cn name;
			if not !catch_exn then
				raise exn
		end
		in
	(* run every test that doens't need a VM *)
	List.iter (fun test ->
		let name, f = test in
		incr testnb;
		execute_test !testnb 0 name f
	) Test.all;

	(* we don't domains tests if xenvm is not here *)
	assert_xenvm_present ();

	(** run every test on each configs available on the all_cfgs array *)
	List.iter (fun test ->
		let name, f = test in
		incr testnb;
		let cn = ref 1 in
		List.iter (fun cfg ->
			execute_test !testnb !cn name (f cfg);
			incr cn
		) all_cfgs
	) Test.all_dom
