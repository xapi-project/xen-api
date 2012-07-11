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
open Pervasiveext
open Xenstore

module D=Debug.Debugger(struct let name="testsuite" end)
open D

exception Domain_left_behind

type domain_config = {
	domain_kernel: string;
	domain_hda: string;
	domain_mem: int64;
	domain_vcpus: int;
	domain_hvm: bool;
}

let simple_cfg = {
	domain_kernel = "/root/xenops.ts/kernel";
	domain_hda = "xenops.ts/image";
	domain_mem = 65536L;
	domain_vcpus = 1;
	domain_hvm = false;
}

let configs = [
	"pv simple", simple_cfg;
]

(************** helpers **************)
let test_failure n s =
	printf "test %3d (%20s):	FAILURE\n%!" n s

let test_succeed n s =
	printf "test %3d (%20s):	SUCCEED\n%!" n s

(************* xen helpers ************)
let list_domid xc =
	List.map (fun x -> x.Xenctrl.domid) (Xenctrl.domain_getinfolist xc 0)

let assert_nodomain xc =
	if List.length (list_domid xc) > 1 then
		raise Domain_left_behind

let check_dead xc domid =
	try
		let inf = Xenctrl.domain_getinfo xc domid in
		inf.Xenctrl.dying || inf.Xenctrl.shutdown
	with
		_ -> true

let with_domain_notbuilt xc xs f =
	let domid = Domain.make ~xc ~xs ~hvm:false ~uuid:(Uuid.insecure ()) in
	finally (fun () -> f domid)
		(fun () -> Domain.destroy ~xc ~xs domid)

let with_domain_nodisk cfg xc xs f =
	with_domain_notbuilt xc xs (fun domid ->
		let kernel = cfg.domain_kernel in
		let mem_kib = cfg.domain_mem in
		let cmdline = "root=/dev/xvda1 ro" in
		let ramdisk = None in
		let vcpus = cfg.domain_vcpus in
		info "building linux";
		Domain.build_linux ~xc ~xs ~mem_kib ~kernel ~cmdline
		                   ~ramdisk ~vcpus domid;
		info "linux built";
		f domid)

let with_domainrestore_nodisk from cfg xc xs f =
	with_domain_notbuilt xc xs (fun domid ->
		let fd = Unix.openfile from [ Unix.O_RDONLY ] 0o640 in
		finally (fun () ->
			let mem_kib = cfg.domain_mem in
			let vcpus = cfg.domain_vcpus in
			info "restoring linux";
			Domain.pv_restore ~xc ~xs ~mem_kib ~vcpus domid fd;
			info "linux restored";
		) (fun () -> Unix.close fd);
		f domid
	)

let build_domainpv cfg xc xs build_function f =
	let losetup path =
		let cmd = [| "losetup"; "/dev/loop6"; path |] in
		match Unixext.spawnvp cmd.(0) cmd with
		| Unix.WEXITED 0 -> "/dev/loop6"
		| _              -> failwith "losetup failed"
		in
	let losetup_free dev =
		let cmd = [| "losetup"; "-d"; dev |] in
		let tries = ref 10 and freed = ref false in
		while not !freed && !tries > 0
		do
			match Unixext.spawnvp cmd.(0) cmd with
			| Unix.WEXITED 0 -> freed := true
			| _              ->
				decr tries;
				Unix.sleep 1;
		done;
		if not !freed then
			failwith "losetup freeing failed"
		in
	let dev = losetup cfg.domain_hda in
	finally (fun () -> build_function cfg xc xs (fun domid ->
		Device.Vbd.add ~xs ~hvm:false ~mode:Device.Vbd.ReadOnly
			       ~virtpath:"xvda"
			       ~phystype:(Device.Vbd.physty_of_string "phy")
			       ~physpath:dev
			       ~dev_type:(Device.Vbd.devty_of_string "disk")
			       domid;
		f domid
		)
	) (fun () -> losetup_free dev)

let with_domain cfg xc xs f =
	build_domainpv cfg xc xs with_domain_nodisk f

let with_domain_restore cfg xc xs file f =
	build_domainpv cfg xc xs (with_domainrestore_nodisk file) f

(************* all tests *************)
let test_xc_open () =
	let xc = Xenctrl.interface_open () in
	Xenctrl.interface_close xc

let test_xs_open () =
	let xs = Xs.daemon_open () in
	Xs.close xs

let test_xs_open_mmap () =
	let xs = Xs.domain_open () in
	Xs.close xs

let __xs_cmd_on xs =
	let dirs = xs.Xs.directory "/" in
	List.iter (fun dir -> ignore (xs.Xs.read ("/" ^ dir))) dirs;
	xs.Xs.mkdir "/testsuite";
	xs.Xs.setperms "/testsuite" (0, Xsraw.PERM_RDWR, []);
	xs.Xs.rm "/testsuite";
	if xs.Xs.getdomainpath 0 <> "/local/domain/0" then
		failwith "getdomainpath string expectation";
	Xs.close xs

let test_xs_cmd xs =
	__xs_cmd_on (Xs.daemon_open ())

let test_xs_cmd2 xs =
	__xs_cmd_on (Xs.domain_open ())

let test_xal () =
	let xal = Xal.init () in
	Xal.close xal

let test_xal2 () =
	let xal = Xal.init () in
	let l = Xal.domains_running xal in
	if List.length l < 1 then
		failwith "xal error: no running domains";
	if Xal.domain_is_dead xal 0 then
		failwith "xal error: Domain 0 appears dead";
	let t = Unix.time () in
	Xal.wait xal 1.;
	if t +. 1. > Unix.time () then
		failwith "xal error: Waited not enough";
	Xal.close xal

let test_domain_creation xc =
	assert_nodomain xc;
	let uuid = Uuid.to_string (Uuid.insecure ()) in
	let domid = Xenctrl.domain_create xc 0l false uuid in
	Xenctrl.domain_destroy xc domid;
	assert_nodomain xc

let test_xenops_creation cfg xc xs =
	assert_nodomain xc;
	let domid = Domain.make ~xc ~xs ~hvm:cfg.domain_hvm ~uuid:(Uuid.insecure ()) in
	Domain.destroy ~xc ~xs domid;
	assert_nodomain xc

let test_xenops_build cfg xc xs =
	assert_nodomain xc;
	with_domain_nodisk cfg xc xs (fun domid -> ());
	assert_nodomain xc

let test_xenops_start cfg xc xs =
	assert_nodomain xc;
	with_domain cfg xc xs (fun domid ->
		Domain.unpause ~xc domid;
		Unix.sleep 10;
		if check_dead xc domid then
			failwith "domain died";
	);
	assert_nodomain xc

let test_xenops_suspend cfg xal xc xs =
	assert_nodomain xc;
	let file = "/tmp/suspend_test" in
	with_domain cfg xc xs (fun domid ->
		Domain.unpause ~xc domid;
		Xal.wait xal 10.;
		if check_dead xc domid then
			failwith "domain died before suspend";
		let suspend_callback () =
			let ack = Domain.shutdown_ack ~timeout:10. ~xc ~xs
			                       domid Domain.Suspend in
			if not ack then
				failwith "Domain didn't acknowlegde suspend"
			in
		info "opening file for suspend";
		let fd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT;
		                              Unix.O_TRUNC; Unix.O_APPEND ] 0o640 in
		info "suspending";
		finally (fun () ->
			Domain.suspend ~xc ~xs ~hvm:cfg.domain_hvm
			               domid fd [] suspend_callback
		) (fun () -> Unix.close fd);
		info "suspended";
	);
	with_domain_restore cfg xc xs file (fun domid ->
		Domain.unpause ~xc domid;
		Xal.wait xal 10.;
		if check_dead xc domid then
			failwith "domain died after restore";
		begin try Unix.unlink file with _ -> () end;
	);
	assert_nodomain xc

let test_xenops_chkpoint cfg xal xc xs =
	assert_nodomain xc;
	let file = "/tmp/chkpoint_test" in
	with_domain cfg xc xs (fun domid ->
		Domain.unpause ~xc domid;
		Xal.wait xal 10.;
		if check_dead xc domid then
			failwith "domain died before suspend";
		let suspend_callback () =
			let ack = Domain.shutdown_ack ~timeout:10. ~xc ~xs
			                       domid Domain.Suspend in
			if not ack then
				failwith "Domain didn't acknowlegde suspend"
			in
		info "opening file for chkpointing";
		let fd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT;
		                              Unix.O_TRUNC; Unix.O_APPEND ] 0o640 in
		info "suspending";
		finally (fun () ->
			Domain.suspend ~xc ~xs ~hvm:cfg.domain_hvm
			               domid fd [] suspend_callback
		) (fun () -> Unix.close fd);
		info "resuming";
		Domain.resume ~xc ~xs ~cooperative:true domid;
		Xal.wait xal 0.5
	);
	assert_nodomain xc

type ty =
	| NO   of (unit -> unit)
	| XC   of (Xenctrl.handle -> unit)
	| XCS  of (domain_config -> Xenctrl.handle -> Xs.xsh -> unit)
	| XCSA of (domain_config -> Xal.ctx -> Xenctrl.handle -> Xs.xsh -> unit)

let all_tests = [
	"[xc] opening", NO test_xc_open;
	"[xs] opening", NO test_xs_open;
	"[xs] opening2", NO test_xs_open;
	"[xs] commands", NO test_xs_cmd;
	"[xs] commands2", NO test_xs_cmd2;
	"[xal] opening", NO test_xal;
	"[xal] commands", NO test_xal2;
	"[xc] domain creation", XC test_domain_creation;
	"[xenops] domain creation", XCS test_xenops_creation;
	"[xenops] domain build", XCS test_xenops_build;
	"[xenops] domain starting", XCS test_xenops_start;
	"[xenops] domain suspend", XCSA test_xenops_suspend;
	"[xenops] domain chkpoint", XCSA test_xenops_chkpoint;
]

(************** function **************)
let _ =
	let testnb = ref 0 in
	let catch_exn = ref true in

	Arg.parse [ "--exn", Arg.Clear catch_exn, "don't catch exceptions" ]
	          (fun _ -> ()) "xenops test suite";

	(* set logger to some file *)
	let logger = sprintf "file:/tmp/xenops.ts.%d.debug" (Unix.getpid ()) in
	let logger_ts = sprintf "file:/tmp/xenops.ts.%d.exn" (Unix.getpid ()) in

	Logs.set_default Log.Debug [ logger ];
	Logs.set_default Log.Info [ logger ];
	Logs.set_default Log.Warn [ logger ];
	Logs.set_default Log.Error [ logger ];

	Logs.set "testsuite" Log.Error [ logger_ts ];

	(* iterate over tests *)
	let execute_test i test =
		let name, fct_test = test in
		let allcfg f = List.iter (fun (n, cfg) -> f cfg) configs in
		begin try
			begin match fct_test with
			| NO f -> f ()
			| XC f ->
				let xc = Xenctrl.interface_open () in
				finally (fun () -> f xc)
					(fun () -> Xenctrl.interface_close xc)
			| XCS f ->
				let xc = Xenctrl.interface_open () in
				finally (fun () ->
					let xs = Xs.daemon_open () in
					finally (fun () ->
						allcfg (fun cfg -> f cfg xc xs);
					) (fun () -> Xs.close xs)
				) (fun () -> Xenctrl.interface_close xc)
			| XCSA f ->
				let ctx = Xal.init () in
				finally (fun () ->
					allcfg (fun cfg ->
						f cfg ctx (Xal.xc_of_ctx ctx)
						          (Xal.xs_of_ctx ctx))
				) (fun () -> Xal.close ctx)
			end;
			test_succeed !testnb name
		with exn ->
			error "test %d [%s]: %s\n" !testnb name (Printexc.to_string exn);
			test_failure !testnb name;
			if not !catch_exn then
				raise exn
		end
		in
	List.iter (fun test ->
		incr testnb;
		execute_test !testnb test) all_tests
