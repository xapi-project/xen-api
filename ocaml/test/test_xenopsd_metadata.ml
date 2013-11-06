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
open Test_highlevel
open Xenops_interface

let test_vm_name = "__test_vm"

type vm_config = {
	oc: (string * string) list;
	platform: (string * string) list;
}

let string_of_vm_config conf =
	Printf.sprintf "other_config = %s, platform = %s"
		(string_of_string_map conf.oc)
		(string_of_string_map conf.platform)

let load_vm_config __context conf =
	let (_: API.ref_VM) = make_vm ~__context
		~name_label:test_vm_name
		~hVM_boot_policy:"BIOS order"
		~other_config:conf.oc
		~platform:conf.platform
		()
	in ()

let run_builder_of_vm __context =
	let vms = Db.VM.get_by_name_label ~__context ~label:test_vm_name in
	let vm = List.nth vms 0 in
	let vm_record = Db.VM.get_record ~__context ~self:vm in
	Xapi_xenops.builder_of_vm ~__context ~vm:vm_record "0" false

(* Test the behaviour of the "hvm_serial" other_config/platform key. *)
module HVMSerial = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = vm_config
		type output_t = string option

		let string_of_input_t = string_of_vm_config
		let string_of_output_t = function
			| Some s -> Printf.sprintf "Some %s" s
			| None -> "None"
	end

	module State = XapiDb

	let load_input = load_vm_config

	let extract_output __context _ =
		match run_builder_of_vm __context with
		| Vm.HVM {Vm.serial = serial} -> serial
		| _ -> failwith "expected HVM metadata"

	let tests =
		[
			(* Should default to "pty" if nothing is set. *)
			(
				{oc=[]; platform=[]},
				Some "pty"
			);
			(* other_config value should override default if no platform value. *)
			(
				{oc=["hvm_serial", "none"]; platform=[]},
				Some "none"
			);
			(* Should be able to disable serial emulation via the platform key. *)
			(
				{oc=[]; platform=["hvm_serial", "none"]},
				Some "none"
			);
			(* platform value should override other_config value. *)
			(
				{oc=["hvm_serial", "none"]; platform=["hvm_serial", "pty"]},
				Some "pty"
			);
			(* platform value should override other_config value. *)
			(
				{oc=["hvm_serial", "pty"]; platform=["hvm_serial", "none"]},
				Some "none"
			);
			(* Windows debugger redirects the serial port to tcp - this should be
			 * configurable via the other_config key. *)
			(
				{oc=["hvm_serial", "tcp:1.2.3.4:7001"]; platform=[]},
				Some "tcp:1.2.3.4:7001"
			);
			(* Windows debugger should be configurable via the platform key too. *)
			(
				{oc=[]; platform=["hvm_serial", "tcp:1.2.3.4:7001"]},
				Some "tcp:1.2.3.4:7001"
			);
			(* Windows debugger setting via the platform key should override anything
			 * set in other_config. *)
			(
				{oc=["hvm_serial", "none"]; platform=["hvm_serial", "tcp:1.2.3.4:7001"]},
				Some "tcp:1.2.3.4:7001"
			);
		]
end))

let vgpu_platform_data = [
	"vgpu_pci_id", "0000:0a:00.0";
	"vgpu_config", "/usr/share/nvidia/vgx/grid_k100.conf";
]

module VideoRam = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = vm_config
		type output_t = int

		let string_of_input_t = string_of_vm_config
		let string_of_output_t = string_of_int
	end

	module State = XapiDb

	let load_input = load_vm_config

	let extract_output __context _ =
		match run_builder_of_vm __context with
		| Vm.HVM {Vm.video_mib = video_mib} -> video_mib
		| _ -> failwith "expected HVM metadata"

	let tests = [
		(* Video ram defaults to 4MiB. *)
		{oc=[]; platform=[]}, 4;
		(* Specifying a different amount of videoram works. *)
		{oc=[]; platform=["videoram", "8"]}, 8;
		(* Default videoram should be 16MiB for vGPU. *)
		{oc = []; platform=vgpu_platform_data}, 16;
		(* Insufficient videoram values should be overridden for vGPU. *)
		{oc = []; platform=vgpu_platform_data @ ["videoram", "8"]}, 16;
		(* videoram values larger than the default should be allowed for vGPU. *)
		{oc = []; platform=vgpu_platform_data @ ["videoram", "32"]}, 32;
		(* Other VGA options shouldn't affect the videoram setting. *)
		{oc = []; platform=["vga", "cirrus"]}, 4;
		{oc = []; platform=["vga", "cirrus"; "videoram", "8"]}, 8;
	]
end))

let test =
	"test_xenopsd_metadata" >:::
		[
			"test_hvm_serial" >:: HVMSerial.test;
			"test_videoram" >:: VideoRam.test;
		]
