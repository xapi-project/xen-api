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

let run_create_metadata ~__context =
	let localhost_uuid = Helpers.get_localhost_uuid () in
	let host = make_host ~__context ~uuid:localhost_uuid () in
	let (_: API.ref_pool) = make_pool ~__context ~master:host () in
	let vms = Db.VM.get_by_name_label ~__context ~label:test_vm_name in
	let vm = List.nth vms 0 in
	Xapi_xenops.create_metadata ~__context ~upgrade:false ~self:vm

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
		let metadata = run_create_metadata ~__context in
		match metadata.Metadata.vm.Vm.ty with
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

let vgpu_manual_setup = Xapi_globs.vgpu_manual_setup_key, "true"
let vgpu_pci_id = Xapi_globs.vgpu_pci_key, "0000:0a:00.0"
let vgpu_config =
	Xapi_globs.vgpu_config_key, "/usr/share/nvidia/vgx/grid_k100.conf"

let vgpu_platform_data = [vgpu_manual_setup; vgpu_pci_id; vgpu_config]

module VideoMode = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = vm_config
		type output_t = Vm.video_card

		let string_of_input_t = string_of_vm_config
		let string_of_output_t = function
			| Vm.Cirrus -> "Cirrus"
			| Vm.Standard_VGA -> "Standard_VGA"
			| Vm.Vgpu -> "Vgpu"
			| Vm.IGD_passthrough Vm.GVT_d -> "IGD_passthrough"
	end

	module State = XapiDb

	let load_input = load_vm_config

	let extract_output __context _ =
		let metadata = run_create_metadata ~__context in
		match metadata.Metadata.vm.Vm.ty with
		| Vm.HVM {Vm.video = video_mode} -> video_mode
		| _ -> failwith "expected HVM metadata"

	let tests = [
		(* Default video mode should be Cirrus. *)
		{oc=[]; platform=[]}, Vm.Cirrus;
		(* Unrecognised video mode should default to Cirrus. *)
		{oc=[]; platform=["vga", "foo"]}, Vm.Cirrus;
		(* Video modes set in the platform map should be respected. *)
		{oc=[]; platform=["vga", "cirrus"]}, Vm.Cirrus;
		{oc=[]; platform=["vga", "std"]}, Vm.Standard_VGA;
		(* The IGD passthrough key should be respected. *)
		{oc=[]; platform=["igd_passthrough", "true"]}, Vm.(IGD_passthrough GVT_d);
		(* The IGD passthrough should override the "vga" key. *)
		{
			oc=[];
			platform=["igd_passthrough", "true"; "vga", "cirrus"]
		}, Vm.(IGD_passthrough GVT_d);
		{
			oc=[];
			platform=["igd_passthrough", "true"; "vga", "std"]
		}, Vm.(IGD_passthrough GVT_d);
		(* We should be able to enable vGPU via the manual setup mode. *)
		{oc=[]; platform=vgpu_platform_data}, Vm.Vgpu;
		(* vGPU mode should override whatever's set for the "vga" key. *)
		{oc=[]; platform=["vga", "cirrus"] @ vgpu_platform_data}, Vm.Vgpu;
		{oc=[]; platform=["vga", "std"] @ vgpu_platform_data}, Vm.Vgpu;
		(* If somehow only one of the vGPU keys is set, this shouldn't
		 * trigger vGPU mode. This should only ever happen if a user is
		 * experimenting with vgpu_manual_setup and has got things wrong. *)
		{oc=[]; platform=[vgpu_manual_setup; vgpu_pci_id]}, Vm.Cirrus;
		{
			oc=[];
			platform=["vga", "cirrus"; vgpu_manual_setup; vgpu_pci_id]
		}, Vm.Cirrus;
		{
			oc=[];
			platform=["vga", "std"; vgpu_manual_setup; vgpu_pci_id]
		}, Vm.Standard_VGA;
		{oc=[]; platform=[vgpu_manual_setup; vgpu_config]}, Vm.Cirrus;
		{
			oc=[];
			platform=["vga", "cirrus"; vgpu_manual_setup; vgpu_config]
		}, Vm.Cirrus;
		{
			oc=[];
			platform=["vga", "std"; vgpu_manual_setup; vgpu_config]
		}, Vm.Standard_VGA;
	]
end))

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
		let metadata = run_create_metadata ~__context in
		match metadata.Metadata.vm.Vm.ty with
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
			"test_videomode" >:: VideoMode.test;
			"test_videoram" >:: VideoRam.test;
		]
