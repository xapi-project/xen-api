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
open Test_vgpu_common
open Xapi_vgpu_type

let string_of_vgpu_conf conf =
	let open Nvidia in
	Printf.sprintf "%04x %s %04x %04x %Ld"
		conf.pdev_id
		(match conf.psubdev_id with
			| Some id -> Printf.sprintf "Some %04x" id
			| None -> "None")
		conf.vdev_id
		conf.vsubdev_id
		conf.framebufferlength

let print_vgpu_conf conf =
	Printf.printf "%s\n" (string_of_vgpu_conf conf)

module OfConfFile = Generic.Make(struct
	module Io = struct
		type input_t = string
		type output_t = Nvidia.vgpu_conf

		let string_of_input_t x = x
		let string_of_output_t = string_of_vgpu_conf
	end

	let transform = Nvidia.of_conf_file

	let tests = [
		"ocaml/test/data/test_vgpu_subdevid.conf",
		{
			Nvidia.pdev_id = 0x3333;
			psubdev_id = Some 0x4444;
			vdev_id = 0x1111;
			vsubdev_id = 0x2222;
			framebufferlength = 0x10000000L;
			num_heads = 2L;
			max_instance = 8L;
			max_x = 1920L;
			max_y = 1200L;
			file_path = "ocaml/test/data/test_vgpu_subdevid.conf";
		};
		"ocaml/test/data/test_vgpu_nosubdevid.conf",
		{
			Nvidia.pdev_id = 0x3333;
			psubdev_id = None;
			vdev_id = 0x1111;
			vsubdev_id = 0x2222;
			framebufferlength = 0x10000000L;
			num_heads = 2L;
			max_instance = 8L;
			max_x = 1920L;
			max_y = 1200L;
			file_path = "ocaml/test/data/test_vgpu_nosubdevid.conf";
		};
	]
end)

(* This test generates a lot of print --- set skip to false to enable *)
let skip = true

let print_nv_types () =
	skip_if skip "Generates print...";
	try
		let open Nvidia in
		if (Sys.file_exists nvidia_conf_dir
			&& Sys.is_directory nvidia_conf_dir) then
			begin
				let vgpu_confs = read_config_dir nvidia_conf_dir in
				List.iter print_vgpu_conf vgpu_confs
			end else
				Printf.printf "No NVIDIA conf files found in %s\n" nvidia_conf_dir
	with e ->
		print_string (Printf.sprintf "%s\n" (Printexc.to_string e));
		assert false (* fail *)

let test_find_or_create () =
	let __context = make_test_database () in
	let k100_ref = find_or_create ~__context k100 in
	(* Check the VGPU type created in the DB has the expected fields. *)
	assert_equal
		~msg:"k100 framebuffer_size is incorrect"
		k100.framebuffer_size
		(Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref);
	assert_equal
		~msg:"k100 max_heads is incorrect"
		k100.max_heads
		(Db.VGPU_type.get_max_heads ~__context ~self:k100_ref);
	assert_equal
		~msg:"k100 size is incorrect"
		k100.size
		(Db.VGPU_type.get_size ~__context ~self:k100_ref);
	(* Simulate an update of framebuffer_size, max_heads and size as if the
	 * config file had been updated. *)
	let new_k100 = {
		k100 with
		framebuffer_size = (Int64.mul k100.framebuffer_size 2L);
		max_heads = (Int64.mul k100.max_heads 2L);
		size = (Int64.mul k100.size 2L);
	} in
	(* We can ignore the result as it should be the same as the VGPU_type ref
	 * obtained earlier. *)
	let (_: API.ref_VGPU_type) = find_or_create ~__context new_k100 in
	(* Make sure the existing VGPU type object in the database
	 * has been updated. *)
	assert_equal
		~msg:"k100 framebuffer_size was not updated"
		new_k100.framebuffer_size
		(Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref);
	assert_equal
		~msg:"k100 max_heads was not updated"
		new_k100.max_heads
		(Db.VGPU_type.get_max_heads ~__context ~self:k100_ref);
	assert_equal
		~msg:"k100 size was not updated"
		new_k100.size
		(Db.VGPU_type.get_size ~__context ~self:k100_ref)

let test =
	"test_vgpu_type" >:::
		[
			"test_of_conf_file" >:: OfConfFile.test;
			"print_nv_types" >:: print_nv_types;
			"test_find_or_create" >:: test_find_or_create;
		]
