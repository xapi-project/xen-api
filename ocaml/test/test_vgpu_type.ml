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

module NvidiaTest = struct
	let string_of_vgpu_conf conf =
		let open Identifier in
		let open Nvidia in
		Printf.sprintf "%04x %s %04x %04x %Ld"
			conf.identifier.pdev_id
			(match conf.identifier.psubdev_id with
				| Some id -> Printf.sprintf "Some %04x" id
				| None -> "None")
			conf.identifier.vdev_id
			conf.identifier.vsubdev_id
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
			Nvidia.({
				identifier = Identifier.({
					pdev_id = 0x3333;
					psubdev_id = Some 0x4444;
					vdev_id = 0x1111;
					vsubdev_id = 0x2222;
				});
				framebufferlength = 0x10000000L;
				num_heads = 2L;
				max_instance = 8L;
				max_x = 1920L;
				max_y = 1200L;
				file_path = "ocaml/test/data/test_vgpu_subdevid.conf";
			});
			"ocaml/test/data/test_vgpu_nosubdevid.conf",
			Nvidia.({
				identifier = Identifier.({
					pdev_id = 0x3333;
					psubdev_id = None;
					vdev_id = 0x1111;
					vsubdev_id = 0x2222;
				});
				framebufferlength = 0x10000000L;
				num_heads = 2L;
				max_instance = 8L;
				max_x = 1920L;
				max_y = 1200L;
				file_path = "ocaml/test/data/test_vgpu_nosubdevid.conf";
			});
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
end

module IntelTest = struct
	let string_of_vgpu_conf conf =
		let open Identifier in
		let open Intel in
		Printf.sprintf "%04x %Ld %Ld %Ld %s %b %s"
			conf.identifier.pdev_id
			conf.identifier.low_gm_sz
			conf.identifier.high_gm_sz
			conf.identifier.fence_sz
			(string_of_string_opt conf.identifier.monitor_config_file)
			conf.experimental
			conf.model_name

	module ReadWhitelistLine = Generic.Make(struct
		module Io = struct
			type input_t = string
			type output_t = Intel.vgpu_conf option

			let string_of_input_t x = x
			let string_of_output_t = string_of_opt string_of_vgpu_conf
		end

		let transform line = Intel.read_whitelist_line ~line

		let tests = [
			(* Test some failure cases. *)
			"", None;
			"nonsense123", None;
			(* Test some success cases. *)
			"1234 experimental=0 name='myvgpu' low_gm_sz=128 high_gm_sz=384 fence_sz=4 monitor_config_file=/my/file",
			Some {
				Intel.identifier = Identifier.({
					pdev_id = 0x1234;
					low_gm_sz = 128L;
					high_gm_sz = 384L;
					fence_sz = 4L;
					monitor_config_file = Some "/my/file";
				});
				experimental = false;
				model_name = "myvgpu";
			};
			"1234 experimental=1 name='myvgpu' low_gm_sz=128 high_gm_sz=384 fence_sz=4 monitor_config_file=/my/file",
			Some {
				Intel.identifier = Identifier.({
					pdev_id = 0x1234;
					low_gm_sz = 128L;
					high_gm_sz = 384L;
					fence_sz = 4L;
					monitor_config_file = Some "/my/file";
				});
				experimental = true;
				model_name = "myvgpu";
			};
		]
	end)
end

let test_find_or_create () =
	let __context = make_test_database () in
	let k100_ref_1 = find_or_create ~__context k100 in
	(* Check the VGPU type created in the DB has the expected fields. *)
	assert_equal
		~msg:"k100 framebuffer_size is incorrect"
		k100.framebuffer_size
		(Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref_1);
	assert_equal
		~msg:"k100 max_heads is incorrect"
		k100.max_heads
		(Db.VGPU_type.get_max_heads ~__context ~self:k100_ref_1);
	assert_equal
		~msg:"k100 size is incorrect"
		k100.size
		(Db.VGPU_type.get_size ~__context ~self:k100_ref_1);
	assert_equal
		~msg:"k100 experimental flag is incorrect"
		k100.experimental
		(Db.VGPU_type.get_experimental ~__context ~self:k100_ref_1);
	(* Simulate an update of framebuffer_size, max_heads, size and the
	 * experimental flag, as if the config file had been updated. *)
	let new_k100 = {
		k100 with
		framebuffer_size = (Int64.mul k100.framebuffer_size 2L);
		max_heads = (Int64.mul k100.max_heads 2L);
		size = (Int64.mul k100.size 2L);
		experimental = not k100.experimental;
	} in
	(* We can ignore the result as it should be the same as the VGPU_type ref
	 * obtained earlier. *)
	let k100_ref_2 = find_or_create ~__context new_k100 in
	(* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
	 * been created. *)
	assert_equal
		~msg:"New k100 type was created erroneously"
		k100_ref_1 k100_ref_2;
	(* Make sure the existing VGPU type object in the database
	 * has been updated. *)
	assert_equal
		~msg:"k100 framebuffer_size was not updated"
		new_k100.framebuffer_size
		(Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref_1);
	assert_equal
		~msg:"k100 max_heads was not updated"
		new_k100.max_heads
		(Db.VGPU_type.get_max_heads ~__context ~self:k100_ref_1);
	assert_equal
		~msg:"k100 size was not updated"
		new_k100.size
		(Db.VGPU_type.get_size ~__context ~self:k100_ref_1);
	assert_equal
		~msg:"k100 was not marked experimental"
		new_k100.experimental
		(Db.VGPU_type.get_experimental ~__context ~self:k100_ref_1)

let test_identifier_lookup () =
	let test_vendor_name = "test_vendor_name" in
	let test_model_name = "test_model_name" in
	let __context = make_test_database () in
	let k100_ref_1 = find_or_create ~__context k100 in
	let k100_ref_2 = find_or_create ~__context
		{k100 with vendor_name = test_vendor_name; model_name = test_model_name} in
	(* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
	 * been created. *)
	assert_equal
		~msg:"New k100 type was created erroneously"
		k100_ref_1 k100_ref_2;
	(* Make sure the VGPU_type's vendor and model names have been updated. *)
	assert_equal
		~msg:"k100 vendor_name was not updated"
		test_vendor_name
		(Db.VGPU_type.get_vendor_name ~__context ~self:k100_ref_1);
	assert_equal
		~msg:"k100 model_name was not updated"
		test_model_name
		(Db.VGPU_type.get_model_name ~__context ~self:k100_ref_1)

let test_vendor_model_lookup () =
	let __context = make_test_database () in
	let k100_ref_1 = find_or_create ~__context k100 in
	(* Set the identifier to the empty string, as if we have upgraded from an old
	 * version that did not have the identifier field. *)
	Db.VGPU_type.set_identifier ~__context ~self:k100_ref_1 ~value:"";
	let k100_ref_2 = find_or_create ~__context k100 in
	(* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
	 * been created. *)
	assert_equal
		~msg:"New k100 type was created erroneously"
		k100_ref_1 k100_ref_2;
	(* Make sure the identifier field has been updated. *)
	assert_equal
		~msg:"k100 identifier was not updated."
		(Identifier.to_string k100.identifier)
		(Db.VGPU_type.get_identifier ~__context ~self:k100_ref_1)

let test =
	"test_vgpu_type" >:::
		[
			"test_of_conf_file" >:: NvidiaTest.OfConfFile.test;
			"print_nv_types" >:: NvidiaTest.print_nv_types;
			"read_whitelist_line" >:: IntelTest.ReadWhitelistLine.test;
			"test_find_or_create" >:: test_find_or_create;
			"test_identifier_lookup" >:: test_identifier_lookup;
			"test_vendor_model_lookup" >:: test_vendor_model_lookup;
		]
