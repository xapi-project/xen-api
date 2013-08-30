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
open Test_vgpu_common
open Xapi_vgpu_type

(* This test generates a lot of print --- set skip to false to enable *)
let skip = true
let k100_path = "/usr/share/nvidia/vgx/grid_k100.conf"

let print_vgpu_conf conf =
	Printf.printf "%04Lx %04Lx %04Lx %Ld\n"
		conf.pdev_id conf.vdev_id conf.vsubdev_id conf.framebufferlength

let test_of_conf_file path () =
	skip_if skip "Generates print...";
	if (Sys.file_exists path && not (Sys.is_directory path)) then begin
		let vgpu_conf = of_conf_file path in
		print_vgpu_conf vgpu_conf
	end

let print_nv_types () =
	skip_if skip "Generates print...";
	try
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
			"test_of_conf_file" >:: test_of_conf_file k100_path;
			"print_nv_types" >:: print_nv_types;
			"test_find_or_create" >:: test_find_or_create;
		]
