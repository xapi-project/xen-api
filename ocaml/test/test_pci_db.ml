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

(* This test generates a lot of print --- set skip to false to enable *)
let skip = true

let test_lookup () =
	skip_if skip "Skipping";
	let db = Pci_db.of_file "/usr/share/hwdata/pci.ids" in
	try
		let subdevices = Pci_db.get_subdevice_names_by_id db 0x10deL 0x11b0L 0x101bL in
		assert (List.length subdevices > 0);
		List.iter print_string subdevices
	with Not_found ->
		failwith "Lookup failed"

let print_pci_db () =
	skip_if skip "Generates lots of text...";
	try
		let db = Pci_db.of_file "/usr/share/hwdata/pci.ids" in
		Pci_db.print db
	with e ->
		print_string (Printf.sprintf "%s\n" (Printexc.to_string e));
		assert false (* fail *)

let data_file = Filename.concat "ocaml/test/data"
let print_string str = str

(* Test that we can merge a whole new vendor tree into a base database. *)
let test_merge_new_vendor () =
	let base_pci_db = Pci_db.of_file (data_file "base-pci.ids") in
	let new_pci_db = Pci_db.of_file (data_file "new-vendor-pci.ids") in
	Pci_db.merge base_pci_db new_pci_db;
	let test_vendor_id = 0x0007L in
	let device_id_with_subdevices = 0x044L in
	(* Test that the merged database has the new vendor. *)
	let new_vendor = Pci_db.get_vendor base_pci_db test_vendor_id in
	assert_equal
		~msg:"New vendor has an unexpected name"
		"NewVendor"
		new_vendor.Pci_db.v_name;
	(* Test that the merged database has the new vendor's devices. *)
	List.iter
		(fun (device_id, device_name) ->
			assert_equal
				~msg:"New vendor's device has an unexpected name"
				~printer:print_string
				device_name
				(Pci_db.get_device base_pci_db test_vendor_id device_id).Pci_db.d_name)
		[
			0x0041L, "NewVendor Device1";
			0x0042L, "NewVendor Device2";
			0x0043L, "NewVendor Device3";
			0x0044L, "NewVendor Device4";
		];
	(* Test that the merged database has imported the new subdevices. *)
	List.iter
		(fun ((subvendor_id, subdevice_id), subdevice_name) ->
			assert_equal
				~msg:"New vendor's subdevice has an unexpected name"
				~printer:print_string
				subdevice_name
				(Pci_db.get_subdevice
					base_pci_db
					test_vendor_id
					device_id_with_subdevices
					subvendor_id
					subdevice_id))
		[
			(0x0007L, 0x0001L), "NewSubVendor SubDevice1";
			(0x0007L, 0x0002L), "NewSubVendor SubDevice2";
			(0x0007L, 0x0003L), "NewSubVendor SubDevice3";
		]

(* Test that we can merge new devices into an existing vendor. *)
let test_merge_new_devices () =
	let base_pci_db = Pci_db.of_file (data_file "base-pci.ids") in
	let new_pci_db = Pci_db.of_file (data_file "new-devices-pci.ids") in
	Pci_db.merge base_pci_db new_pci_db;
	let test_vendor_id = 0x0003L in
	let device_id_with_subdevices = 0x0013L in
	(* Test that the merged database has the new devices. *)
	List.iter
		(fun (device_id, device_name) ->
			assert_equal
				~msg:"Vendor's new device has an unexpected name"
				~printer:print_string
				device_name
				(Pci_db.get_device base_pci_db test_vendor_id device_id).Pci_db.d_name)
		[
			0x0011L, "NewDevice1";
			0x0012L, "NewDevice2";
			0x0013L, "NewDevice3";
		];
	(* Test that the merged database has the new subdevices. *)
	List.iter
		(fun ((subvendor_id, subdevice_id), subdevice_name) ->
			assert_equal
				~msg:"New vendor's subdevice has an unexpected name"
				~printer:print_string
				subdevice_name
				(Pci_db.get_subdevice
					base_pci_db
					test_vendor_id
					device_id_with_subdevices
					subvendor_id
					subdevice_id))
		[
			(0x0003L, 0x01f1L), "NewSubDevice1";
			(0x0003L, 0x01f2L), "NewSubDevice2";
		]

(* Test that we can merge new subdevices into an existing device. *)
let test_merge_new_subdevices () =
	let base_pci_db = Pci_db.of_file (data_file "base-pci.ids") in
	let new_pci_db = Pci_db.of_file (data_file "new-subdevices-pci.ids") in
	Pci_db.merge base_pci_db new_pci_db;
	let test_vendor_id = 0x0002L in
	let device_id_with_subdevices = 0x0001L in
	(* Test that the merged database has the new subdevices. *)
	List.iter
		(fun ((subvendor_id, subdevice_id), subdevice_name) ->
			assert_equal
				~msg:"New vendor's subdevice has an unexpected name"
				~printer:print_string
				subdevice_name
				(Pci_db.get_subdevice
					base_pci_db
					test_vendor_id
					device_id_with_subdevices
					subvendor_id
					subdevice_id))
		[
			(0x0002L, 0x0be1L), "SubDeviceName1";
			(0x0002L, 0x0be2L), "SubDeviceName2";
			(0x0002L, 0x0be3L), "SubDeviceName3";
			(0x0002L, 0x0be4L), "SubDeviceName4";
		]

(* Test that the original database is respected when attempting to merge
 * a duplicate device. *)
let test_merge_duplicate_device () =
	let base_pci_db = Pci_db.of_file (data_file "base-pci.ids") in
	let new_pci_db = Pci_db.of_file (data_file "duplicate-device-pci.ids") in
	Pci_db.merge base_pci_db new_pci_db;
	let test_vendor_id = 0x0002L in
	let test_device_id = 0x0001L in
	(* Test that the duplicate device from the new DB has not overwritten the
	 * device in the base DB. *)
	assert_equal
		~msg:"Device has been overwritten"
		~printer:print_string
		"SimpleDeviceName-2-1"
		(Pci_db.get_device base_pci_db test_vendor_id test_device_id).Pci_db.d_name

(* Test that the original database is respected when attempting to merge
 * a duplicate subdevice. *)
let test_merge_duplicate_subdevice () =
	let base_pci_db = Pci_db.of_file (data_file "base-pci.ids") in
	let new_pci_db = Pci_db.of_file (data_file "duplicate-subdevice-pci.ids") in
	Pci_db.merge base_pci_db new_pci_db;
	let test_vendor_id = 0x0003L in
	let test_device_id = 0x0003L in
	let test_subvendor_id = 0x0003L in
	let test_subdevice_id = 0x0004L in
	(* Test that the duplicate device from the new DB has not overwritten the
	 * device in the base DB. *)
	assert_equal
		~msg:"Subdevice has been overwritten"
		~printer:print_string
		"SimpleSubDeviceName-3-3-3-4"
		(Pci_db.get_subdevice
			base_pci_db
			test_vendor_id
			test_device_id
			test_subvendor_id
			test_subdevice_id)

(* Test that we can merge a new class tree into a base database. *)
let test_merge_new_class () =
	let base_pci_db = Pci_db.of_file (data_file "base-pci.ids") in
	let new_pci_db = Pci_db.of_file (data_file "new-classes-pci.ids") in
	Pci_db.merge base_pci_db new_pci_db;
	let test_class_id = 0x7fL in
	(* Test that the merged database has the new class. *)
	assert_equal
		~msg:"New class has an unexpected name"
		~printer:print_string
		"NewClass"
		(Pci_db.get_class base_pci_db test_class_id).Pci_db.c_name;
	(* Test that the merged database has the new subclasses. *)
	List.iter
		(fun (subclass_id, subclass_name) ->
			assert_equal
				~msg:"New class's subclass has an unexpected name"
				~printer:print_string
				subclass_name
				(Pci_db.get_subclass base_pci_db test_class_id subclass_id))
		[
			(0x01L, "NewSubClass1");
			(0x02L, "NewSubClass2");
			(0x03L, "NewSubClass3");
		]

(* Test that we can merge new subclasses into an existing class. *)
let test_merge_new_subclasses () =
	let base_pci_db = Pci_db.of_file (data_file "base-pci.ids") in
	let new_pci_db = Pci_db.of_file (data_file "new-subclasses-pci.ids") in
	Pci_db.merge base_pci_db new_pci_db;
	let test_class_id = 0x00L in
	(* Test that the merged database has the new subclasses. *)
	List.iter
		(fun (subclass_id, subclass_name) ->
			assert_equal
				~msg:"New class's subclass has an unexpected name"
				~printer:print_string
				subclass_name
				(Pci_db.get_subclass base_pci_db test_class_id subclass_id))
		[
			(0x70L, "NewSubClass1");
			(0x71L, "NewSubClass2");
			(0x72L, "NewSubClass3");
		]

let suite_test_merge =
	"suite_test_merge" >:::
		[
			"test_merge_new_vendor" >:: test_merge_new_vendor;
			"test_merge_new_devices" >:: test_merge_new_devices;
			"test_merge_new_subdevices" >:: test_merge_new_subdevices;
			"test_merge_duplicate_device" >:: test_merge_duplicate_device;
			"test_merge_duplicate_subdevice" >:: test_merge_duplicate_subdevice;
			"test_merge_new_class" >:: test_merge_new_class;
			"test_merge_new_subclasses" >:: test_merge_new_subclasses;
		]

let test =
	"test_pci_db" >:::
		[
			"print_pci_db" >:: print_pci_db;
			"test_lookup" >:: test_lookup;
			suite_test_merge;
		]
