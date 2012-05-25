(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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

open Int32
open Printf
open Stringext

exception InvalidFeatureString of string
exception MaskingNotSupported of string
exception ManufacturersDiffer

(* === Types and conversion === *)

type manufacturer = AMD | Intel | Unknown
and maskability = No | Base | Full
and features =
	{
		base_ecx: int32;
		base_edx: int32;
		ext_ecx: int32;
		ext_edx: int32;
	}
and cpu_info =
	{
		manufacturer: manufacturer;
		family: int32;
		model: int32;
		stepping: int32;
		features: features;
		physical_features: features;
		maskable: maskability;
	}

let features_to_string f =
	sprintf "%8.8lx-%8.8lx-%8.8lx-%8.8lx"
		f.base_ecx f.base_edx f.ext_ecx f.ext_edx
		
let string_to_features s =
	let digits = String.explode "0123456789abcdef" in
	let len = String.length s in
	let buf = Buffer.create len in
	for i = 0 to len - 1 do
		match Char.lowercase s.[i] with
		| ' ' | '-' -> ()
		| c when List.mem c digits -> Buffer.add_char buf c
		| _ -> raise (InvalidFeatureString "String contains illegal character")
	done;
	let s = Buffer.contents buf in
	if String.length s <> 32 then raise (InvalidFeatureString "String is not of the right length");
	{
		base_ecx = Int32.of_string ("0x" ^ (String.sub s 0 8));
		base_edx = Int32.of_string ("0x" ^ (String.sub s 8 8));
		ext_ecx = Int32.of_string ("0x" ^ (String.sub s 16 8));
		ext_edx = Int32.of_string ("0x" ^ (String.sub s 24 8))
	}
	
(* === Read CPUID information from the hardware === *)
	
(* CPUID wrapper: Arguments are leaf and count, *)
(* return values are %eax, %ebx, %ecx and %edx  *)
external cpuid: int32 -> int32 -> (int32 * int32 * int32 * int32) = "do_cpuid"

let read_manufacturer () = 
	match cpuid 0l 0l with
	| (_, 0x68747541l, 0x444D4163l, 0x69746E65l) -> AMD
	| (_, 0x756e6547l, 0x6c65746el, 0x49656e69l) -> Intel 
	| _ -> Unknown

(* Unfold the family/model/stepping numbers from leaf 1 *)
let read_family () = 
	match cpuid 1l 0l with (eax, _, _, _) ->
		let family = (shift_right (logand eax 0x00000f00l) 8) in
		match family with
		| 0xfl -> add family (shift_right (logand eax 0x0ff00000l) 20)
		| _ -> family

let read_model () = 
	match cpuid 1l 0l with (eax, _, _, _) -> 
		logor (shift_right (logand eax 0x000000f0l) 4) 
			(shift_right (logand eax 0x000f0000l) 12)

let read_stepping () = 
	match cpuid 1l 0l with (eax, _, _, _) -> 
		logand eax 0xfl		

(* Read the feature flags and extended feature flags *)
let read_features () = 
	let base = cpuid 1l 0l in
	let ext = cpuid 0x80000001l 0l in
	match (base, ext) with 
	| ((_, _, base_ecx, base_edx), (_, _, ext_ecx, ext_edx)) -> 
		{
			base_ecx = base_ecx;
			base_edx = base_edx;
			ext_ecx = ext_ecx;
			ext_edx = ext_edx
		} 
		
(* Does this Intel CPU support "FlexMigration"? 
 * It's not sensibly documented, so check by model *)
let has_flexmigration family model stepping =
	let fully_maskable_models =
		[0x17l; 0x1dl; 0x1el; 0x1fl; 0x25l; 0x2al; 0x2cl; 0x2cl; 0x2dl; 0x2el; 0x2fl; 0x3al] in
	if family <> 0x6l then
		No
	else if model = 0x1dl || (model = 0x17l && stepping >= 4l) then
		Base
	else if (model = 0x1al && stepping > 2l) || List.mem model fully_maskable_models then
		Full
	else
		No

(* Does this AMD CPU have Extended Migration Technology? 
 * Known good on Barcelona and better; did exist on some older CPUs 
 * but not really documented which ones *)
let has_emt family = 
	if family >= 0x10l then
		Full
	else
		No
	
let is_maskable manufacturer family model stepping =
	match manufacturer with 
	| Unknown -> No
	| Intel -> has_flexmigration family model stepping
	| AMD -> has_emt family

let get_features_from_xen () =
	let features = 
	  try Xenctrl.with_intf (fun xc -> Xenctrlext.get_boot_cpufeatures xc) 
	  with _ -> 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l 
	in
	match features with
	| base_ecx, base_edx, ext_ecx, ext_edx,
		masked_base_ecx, masked_base_edx, masked_ext_ecx, masked_ext_edx ->	
		{
			base_ecx = masked_base_ecx;
			base_edx = masked_base_edx;
			ext_ecx = masked_ext_ecx;
			ext_edx = masked_ext_edx
		},
		{
			base_ecx = base_ecx;
			base_edx = base_edx;
			ext_ecx = ext_ecx;
			ext_edx = ext_edx
		}
		
let get_current_mask () =
	let masks = Xen_cmdline.list_cpuid_masks () in
	let get_mask m =
		if List.mem_assoc m masks = false then
			0xffffffffl
		else
			Int32.of_string (List.assoc m masks)
	in
	{
		base_ecx = get_mask "cpuid_mask_ecx";
		base_edx = get_mask "cpuid_mask_edx";
		ext_ecx = get_mask "cpuid_mask_ext_ecx";
		ext_edx = get_mask "cpuid_mask_ext_edx"
	}
	
let read_cpu_info () =
	let manufacturer = read_manufacturer () in
	let family = read_family () in
	let model = read_model () in
	let features, phy_features = get_features_from_xen () in
	let stepping = read_stepping () in
	{
		manufacturer = manufacturer;
		family = family;
		model = model;
		stepping = stepping;
		features = features;
		physical_features = phy_features;
		maskable = is_maskable manufacturer family model stepping;
	}
	
(* === Masking checks === *)

let mask_features features mask =
	{
		base_ecx = logand features.base_ecx mask.base_ecx;
		base_edx = logand features.base_edx mask.base_edx;
		ext_ecx = logand features.ext_ecx mask.ext_ecx;
		ext_edx = logand features.ext_edx mask.ext_edx;
	}

let assert_maskability cpu manufacturer features = 
	(* Manufacturers need to be the same *)
	if manufacturer != cpu.manufacturer then 
		raise ManufacturersDiffer;
	(* Check whether the features can be obtained by masking the physical features *)
	let base = (logand cpu.physical_features.base_ecx features.base_ecx) = features.base_ecx 
		&& (logand cpu.physical_features.base_edx features.base_edx) = features.base_edx in
	match cpu.maskable with
	| No ->
		begin match cpu.manufacturer with 
		| Unknown -> raise (MaskingNotSupported "Unknown CPU manufacturer")
		| Intel -> raise (MaskingNotSupported "CPU does not have FlexMigration")
		| AMD -> raise (MaskingNotSupported "CPU does not have Extended Migration Technology")
		end
	| Base ->
		if not (base && cpu.physical_features.ext_ecx = features.ext_ecx
			&& cpu.physical_features.ext_edx = features.ext_edx) then
			raise (InvalidFeatureString "CPU features cannot be masked to obtain \
				given features (only base features can be masked)")
	| Full ->
		if not (base && (logand cpu.physical_features.ext_ecx features.ext_ecx) = features.ext_ecx 
			&& (logand cpu.physical_features.ext_edx features.ext_edx) = features.ext_edx) then
			raise (InvalidFeatureString "CPU features cannot be masked to obtain given features")

let xen_masking_string cpu features = 
	let rec stringify reglist = 
		match reglist with 
		| (reg, host, pool) :: rest ->
			if (host = pool) then stringify rest
			else ("cpuid_mask_" ^ reg, sprintf "0x%8.8lx" pool) :: stringify rest
		| [] -> [] 
	in
	stringify [("ecx", cpu.physical_features.base_ecx, features.base_ecx);
		("edx", cpu.physical_features.base_edx, features.base_edx);
		("ext_ecx", cpu.physical_features.ext_ecx, features.ext_ecx); 
		("ext_edx", cpu.physical_features.ext_edx, features.ext_edx)]

