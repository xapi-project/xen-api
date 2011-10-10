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
(***************************************************************************
 * Testing code
 **************************************************************************)
open Printf
open Stringext
open Common
open Xenbus

let tnb data =
	try int_of_string (List.hd (String.split '\000' data))
	with _ -> 0

let do_test xb =
	let i = ref 0 in
	let disp op pkt =
		let tid, rid, ty, data = pkt in
		printf "%3x %s %d(%d) %s \"%s\".\n%!" !i op tid rid (Xb.Op.to_string ty) (String.escaped data);
		in
	let y (tid, ty, data) =
		let spkt = (tid, !i, ty, data) in 
		disp "S: " spkt;
		send_packet xb tid !i ty data;
		let rpkt = recv_packet xb in
		disp "R: " rpkt;
		incr i;
		let (_, _, _, data) = rpkt in
		data
		in
	let x spkt = ignore (y spkt) in
	(* no arguments *)
	x (0, Xb.Op.Read, "\000");
	x (0, Xb.Op.Write, "\000");
	x (0, Xb.Op.Getdomainpath, "\000");
	x (0, Xb.Op.Directory, "\000");
	x (0, Xb.Op.Mkdir, "\000");
	x (0, Xb.Op.Getperms, "\000");
	x (0, Xb.Op.Setperms, "\000");

	(* too many arguments *)
	x (0, Xb.Op.Write, "/test\000xxx\000yyy\000\000zzz");
	x (0, Xb.Op.Read, "/test");
	x (0, Xb.Op.Read, "/test\000");
	x (0, Xb.Op.Read, "/test\000some\000otherargs");
	x (0, Xb.Op.Directory, "/test\000spurious\000abc");
	x (0, Xb.Op.Directory, "/test\000spurious");
	x (0, Xb.Op.Getperms, "/test\000someotherargs");
	x (0, Xb.Op.Getperms, "/test\000someotherargs\000others");

	(* others *)
	x (0, Xb.Op.Write, "/test\000");
	x (0, Xb.Op.Write, "/test\000abc");
	x (0, Xb.Op.Read, "/test");
	x (0, Xb.Op.Read, "/test\000");
	x (0, Xb.Op.Read, "/test\000\000");
	x (0, Xb.Op.Write, "/test");
	x (0, Xb.Op.Read, "/test");
	x (0, Xb.Op.Read, "/test\000");
	x (0, Xb.Op.Read, "/test\000\000");

	x (0, Xb.Op.Write, "\000/");

	x (0, Xb.Op.Directory, "/test");
	x (0, Xb.Op.Directory, "/test\000");
	x (0, Xb.Op.Directory, "/test\000\000");

	x (0, Xb.Op.Rm, "/test");
	x (0, Xb.Op.Directory, "/test");
	x (0, Xb.Op.Directory, "/test\000");
	x (0, Xb.Op.Directory, "/test\000\000");

	x (0, Xb.Op.Write, "/test/abc\000x1");
	x (0, Xb.Op.Write, "/test/def\000x2");
	x (0, Xb.Op.Write, "/test/xyz\000x3");
	x (0, Xb.Op.Write, "/test/xyz\000x4");

	x (0, Xb.Op.Directory, "/test");
	x (0, Xb.Op.Directory, "/test\000");
	x (0, Xb.Op.Directory, "/test\000spurious\000");

	x (0, Xb.Op.Getperms, "/test");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Getperms, "/test\000spurious\000");

	(* setperms getperms *)
	x (0, Xb.Op.Setperms, "/test");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Setperms, "/test\000");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Setperms, "/test\000\000");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Setperms, "/test\000n0\000");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Setperms, "/test\000n0");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Setperms, "/test\000n0\000r1");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Setperms, "/test\000n0\000r1\000");
	x (0, Xb.Op.Getperms, "/test\000");
	x (0, Xb.Op.Setperms, "/test\000n0r2\000spurious");
	x (0, Xb.Op.Getperms, "/test\000");

	(* get domain path *)
	x (0, Xb.Op.Getdomainpath, "3\000");
	x (0, Xb.Op.Getdomainpath, "10\000");
	x (0, Xb.Op.Getdomainpath, "13 \000");
	x (0, Xb.Op.Getdomainpath, " 17\000");
	x (0, Xb.Op.Getdomainpath, "45\000spurious");
	x (0, Xb.Op.Getdomainpath, "45\000really\000");

	let t1 = tnb (y (0, Xb.Op.Transaction_start, "\000")) in
	x (t1, Xb.Op.Transaction_end, "\000");
	let t2 = tnb (y (0, Xb.Op.Transaction_start, "\000")) in
	x (t2, Xb.Op.Transaction_end, string_of_int t2 ^ "\000");
	let t3 = tnb (y (0, Xb.Op.Transaction_start, "\000\000")) in
	x (t3, Xb.Op.Transaction_end, "0\000\000");
	()

(***************************************************************************
 * Main
 **************************************************************************)
open Pervasiveext

let () =
	let xb = open_xb () in
	finally (fun () -> do_test xb) (fun () -> close_xb xb);
