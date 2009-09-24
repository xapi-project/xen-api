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
(* test dumpcore with OSS libxc *)

let finally fct clean_f =
	let result = try
		fct ();
	with
		exn ->
		  clean_f (); raise exn in
	clean_f ();
	result

let _ =
	let domid = ref (-1) in
	let file = ref "" in
	Arg.parse [
		"-domid", Arg.Set_int domid, "domid to dumpcore";
		"-file", Arg.Set_string file, "dumpcore filename"; ]
		(fun s -> ()) "dumpcore";

	let handle = Xenguest.init () in
	finally (fun () ->
		Xenguest.dumpcore handle !domid !file
		) (fun () -> Xenguest.close handle)
