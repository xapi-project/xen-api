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
open Pervasiveext

let xc = Xenctrl.interface_open() 
let domid = ref None
let handle = ref None

let _ =
	Arg.parse (Arg.align [
		"-domid", Arg.Int (fun i -> domid := Some i),
			" the domain id whose handle we will change";
		"-handle", Arg.String (fun i -> handle := Some i),
			" the new handle value";
		]) (fun x -> Printf.printf "Warning, ignoring unknown argument: %s" x)
		"Set a domain's handle";
	match !domid, !handle with
		| Some domid, Some handle ->
			Xenctrl.domain_sethandle xc domid handle
		| _, _ ->
			failwith "Must have -domid and -handle arguments"
