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
open Xenops_utils

module D = Debug.Make(struct let name = "xenctrl" end)
open D

type handle = unit
let uuid_of_handle = failwith "uuid_of_handle"


let with_intf f = failwith "with_intf"

type domid = int

type dominfo = {
	domid: domid;
	handle: handle;
	cpu_time: int64;
	shutdown: bool;
	shutdown_code: int;
	total_memory_pages: nativeint;
	hvm_guest: bool;
}

let domain_getinfo xc domid = {
	domid = 0;
	handle = ();
	cpu_time = 0L;
	shutdown = false;
	shutdown_code = 0;
	total_memory_pages = 0n;
	hvm_guest = false;
}
let domain_getinfolist xc from = [ domain_getinfo xc from ]

let domain_setmaxmem xc domid max = ()

let readconsolering () = "console"

type physinfo = {
	total_pages: nativeint;
}

let physinfo () = {
	total_pages = 0n;
}

let send_debug_keys xc keys = ()

let get_max_nr_cpus xc = 0

let shadow_allocation_get xc domid = 0
let shadow_allocation_set xc domid x = ()
