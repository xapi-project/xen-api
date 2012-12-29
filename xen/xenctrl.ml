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

let xc_open () = failwith "xc_open"
let with_intf f = failwith "with_intf"

exception Error of string

type domid = int

type dominfo = {
	domid: domid;
	handle: handle;
	cpu_time: int64;
	shutdown: bool;
	shutdown_code: int;
	paused: bool;
	total_memory_pages: nativeint;
	max_memory_pages: nativeint;
	hvm_guest: bool;
}

let domain_getinfo xc domid = {
	domid = 0;
	handle = ();
	cpu_time = 0L;
	shutdown = false;
	shutdown_code = 0;
	paused = false;
	total_memory_pages = 0n;
	max_memory_pages = 0n;
	hvm_guest = false;
}
let domain_getinfolist xc from = [ domain_getinfo xc from ]

let domain_setmaxmem xc domid max = ()

let readconsolering () = "console"

type flags =
| CDF_HVM
| CDF_HAP

let domain_create xc ssid flags uuid = 1
let domain_sethandle xc domid uuid = ()

let domain_pause xc domid = ()
let domain_unpause xc domid = ()

type physinfo = {
	total_pages: nativeint;
	free_pages: nativeint;
	scrub_pages: nativeint;
}

let physinfo () = {
	total_pages = 0n;
	free_pages = 0n;
	scrub_pages = 0n;
}

let send_debug_keys xc keys = ()

let get_max_nr_cpus xc = 0

let shadow_allocation_get xc domid = 0
let shadow_allocation_set xc domid x = ()

let pages_to_kib x = Int64.div x 4L

type reason = Reboot | Halt | Poweroff | Suspend | Crash

let domain_shutdown xc domid reason = ()
let domain_destroy xc domid = ()
let domain_resume_fast xc domid = ()

let domain_ioport_permission xc domid first_ports nr_ports perm = ()

let domain_iomem_permission xc domid first_pfn nr_pfns perm = ()

let domain_test_assign_device xc domid (domain, bus, slot, func) = true
let domain_assign_device xc domid (domain, bus, slot, func) = ()
let domain_deassign_device xc domid (domain, bus, slot, func) = ()

let domain_irq_permission xc domid irq bool = ()

let domain_set_timer_mode xc domid mode = ()
let domain_set_hpet xc domid hpet = ()
let domain_set_vpt_align xc domid vpt_align = ()
let domain_max_vcpus xc domid vcpus = ()
let domain_set_memmap_limit xc domid mib = ()

let domain_send_s3resume xc domid = ()
let domain_trigger_power xc domid = ()
let domain_trigger_sleep xc domid = ()

let domain_cpuid_set xc domid node id = [| Some "hello" |]
let cpuid_check xc node id = true, [| Some "hello" |]
let domain_cpuid_apply_policy xc domid = ()

let domain_suppress_spurious_page_faults xc domid = ()

let vcpu_affinity_set xc domid vcpu cpumap = ()
let vcpu_affinity_get xc domid vcpu = [| true |]

let domain_set_machine_address_size xc domid width = ()

let evtchn_alloc_unbound xc a b = 0
