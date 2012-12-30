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

let vif_script = ref "/usr/lib/xcp/scripts/vif"
let pci_flr_script = ref "/usr/lib/xcp/lib/pci-flr"
let vncterm_wrapper = ref "/usr/lib/xcp/lib/vncterm-wrapper"
let vncterm = ref "/usr/lib/xen/bin/vncterm"
let xenguest = ref "/usr/lib/xcp/lib/xenguest"
let qemu_dm_wrapper = ref "/usr/lib/xcp/lib/qemu-dm-wrapper"

let config_spec = [
	"vif-script", Arg.Set_string vif_script, "Path to the vif backend script";
	"pci-flr-script", Arg.Set_string pci_flr_script, "Path to the PCI function-level reset script";
	"vncterm-wrapper", Arg.Set_string vncterm_wrapper, "Path to the vncterm-wrapper script";
	"vncterm", Arg.Set_string vncterm, "Path to the vncterm binary";
	"xenguest", Arg.Set_string xenguest, "Path to the xenguest binary";
	"qemu-dm-wrapper", Arg.Set_string qemu_dm_wrapper, "Path to the qemu-dm-wrapper script";
]

