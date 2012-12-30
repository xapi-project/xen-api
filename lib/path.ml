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
let tune2fs = ref "/sbin/tune2fs"
let mkfs = ref "/sbin/mkfs"
let mount = ref "/bin/mount"
let umount = ref "/bin/umount"
let ionice = ref "/usr/bin/ionice"
let setup_vif_rules = ref "/usr/lib/xcp/lib/setup-vif-rules"

let table = [
	"vif-script", vif_script, "path to the vif backend script";
	"pci-flr-script", pci_flr_script, "path to the PCI function-level reset script";
	"vncterm-wrapper", vncterm_wrapper, "path to the vncterm-wrapper script";
	"vncterm", vncterm, "path to the vncterm binary";
	"xenguest", xenguest, "path to the xenguest binary";
	"qemu-dm-wrapper", qemu_dm_wrapper, "path to the qemu-dm-wrapper script";
	"tune2fs", tune2fs, "path to the tune2fs binary";
	"mkfs", mkfs, "path to the mkfs binary";
	"mount", mount, "path to the mount binary";
	"umount", umount, "path to the umount binary";
	"ionice", ionice, "path to the ionice binary";
	"setup-vif-rules", setup_vif_rules, "path to the setup-vif-rules script";
]

let config_spec = List.map (fun (a, b, c) -> a, Arg.Set_string b, c) table
