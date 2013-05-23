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
let vbd_script = ref "/etc/xen/scripts/block"

let pci_flr_script = ref "/usr/lib/xcp/lib/pci-flr"

let vncterm = ref "/usr/lib/xcp/lib/vncterm"
let xenguest = ref "/usr/lib/xcp/lib/xenguest"
let tune2fs = ref "/sbin/tune2fs"
let mkfs = ref "/sbin/mkfs"
let mount = ref "/bin/mount"
let umount = ref "/bin/umount"
let ionice = ref "/usr/bin/ionice"
let setup_vif_rules = ref "/usr/lib/xcp/lib/setup-vif-rules"
let hvmloader = ref "/usr/lib/xen-4.1/boot/hvmloader"
let pygrub = ref "/usr/lib/xen-4.1/bin/pygrub"
let eliloader = ref "/usr/bin/eliloader"

open Unix

let essentials = [
	X_OK, "vbd-script", vbd_script, "path to the vbd backend script";
	X_OK, "vif-script", vif_script, "path to the vif backend script";
	X_OK, "vncterm", vncterm, "path to the vncterm binary";
	X_OK, "xenguest", xenguest, "path to the xenguest binary";
	X_OK, "tune2fs", tune2fs, "path to the tune2fs binary";
	X_OK, "mkfs", mkfs, "path to the mkfs binary";
	X_OK, "mount", mount, "path to the mount binary";
	X_OK, "umount", umount, "path to the umount binary";
	X_OK, "ionice", ionice, "path to the ionice binary";
	X_OK, "setup-vif-rules", setup_vif_rules, "path to the setup-vif-rules script";
	R_OK, "hvmloader", hvmloader, "path to the hvmloader binary for HVM guests";
	X_OK, "pygrub", pygrub, "path to the pygrub bootloader binary";
	X_OK, "eliloader", eliloader, "path to the eliloader bootloader binary";
] @ Path.hvm_guests @ Path.network_configuration

let nonessentials = [
	X_OK, "pci-flr-script", pci_flr_script, "path to the PCI function-level reset script";
]

