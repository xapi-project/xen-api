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

let vif_script = ref "/usr/lib/xcp/scripts/vif-xl"
let vbd_script = ref "/usr/lib/xcp/scripts/block-xl"

let vncterm = ref "/usr/lib/xcp/lib/vncterm"
let xenguest = ref "/usr/lib/xcp/lib/xenguest"
let mount = ref "/bin/mount"
let umount = ref "/bin/umount"
let ionice = ref "/usr/bin/ionice"
let setup_vif_rules = ref "/usr/lib/xcp/lib/setup-vif-rules"
let vnc_dir = ref ""

open Unix

let essentials = [
	X_OK, "vbd-xl-script", vbd_script, "path to the vbd-xl backend script";
	X_OK, "vif-xl-script", vif_script, "path to the vif-xl backend script";
	X_OK, "vncterm", vncterm, "path to the vncterm binary";
	X_OK, "xenguest", xenguest, "path to the xenguest binary";
	X_OK, "mount", mount, "path to the mount binary";
	X_OK, "umount", umount, "path to the umount binary";
	X_OK, "ionice", ionice, "path to the ionice binary";
	X_OK, "setup-vif-rules", setup_vif_rules, "path to the setup-vif-rules script";
] @ Path.hvm_guests @ Path.pv_guests @ Path.network_configuration

let nonessentials = [
]

