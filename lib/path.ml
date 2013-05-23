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

let network_conf = ref "/etc/xcp/network.conf"
let qemu_dm_wrapper = ref "/usr/lib/xcp/lib/qemu-dm-wrapper"
let qemu_system_i386 = ref "/usr/lib/xen/bin/qemu-system-i386"
let chgrp = ref "/bin/chgrp"
let hvmloader = ref "/usr/lib/xen-4.1/boot/hvmloader"

open Unix

let hvm_guests = [
	R_OK, "hvmloader", hvmloader, "path to the hvmloader binary for HVM guests";
	X_OK, "qemu-dm-wrapper", qemu_dm_wrapper, "path to the qemu-dm-wrapper script";
	X_OK, "qemu-system-i386", qemu_system_i386, "path to the qemu-system-i386 binary";
]

(* libvirt xc *)
let network_configuration = [
	R_OK, "network-conf", network_conf, "path to the network backend switch";
]

let essentials = [
	X_OK, "chgrp", chgrp, "path to the chgrp binary";
]

let nonessentials = [
]

let make_resources ~essentials ~nonessentials =
	let open Xcp_service in
	List.map (fun (perm, name, path, description) -> {
		essential = true;
		name; description; path;
		perms = [ perm ]
	}) essentials @ (List.map (fun (perm, name, path, description) -> {
		essential = false;
		name; description; path;
		perms = [ perm ]
	}) nonessentials)
