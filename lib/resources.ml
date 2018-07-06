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
let qemu_dm_wrapper = ref "qemu-dm-wrapper"
let qemu_system_i386 = ref "qemu-system-i386"
let upstream_compat_qemu_dm_wrapper = ref "qemu-wrapper"
let chgrp = ref "chgrp"
let modprobe = ref "/usr/sbin/modprobe"
let rmmod = ref "/usr/sbin/rmmod"
let hvmloader = ref "hvmloader"
let pygrub = ref "pygrub"
let eliloader = ref "eliloader"
let legacy_conv_tool = ref "convert-legacy-stream"
let verify_libxc_v2 = ref "verify-stream-v2"
let cpu_info_file = ref "/etc/xensource/boot_time_cpus"
let pvinpvh_xen = ref "/usr/libexec/xen/boot/xen-shim"

open Unix

let hvm_guests = [
  R_OK, "hvmloader", hvmloader, "path to the hvmloader binary for HVM guests";
  X_OK, "qemu-dm-wrapper", qemu_dm_wrapper, "path to the qemu-dm-wrapper script";
  X_OK, "qemu-system-i386", qemu_system_i386, "path to the qemu-system-i386 binary";
  X_OK, "upstream-compat-qemu-dm-wrapper", upstream_compat_qemu_dm_wrapper, "path to the upstream compat qemu-dm-wrapper script";
]

let pv_guests = [
  X_OK, "pygrub", pygrub, "path to the pygrub bootloader binary";
  X_OK, "eliloader", eliloader, "path to the eliloader bootloader binary";
]

let pvinpvh_guests = [
  X_OK, "pvinpvh-xen", pvinpvh_xen, "path to the inner-xen for PV-in-PVH guests";
]

(* libvirt xc *)
let network_configuration = [
  R_OK, "network-conf", network_conf, "path to the network backend switch";
]

let essentials = [
  X_OK, "chgrp", chgrp, "path to the chgrp binary";
  X_OK, "modprobe", modprobe, "path to the modprobe binary";
  X_OK, "rmmod", rmmod, "path to the rmmod binary";
]

let nonessentials = [
  X_OK, "convert-legacy-stream", legacy_conv_tool, "path to convert-legacy-stream tool";
  R_OK, "cpu-info-file", cpu_info_file, "Where to cache boot-time CPU info";
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
