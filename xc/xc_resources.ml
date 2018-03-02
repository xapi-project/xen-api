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
let igmp_query_injector_script = ref "/usr/libexec/xenopsd/igmp_query_injector.py"

let vncterm = ref "vncterm"
let xenguest = ref "xenguest"
let emu_manager = ref "emu-manager"
let tune2fs = ref "tune2fs"
let mount = ref "mount"
let umount = ref "umount"
let ionice = ref "ionice"
let setup_vif_rules = ref "setup-vif-rules"
let setup_pvs_proxy_rules = ref "setup-pvs-proxy-rules"
let vgpu = ref "vgpu"
let gimtool = ref "/opt/xensource/bin/gimtool"

let alternatives = ref "/usr/lib/xapi/alternatives"


open Unix

let essentials = [
  X_OK, "vbd-script", vbd_script, "path to the vbd backend script";
  X_OK, "vif-script", vif_script, "path to the vif backend script";
  X_OK, "xenguest", xenguest, "path to the xenguest binary";
  X_OK, "emu-manager", emu_manager, "path to the emu-manager binary";
  X_OK, "tune2fs", tune2fs, "path to the tune2fs binary";
  X_OK, "mount", mount, "path to the mount binary";
  X_OK, "umount", umount, "path to the umount binary";
  X_OK, "ionice", ionice, "path to the ionice binary";
  X_OK, "setup-vif-rules", setup_vif_rules, "path to the setup-vif-rules script";
  X_OK, "setup-pvs-proxy-rules", setup_pvs_proxy_rules, "path to the setup-pvs-proxy-rules script";
] @ Resources.network_configuration

let nonessentials = [
  X_OK, "pci-flr-script", pci_flr_script, "path to the PCI function-level reset script";
  X_OK, "alternatives", alternatives, "path to the alternative xenguests";
  X_OK, "vgpu", vgpu, "path to the vgpu binary";
  X_OK, "vncterm", vncterm, "path to the vncterm binary";
  X_OK, "gimtool", gimtool, "path to the gimtool binary";
  X_OK, "igmp-query-injector-script", igmp_query_injector_script, "path to the igmp query injector script";
] @ Resources.hvm_guests @ Resources.pv_guests @ Resources.pvinpvh_guests

