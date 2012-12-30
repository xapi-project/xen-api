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

let config_spec = [
	"vif-script", Arg.Set_string vif_script, "Path to the vif backend script";
	"pci-flr-script", Arg.Set_string pci_flr_script, "Path to the PCI function-level reset script";
]

