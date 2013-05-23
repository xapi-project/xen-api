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


let sockets_group = ref "xapi"

let qemu_vif_script = ref "/usr/lib/xcp/scripts/qemu-vif-script"

open Unix

let essentials = [
        X_OK, "qemu-vif-script", qemu_vif_script, "path to the qemu vif script";
]
