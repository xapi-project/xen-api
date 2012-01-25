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

open Xenctrl

external get_boot_cpufeatures: handle ->  (int32 * int32 * int32 * int32 * int32 * int32 * int32 * int32) = "stub_xenctrlext_get_boot_cpufeatures" 

external domain_set_timer_mode: handle -> domid -> int -> unit = "stub_xenctrlext_domain_set_timer_mode"
external domain_set_hpet: handle -> domid -> int -> unit = "stub_xenctrlext_domain_set_hpet"
external domain_set_vpt_align: handle -> domid -> int -> unit = "stub_xenctrlext_domain_set_vpt_align"

external domain_send_s3resume: handle -> domid -> unit = "stub_xenctrlext_domain_send_s3resume"
external domain_get_acpi_s_state: handle -> domid -> int = "stub_xenctrlext_domain_get_acpi_s_state"
