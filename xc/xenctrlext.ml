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

external get_boot_cpufeatures :
  handle -> int32 * int32 * int32 * int32 * int32 * int32 * int32 * int32
  = "stub_xenctrlext_get_boot_cpufeatures"

external domain_set_timer_mode : handle -> domid -> int -> unit
  = "stub_xenctrlext_domain_set_timer_mode"

external domain_send_s3resume : handle -> domid -> unit
  = "stub_xenctrlext_domain_send_s3resume"

external domain_get_acpi_s_state : handle -> domid -> int
  = "stub_xenctrlext_domain_get_acpi_s_state"

exception Unix_error of Unix.error * string

let _ =
  Callback.register_exception "Xenctrlext.Unix_error"
    (Unix_error (Unix.E2BIG, ""))

type runstateinfo = {
    state: int32
  ; missed_changes: int32
  ; state_entry_time: int64
  ; time0: int64
  ; time1: int64
  ; time2: int64
  ; time3: int64
  ; time4: int64
  ; time5: int64
}

external domain_get_runstate_info : handle -> int -> runstateinfo
  = "stub_xenctrlext_get_runstate_info"

external get_max_nr_cpus : handle -> int = "stub_xenctrlext_get_max_nr_cpus"

external domain_set_target : handle -> domid -> domid -> unit
  = "stub_xenctrlext_domain_set_target"

external physdev_map_pirq : handle -> domid -> int -> int
  = "stub_xenctrlext_physdev_map_pirq"

external assign_device : handle -> domid -> int -> int -> unit
  = "stub_xenctrlext_assign_device"

external deassign_device : handle -> domid -> int -> unit
  = "stub_xenctrlext_deassign_device"

external domid_quarantine : unit -> int = "stub_xenctrlext_domid_quarantine"

external domain_soft_reset : handle -> domid -> unit
  = "stub_xenctrlext_domain_soft_reset"

external domain_update_channels : handle -> domid -> int -> int -> unit
  = "stub_xenctrlext_domain_update_channels"

external vcpu_setaffinity_soft : handle -> domid -> int -> bool array -> unit
  = "stub_xenctrlext_vcpu_setaffinity_soft"

type meminfo = {memfree: int64; memsize: int64}

type numainfo = {memory: meminfo array; distances: int array array}

type cputopo = {core: int; socket: int; node: int}

external numainfo : handle -> numainfo = "stub_xenctrlext_numainfo"

external cputopoinfo : handle -> cputopo array = "stub_xenctrlext_cputopoinfo"

external xc_get_msr_arch_caps : handle -> int64
  = "stub_xenctrlext_get_msr_arch_caps"
