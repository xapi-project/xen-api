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

type handle

type domid = Xenctrl.domid

type error = Unix.error * string

type +'a outcome = ('a, error) result

val handle_outcome : default:'a -> 'a outcome -> 'a
(** [handle_outcome ~default r] returns [default] when [r] is an error,
    and the underlying value otherwise.
    Logs errors, but doesn't raise exceptions
*)

external interface_open : unit -> handle = "stub_xenctrlext_interface_open"

val get_handle : unit -> handle

external domain_set_timer_mode : handle -> domid -> int -> unit
  = "stub_xenctrlext_domain_set_timer_mode"

external domain_send_s3resume : handle -> domid -> unit
  = "stub_xenctrlext_domain_send_s3resume"

external domain_get_acpi_s_state : handle -> domid -> int
  = "stub_xenctrlext_domain_get_acpi_s_state"

exception Unix_error of Unix.error * string

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

val domain_get_runstate_info : handle -> int -> runstateinfo outcome

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

type meminfo = {memfree: int64; memsize: int64}

type numainfo = {memory: meminfo array; distances: int array array}

type cputopo = {core: int; socket: int; node: int}

val vcpu_setaffinity_hard : handle -> domid -> int -> bool array -> unit

val vcpu_setaffinity_soft : handle -> domid -> int -> bool array -> unit

val numainfo : handle -> numainfo

val cputopoinfo : handle -> cputopo array

external combine_cpu_policies : int64 array -> int64 array -> int64 array
  = "stub_xenctrlext_combine_cpu_featuresets"

external policy_is_compatible : int64 array -> int64 array -> string option
  = "stub_xenctrlext_featuresets_are_compatible"

module NumaNode : sig
  type t

  val none : t

  val from : int -> t
end

val domain_claim_pages :
  handle -> domid -> ?numa_node:NumaNode.t -> int -> unit outcome
(** Returns {Unix_error} if there's not enough memory to claim in the system.
    Returns {`Not_available msg} if a single numa node is requested and xen does not
    provide page claiming for single numa nodes. *)

val get_nr_nodes : handle -> int outcome
(** Returns the count of NUMA nodes available in the system. *)

module DomainNuma : sig
  type domain_numainfo_node_pages = {
      tot_pages_per_node: int64 array (* page=4k bytes *)
  }

  val domain_get_numa_info_node_pages :
    handle -> int -> domain_numainfo_node_pages outcome

  type t = {optimised: bool; nodes: int; memory: int64 array (* bytes *)}

  val state : handle -> domid:int -> t
end

module HostNuma : sig
  type node_meminfo = {size: int64; free: int64; claimed: int64}

  val numa_get_meminfo : handle -> node_meminfo array outcome
end
