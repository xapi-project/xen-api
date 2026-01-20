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

external interface_open : unit -> handle = "stub_xenctrlext_interface_open"

let handle = ref None

let get_handle () =
  match !handle with
  | Some h ->
      h
  | None ->
      let h =
        try interface_open ()
        with e ->
          let msg = Printexc.to_string e in
          failwith ("failed to open xenctrlext: " ^ msg)
      in
      handle := Some h ;
      h

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

external vcpu_setaffinity_hard : handle -> domid -> int -> bool array -> unit
  = "stub_xenctrlext_vcpu_setaffinity_hard"

external vcpu_setaffinity_soft : handle -> domid -> int -> bool array -> unit
  = "stub_xenctrlext_vcpu_setaffinity_soft"

type meminfo = {memfree: int64; memsize: int64}

type numainfo = {memory: meminfo array; distances: int array array}

type cputopo = {core: int; socket: int; node: int}

external numainfo : handle -> numainfo = "stub_xenctrlext_numainfo"

external cputopoinfo : handle -> cputopo array = "stub_xenctrlext_cputopoinfo"

external combine_cpu_policies : int64 array -> int64 array -> int64 array
  = "stub_xenctrlext_combine_cpu_featuresets"

external policy_is_compatible : int64 array -> int64 array -> string option
  = "stub_xenctrlext_featuresets_are_compatible"

external stub_domain_claim_pages : handle -> domid -> int -> int -> unit
  = "stub_xenctrlext_domain_claim_pages"

module NumaNode = struct
  type t = int

  (** Defined as XC_NUMA_NO_NODE in xen.git/tools/include/xenguest.h, it's an
      unsigned int (~0U) *)
  let none = 0xFFFFFFFF

  let from = Fun.id
end

exception Not_available

let domain_claim_pages handle domid ?(numa_node = NumaNode.none) nr_pages =
  if numa_node <> NumaNode.none then raise Not_available ;
  stub_domain_claim_pages handle domid numa_node nr_pages

let get_nr_nodes handle =
  let info = numainfo handle in
  Array.length info.memory

module DomainNuma = struct
  (* Numa state of a domain *)

  type domain_numainfo_node_pages = {
      tot_pages_per_node: int64 array (* page=4k bytes *)
  }

  external domain_get_numa_info_node_pages :
    handle -> int -> domain_numainfo_node_pages
    = "stub_xc_domain_numa_get_node_pages_wrapper"

  type t = {optimised: bool; nodes: int; memory: int64 array (* bytes *)}

  (* nodes = -1 signals that could not obtain a meaningful value *)
  let default = {optimised= false; nodes= -1; memory= [||]}

  let state handle ~domid =
    try
      let host_nodes = get_nr_nodes handle in
      let pages = domain_get_numa_info_node_pages handle domid in
      let memory =
        Array.map (fun n -> Int64.shift_left n 12) pages.tot_pages_per_node
      in
      let nodes =
        Array.fold_left
          (fun n pages ->
            if pages > 0L then
              n + 1
            else
              n
          )
          0 pages.tot_pages_per_node
      in
      let optimised = nodes = 1 || nodes < host_nodes in
      {optimised; nodes; memory}
    with Failure _ -> default
end
