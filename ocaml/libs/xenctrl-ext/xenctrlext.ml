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

module D = Debug.Make (struct let name = "xenctrlext" end)

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

type error = Unix.error * string

type +'a outcome = ('a, error) result

let handle_outcome ~default = function
  | Ok v ->
      v
  | Error (err, fn) ->
      D.warn "Ignoring %s: %s" fn (Unix.error_message err) ;
      default

let error ~__FUNCTION__ code binding =
  D.debug "%s: %s: %s" __FUNCTION__ binding (Unix.error_message code) ;
  Result.error (code, binding)

let wrap ~__FUNCTION__ f =
  try Ok (f ()) with
  | Unix_error (code, binding) as e ->
      D.log_backtrace e ;
      error ~__FUNCTION__ code binding
  | Failure _ as e ->
      D.log_backtrace e ;
      error ~__FUNCTION__ Unix.ENOSYS __FUNCTION__
  | Xenctrl.Error _ as e ->
      D.log_backtrace e ;
      (* this indicates a bug in the stub, we shouldn't raise a Xenctrl.Error
         in a Xenctrlext stub. But we don't want to make that a
         hard failure, treat it as if the stub was completely missing when
         buggy. *)
      error ~__FUNCTION__ Unix.ENOSYS __FUNCTION__

let wrap0 ~__FUNCTION__ f (xc : handle) = wrap ~__FUNCTION__ @@ fun () -> f xc

let wrap1 ~__FUNCTION__ f (xc : handle) (arg1 : domid) =
  wrap ~__FUNCTION__ @@ fun () -> f xc arg1

let wrap3 ~__FUNCTION__ f (xc : handle) (arg1 : domid) arg2 arg3 =
  wrap ~__FUNCTION__ @@ fun () -> f xc arg1 arg2 arg3

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

(* this is always in our patchqueue, but not part of upstream Xen *)
let domain_get_runstate_info = wrap1 ~__FUNCTION__ domain_get_runstate_info

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

let vcpu_setaffinity_soft xc domid vcpu affinity =
  wrap3 ~__FUNCTION__ vcpu_setaffinity_soft xc domid vcpu affinity
  |> handle_outcome ~default:()

type meminfo = {memfree: int64; memsize: int64}

type numainfo = {memory: meminfo array; distances: int array array}

type cputopo = {core: int; socket: int; node: int}

external numainfo : handle -> numainfo = "stub_xenctrlext_numainfo"

external cputopoinfo : handle -> cputopo array = "stub_xenctrlext_cputopoinfo"

external combine_cpu_policies : int64 array -> int64 array -> int64 array
  = "stub_xenctrlext_combine_cpu_featuresets"

external policy_is_compatible : int64 array -> int64 array -> string option
  = "stub_xenctrlext_featuresets_are_compatible"

external domain_claim_pages : handle -> domid -> int -> int -> unit
  = "stub_xenctrlext_domain_claim_pages"

module NumaNode = struct
  type t = int

  (** Defined as XC_NUMA_NO_NODE in xen.git/tools/include/xenguest.h, it's an
      unsigned int (~0U) *)
  let none = 0xFFFFFFFF

  let from = Fun.id
end

let domain_claim_pages handle domid ?(numa_node = NumaNode.none) nr_pages =
  wrap3 ~__FUNCTION__ domain_claim_pages handle domid numa_node nr_pages

module HostNuma = struct
  (* Numa state of a host *)

  type node_meminfo = {size: int64; free: int64; claimed: int64}

  external numa_get_meminfo : handle -> node_meminfo array
    = "stub_xenctrlext_numa_meminfo"

  let numa_get_meminfo = wrap0 ~__FUNCTION__ numa_get_meminfo
end

let get_nr_nodes handle =
  let meminfo = HostNuma.numa_get_meminfo handle in
  Result.map Array.length meminfo

module DomainNuma = struct
  (* Numa state of a domain *)

  type domain_numainfo_node_pages = {
      tot_pages_per_node: int64 array (* page=4k bytes *)
  }

  external domain_get_numa_info_node_pages :
    handle -> int -> domain_numainfo_node_pages
    = "stub_xc_domain_numa_get_node_pages_wrapper"

  let domain_get_numa_info_node_pages =
    wrap1 ~__FUNCTION__ domain_get_numa_info_node_pages

  type t = {optimised: bool; nodes: int; memory: int64 array (* bytes *)}

  (* nodes = -1 signals that could not obtain a meaningful value *)
  let default = {optimised= false; nodes= -1; memory= [||]}

  let state handle ~domid =
    match
      (get_nr_nodes handle, domain_get_numa_info_node_pages handle domid)
    with
    | Ok host_nodes, Ok pages ->
        let memory =
          Array.map (fun n -> Int64.shift_left n 12) pages.tot_pages_per_node
        in
        let nodes =
          Array.fold_left
            (fun n pages ->
              if pages > 4096L then
                n + 1
              else
                n
            )
            0 pages.tot_pages_per_node
        in
        let optimised = nodes = 1 || nodes < host_nodes in
        {optimised; nodes; memory}
    | _ ->
        default
end
