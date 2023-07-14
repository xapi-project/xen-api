(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

open Xapi_globs

module D = Debug.Make (struct let name = "cpuid_helpers" end)

open D

(** Field definitions for checked string map access *)
let features_t t =
  Map_check.pickler
    (Xenops_interface.CPU_policy.of_string t)
    Xenops_interface.CPU_policy.to_string

let features =
  Map_check.(field Xapi_globs.cpu_info_features_key (features_t `vm))

let features_pv =
  Map_check.(field Xapi_globs.cpu_info_features_pv_key (features_t `host))

let features_hvm =
  Map_check.(field Xapi_globs.cpu_info_features_hvm_key (features_t `host))

let features_pv_host =
  Map_check.(field Xapi_globs.cpu_info_features_pv_host_key (features_t `host))

let features_hvm_host =
  Map_check.(field Xapi_globs.cpu_info_features_hvm_host_key (features_t `host))

let cpu_count = Map_check.(field "cpu_count" int)

let socket_count = Map_check.(field "socket_count" int)

let vendor = Map_check.(field "vendor" string)

let get_flags_for_vm ~__context vm cpu_info =
  let features_field =
    match Helpers.domain_type ~__context ~self:vm with
    | `hvm | `pv_in_pvh | `pvh ->
        features_hvm
    | `pv ->
        features_pv
  in
  let vendor = List.assoc cpu_info_vendor_key cpu_info in
  let migration = Map_check.getf features_field cpu_info in
  (vendor, migration)

(* Return the featureset to be used for the next boot of the given VM. *)
let next_boot_cpu_features ~__context ~vm =
  (* On VM.start, the feature set is inherited from the pool level (PV or HVM) *)
  let pool = Helpers.get_pool ~__context in
  let pool_cpu_info = Db.Pool.get_cpu_info ~__context ~self:pool in
  let features_field_boot =
    (* Always use VM.domain_type, even if the VM is running, because we
       need the features for when the VM starts next. *)
    let domain_type =
      Db.VM.get_domain_type ~__context ~self:vm |> Helpers.check_domain_type
    in
    match domain_type with
    | `hvm | `pv_in_pvh | `pvh ->
        features_hvm_host
    | `pv ->
        features_pv_host
  in
  Map_check.getf features_field_boot pool_cpu_info
  |> Xenops_interface.CPU_policy.to_string

let get_host_cpu_info ~__context ~vm:_ ~host ?remote () =
  match remote with
  | None ->
      Db.Host.get_cpu_info ~__context ~self:host
  | Some (rpc, session_id) ->
      Client.Client.Host.get_cpu_info ~rpc ~session_id ~self:host

let get_host_compatibility_info ~__context ~vm ~host ?remote () =
  get_host_cpu_info ~__context ~vm ~host ?remote ()
  |> get_flags_for_vm ~__context vm

(* Compare the CPU on which the given VM was last booted to the CPU of the given host. *)
let assert_vm_is_compatible ~__context ~vm ~host ?remote () =
  let fail msg =
    raise
      (Api_errors.Server_error
         ( Api_errors.vm_incompatible_with_this_host
         , [Ref.string_of vm; Ref.string_of host; msg]
         )
      )
  in
  if Db.VM.get_power_state ~__context ~self:vm <> `Halted then
    let open Xapi_xenops_queue in
    let module Xenopsd = (val make_client (default_xenopsd ()) : XENOPS) in
    let dbg = Context.string_of_task __context in
    try
      let host_cpu_vendor, host_cpu_features =
        get_host_compatibility_info ~__context ~vm ~host ?remote ()
      in
      let vm_cpu_info = Db.VM.get_last_boot_CPU_flags ~__context ~self:vm in
      if List.mem_assoc cpu_info_vendor_key vm_cpu_info then (
        (* Check the VM was last booted on a CPU with the same vendor as this host's CPU. *)
        let vm_cpu_vendor = List.assoc cpu_info_vendor_key vm_cpu_info in
        debug "VM last booted on CPU of vendor %s; host CPUs are of vendor %s"
          vm_cpu_vendor host_cpu_vendor ;
        if vm_cpu_vendor <> host_cpu_vendor then
          fail
            "VM last booted on a host which had a CPU from a different vendor."
      ) ;
      if List.mem_assoc cpu_info_features_key vm_cpu_info then (
        (* Check the VM was last booted on a CPU whose features are a subset of the features of this host's CPU. *)
        let vm_cpu_features = Map_check.getf features vm_cpu_info in
        debug
          "VM last booted on CPU with features %s; host CPUs have migration \
           features %s"
          (Xenops_interface.CPU_policy.to_string vm_cpu_features)
          (Xenops_interface.CPU_policy.to_string host_cpu_features) ;
        if not (Xenopsd.HOST.is_compatible dbg vm_cpu_features host_cpu_features)
        then (
          debug
            "VM CPU features (%s) are not compatible with host CPU features (%s)\n"
            (Xenops_interface.CPU_policy.to_string vm_cpu_features)
            (Xenops_interface.CPU_policy.to_string host_cpu_features) ;
          fail
            "VM last booted on a CPU with features this host's CPU does not \
             have."
        )
      )
    with Not_found ->
      fail
        "Host does not have new leveling feature keys - not comparing VM's \
         flags"
