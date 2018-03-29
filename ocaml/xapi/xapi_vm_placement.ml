(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(**
 * @group Virtual-Machine Management
*)

open Db_filter_types
open Vm_placement

(* === Snapshot constructors ================================================ *)

let create_guest_snapshot __context guest =
  let r = Db.VM.get_record ~__context ~self:guest in
  { GS.id                 = r.API.vM_uuid
  ; GS.memory_overhead    = r.API.vM_memory_overhead
  ; GS.memory_static_max  = r.API.vM_memory_static_max
  ; GS.memory_static_min  = r.API.vM_memory_static_min
  ; GS.memory_dynamic_max = r.API.vM_memory_dynamic_max
  ; GS.memory_dynamic_min = r.API.vM_memory_dynamic_min
  }

let create_host_snapshot __context host =
  let host_id = Db.Host.get_uuid __context host in
  let memory_overhead = Db.Host.get_memory_overhead ~__context ~self:host in
  let metrics = Db.Host.get_metrics ~__context ~self:host in
  let memory_total = Db.Host_metrics.get_memory_total ~__context ~self:metrics in
  let guest_snapshots guest_type = List.map
      (create_guest_snapshot __context)
      (Db.VM.get_refs_where ~__context
         ~expr:(Eq (Field guest_type, Literal (Ref.string_of host))))
  in
  { HS.id               = host_id
  ; HS.is_pool_master   = Helpers.is_pool_master ~__context ~host
  ; HS.guests_resident  = guest_snapshots "resident_on"
  ; HS.guests_scheduled = guest_snapshots "scheduled_to_be_resident_on"
  ; HS.memory_overhead  = memory_overhead
  ; HS.memory_total     = memory_total
  }

let create_pool_subset_snapshot __context pool hosts =
  let host_snapshots =  List.map (create_host_snapshot __context) hosts in
  { PS.id    = Ref.string_of pool
  ; PS.hosts = host_snapshots
  }

(* === Snapshot summary constructors ======================================== *)
let create_pool_subset_snapshot_summary __context extra_guests pool hosts =
  summarise_pool_snapshot
    (List.map (create_guest_snapshot __context) extra_guests)
    (create_pool_subset_snapshot __context pool hosts)

(* === Plumbing code ======================================================== *)

(** Returns a list of affinity host identifiers for the given [guest]. *)
let affinity_host_ids_of_guest __context guest =
  let affinity_host = Db.VM.get_affinity ~__context ~self:guest in
  let affinity_host_is_valid = Db.is_valid_ref __context affinity_host in
  if affinity_host_is_valid
  then [Db.Host.get_uuid __context affinity_host]
  else []

(** Returns a single host (from the given list of hosts) on which the given [vm]
    can boot. @raise Api_errors.no_hosts_available if no such host exists. *)
let select_host __context guest validate_host hosts =
  let pool_summary = create_pool_subset_snapshot_summary __context [guest]
      (Helpers.get_pool ~__context) hosts in
  let affinity_host_ids = affinity_host_ids_of_guest __context guest in
  let random_fn =
    if Xapi_fist.deterministic_host_selection ()
    then zero_fn
    else biased_random_fn in
  let validate_host =
    (fun uuid -> validate_host (Db.Host.get_by_uuid ~__context ~uuid)) in
  let host = select_host_from_summary pool_summary affinity_host_ids
      validate_host random_fn in
  match host with
  | Some (host) ->
    Db.Host.get_by_uuid ~__context ~uuid:host
  | None ->
    raise (Api_errors.Server_error (Api_errors.no_hosts_available, []))
