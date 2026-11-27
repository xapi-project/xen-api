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

open Xapi_database.Db_filter_types
open Vm_placement

(* === Snapshot constructors ================================================ *)

let create_guest_snapshot __context guest =
  let self = guest in
  {
    GS.id= Db.VM.get_uuid ~__context ~self
  ; GS.memory_overhead= Db.VM.get_memory_overhead ~__context ~self
  ; GS.memory_static_max= Db.VM.get_memory_static_max ~__context ~self
  ; GS.memory_static_min= Db.VM.get_memory_static_min ~__context ~self
  ; GS.memory_dynamic_max= Db.VM.get_memory_dynamic_max ~__context ~self
  ; GS.memory_dynamic_min= Db.VM.get_memory_dynamic_min ~__context ~self
  }

let create_host_snapshot __context host =
  let host_id = Db.Host.get_uuid ~__context ~self:host in
  let memory_overhead = Db.Host.get_memory_overhead ~__context ~self:host in
  let metrics = Db.Host.get_metrics ~__context ~self:host in
  let memory_total =
    Db.Host_metrics.get_memory_total ~__context ~self:metrics
  in
  let guest_snapshots guest_type =
    List.map
      (create_guest_snapshot __context)
      (Db.VM.get_refs_where ~__context
         ~expr:(Eq (Field guest_type, Literal (Ref.string_of host)))
      )
  in
  {
    HS.id= host_id
  ; HS.is_pool_master= Helpers.is_pool_master ~__context ~host
  ; HS.guests_resident= guest_snapshots "resident_on"
  ; HS.guests_scheduled= guest_snapshots "scheduled_to_be_resident_on"
  ; HS.memory_overhead
  ; HS.memory_total
  }

let create_pool_subset_snapshot __context pool hosts =
  let host_snapshots = List.map (create_host_snapshot __context) hosts in
  {PS.id= Ref.string_of pool; PS.hosts= host_snapshots}

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
  if affinity_host_is_valid then
    [Db.Host.get_uuid ~__context ~self:affinity_host]
  else
    []

(** Returns a single host (from the given list of hosts) on which the given [vm]
    can boot. @raise Api_errors.no_hosts_available if no such host exists. *)
let select_host __context guest validate_host hosts =
  let pool_summary =
    create_pool_subset_snapshot_summary __context [guest]
      (Helpers.get_pool ~__context)
      hosts
  in
  let affinity_host_ids = affinity_host_ids_of_guest __context guest in
  let random_fn =
    if Xapi_fist.deterministic_host_selection () then
      zero_fn
    else
      biased_random_fn
  in
  let validate_host uuid =
    validate_host (Db.Host.get_by_uuid ~__context ~uuid)
  in
  let pool = Helpers.get_pool ~__context in
  let master_bias = Db.Pool.get_coordinator_bias ~__context ~self:pool in
  let host =
    select_host_from_summary pool_summary affinity_host_ids validate_host
      master_bias random_fn
  in
  match host with
  | Some host ->
      Db.Host.get_by_uuid ~__context ~uuid:host
  | None ->
      raise (Api_errors.Server_error (Api_errors.no_hosts_available, []))

module StringSet = Set.Make (String)

let add_guests_to_host_summary guests host =
  let sum init guest_value =
    List.fold_left (fun acc g -> acc ++ guest_value g) init guests
  in
  {
    host with
    HSS.memory_static_min_sum=
      sum host.HSS.memory_static_min_sum (fun g -> g.GS.memory_static_min)
  ; HSS.memory_dynamic_min_sum=
      sum host.HSS.memory_dynamic_min_sum (fun g -> g.GS.memory_dynamic_min)
  ; HSS.memory_dynamic_max_sum=
      sum host.HSS.memory_dynamic_max_sum (fun g -> g.GS.memory_dynamic_max)
  ; HSS.memory_static_max_sum=
      sum host.HSS.memory_static_max_sum (fun g -> g.GS.memory_static_max)
  ; HSS.memory_available_sum=
      sum host.HSS.memory_available_sum (fun g -> 0L -- g.GS.memory_overhead)
  }

(** The caller must release the claim after reservation, including on failure. *)
let select_host_claiming claims __context guest validate_host hosts =
  (* A claim released during the snapshot may have become a reservation
     after its host was read. *)
  let claimed_before = Placement_claims.claims claims in
  let pool = Helpers.get_pool ~__context in
  let pool_snapshot = create_pool_subset_snapshot __context pool hosts in
  let guest_snapshot = create_guest_snapshot __context guest in
  let pool_summary = summarise_pool_snapshot [guest_snapshot] pool_snapshot in
  let affinity_host_ids = affinity_host_ids_of_guest __context guest in
  let random_fn =
    if Xapi_fist.deterministic_host_selection () then
      zero_fn
    else
      biased_random_fn
  in
  let master_bias = Db.Pool.get_coordinator_bias ~__context ~self:pool in
  let not_counted =
    List.fold_left
      (fun acc host ->
        List.fold_left
          (fun acc g -> StringSet.add g.GS.id acc)
          acc
          (host.HS.guests_resident @ host.HS.guests_scheduled)
      )
      (StringSet.singleton guest_snapshot.GS.id)
      pool_snapshot.PS.hosts
  in
  let counted claims =
    List.filter_map
      (fun (_, c) ->
        if StringSet.mem c.Placement_claims.guest.GS.id not_counted then
          None
        else
          Some c
      )
      claims
  in
  let counted_before = counted claimed_before in
  let in_flight current =
    let current = counted current in
    let ids =
      List.fold_left
        (fun acc c -> StringSet.add c.Placement_claims.guest.GS.id acc)
        StringSet.empty current
    in
    current
    @ List.filter
        (fun c -> not (StringSet.mem c.Placement_claims.guest.GS.id ids))
        counted_before
  in
  let summary ~excluded in_flight =
    let hosts =
      List.filter_map
        (fun host ->
          if List.mem host.HSS.id excluded then
            None
          else
            let claimed =
              List.filter_map
                (fun c ->
                  if c.Placement_claims.host = host.HSS.id then
                    Some c.Placement_claims.guest
                  else
                    None
                )
                in_flight
            in
            Some (add_guests_to_host_summary claimed host)
        )
        pool_summary.PSS.hosts
    in
    {pool_summary with PSS.hosts}
  in
  let rec select ~excluded =
    let claimed =
      Placement_claims.claim claims ~vm:guest (fun current ->
          let summary = summary ~excluded (in_flight current) in
          select_host_from_summary summary affinity_host_ids
            (fun _ -> true)
            master_bias random_fn
          |> Option.map (fun host ->
              {Placement_claims.host; guest= guest_snapshot}
          )
      )
    in
    match claimed with
    | None ->
        raise (Api_errors.Server_error (Api_errors.no_hosts_available, []))
    | Some {host= uuid; _} ->
        let host = Db.Host.get_by_uuid ~__context ~uuid in
        if validate_host host then
          host
        else (
          Placement_claims.release claims ~vm:guest ;
          select ~excluded:(uuid :: excluded)
        )
  in
  select ~excluded:[]
