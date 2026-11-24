(*
 * Copyright (C) Cloud Software Group
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

module D = Debug.Make (struct let name = "xapi_gpu_partition" end)

open D
module Lifecycle = Gpu.Gpu_partition_lifecycle

type chooser =
  __context:Context.t -> self:API.ref_VGPU -> API.ref_GPU_partition option

let choose_none : chooser = fun ~__context:_ ~self:_ -> None

let string_of_action = Lifecycle.string_of_ref_action Ref.string_of

let ref_of_action = function
  | Lifecycle.Set v ->
      v
  | Lifecycle.Clear | Lifecycle.Untouched ->
      Ref.null

let initial_refs =
  let open Lifecycle in
  let eff = outcome_of Create ~chosen:None in
  (ref_of_action eff.resident, ref_of_action eff.scheduled)

type binding = Resident | Scheduled

let string_of_binding = function
  | Resident ->
      "resident"
  | Scheduled ->
      "scheduled"

(* The only reads and writes of a VGPU's partition references. *)
let get_binding ~__context ~self = function
  | Resident ->
      Db.VGPU.get_resident_on_partition ~__context ~self
  | Scheduled ->
      Db.VGPU.get_scheduled_to_be_resident_on_partition ~__context ~self

let set_binding ~__context ~self binding ~value =
  match binding with
  | Resident ->
      Db.VGPU.set_resident_on_partition ~__context ~self ~value
  | Scheduled ->
      Db.VGPU.set_scheduled_to_be_resident_on_partition ~__context ~self ~value

(* The card-level reference a partition reference has to agree with. *)
let card_of_binding ~__context ~self = function
  | Resident ->
      Db.VGPU.get_resident_on ~__context ~self
  | Scheduled ->
      Db.VGPU.get_scheduled_to_be_resident_on ~__context ~self

type location = {
    card: API.ref_PGPU
  ; host: API.ref_host
  ; generation: int64  (** the card's partition_layout_generation *)
}

(* Where a partition is. [None] once the partition or its card has been
   destroyed: either can go between two reads, so this does not test validity
   first and then read. *)
let locate ~__context partition =
  try
    let card = Db.GPU_partition.get_PGPU ~__context ~self:partition in
    let host = Db.PGPU.get_host ~__context ~self:card in
    let generation =
      Db.PGPU.get_partition_layout_generation ~__context ~self:card
    in
    Some {card; host; generation}
  with Db_exn.DBCache_NotFound _ -> None

type resolution =
  | Unbound
  | Live of API.ref_GPU_partition
  | Stale_generation
  | Wrong_host
  | Destroyed

let string_of_resolution = function
  | Unbound ->
      "Unbound"
  | Live partition ->
      Printf.sprintf "Live %s" (Ref.string_of partition)
  | Stale_generation ->
      "Stale_generation"
  | Wrong_host ->
      "Wrong_host"
  | Destroyed ->
      "Destroyed"

let resolve_ref ~__context ~self binding ~on_host =
  let partition = get_binding ~__context ~self binding in
  if partition = Ref.null then
    Unbound
  else
    match locate ~__context partition with
    | None ->
        Destroyed
    | Some {host; _} when host <> on_host ->
        Wrong_host
    | Some {generation; _}
      when generation
           <> Db.VGPU.get_partition_layout_generation ~__context ~self ->
        Stale_generation
    | Some _ ->
        Live partition

let occupants ~__context ~self:partition =
  match locate ~__context partition with
  | None ->
      []
  | Some {host; _} ->
      let holds binding vgpu =
        resolve_ref ~__context ~self:vgpu binding ~on_host:host = Live partition
      in
      List.filter (holds Resident)
        (Db.GPU_partition.get_resident_VGPUs ~__context ~self:partition)
      @ List.filter (holds Scheduled)
          (Db.GPU_partition.get_scheduled_VGPUs ~__context ~self:partition)
      |> Xapi_stdext_std.Listext.List.setify

let clear_destroyed_refs ~__context =
  let clear self binding =
    let partition = get_binding ~__context ~self binding in
    if partition <> Ref.null && Option.is_none (locate ~__context partition)
    then (
      info
        "%s: VGPU %s: %s partition %s no longer exists; clearing the reference"
        __FUNCTION__ (Ref.string_of self)
        (string_of_binding binding)
        (Ref.string_of partition) ;
      set_binding ~__context ~self binding ~value:Ref.null
    )
  in
  Db.VGPU.get_all ~__context
  |> List.iter (fun self -> clear self Resident ; clear self Scheduled)

let apply_ref_action ~__context ~self binding action =
  let open Lifecycle in
  match action with
  | Untouched ->
      ()
  | Clear ->
      set_binding ~__context ~self binding ~value:Ref.null
  | Set value ->
      set_binding ~__context ~self binding ~value

(* The references an outcome sets, with the partition it sets them to. *)
let bindings_set (eff : _ Lifecycle.outcome) =
  let set binding = function
    | Lifecycle.Set partition ->
        [(binding, partition)]
    | Lifecycle.Clear | Lifecycle.Untouched ->
        []
  in
  set Resident eff.resident @ set Scheduled eff.scheduled

(* A pick is admitted when the partition still exists and is on the card the
   VGPU holds at the card level for every reference the transition would set
   with it. Returns the partition's location so its generation can be
   recorded. A transition that sets nothing with the pick ignores it. *)
let admit ~__context ~self transition partition =
  let eff = Lifecycle.outcome_of transition ~chosen:(Some partition) in
  if bindings_set eff = [] then
    None
  else
    match locate ~__context partition with
    | None ->
        error "%s: VGPU %s %s: chosen partition %s no longer exists"
          __FUNCTION__ (Ref.string_of self)
          (Lifecycle.string_of_transition transition)
          (Ref.string_of partition) ;
        None
    | Some ({card; _} as location) -> (
        let off_card =
          List.filter
            (fun (binding, _) ->
              card_of_binding ~__context ~self binding <> card
            )
            (bindings_set eff)
        in
        match off_card with
        | [] ->
            Some location
        | (binding, _) :: _ ->
            error
              "%s: VGPU %s %s: chosen partition %s is on PGPU %s, but the \
               VGPU's %s PGPU is %s"
              __FUNCTION__ (Ref.string_of self)
              (Lifecycle.string_of_transition transition)
              (Ref.string_of partition) (Ref.string_of card)
              (string_of_binding binding)
              (Ref.string_of (card_of_binding ~__context ~self binding)) ;
            None
      )

let apply_nolock ~__context ~self ?(choose = choose_none) transition =
  let open Lifecycle in
  let chosen =
    Option.bind (choose ~__context ~self) (fun partition ->
        admit ~__context ~self transition partition
        |> Option.map (fun location -> (partition, location))
    )
  in
  let eff = outcome_of transition ~chosen:(Option.map fst chosen) in
  debug "%s: VGPU %s %s -> resident:%s scheduled:%s" __FUNCTION__
    (Ref.string_of self)
    (string_of_transition transition)
    (string_of_action eff.resident)
    (string_of_action eff.scheduled) ;
  (* Read before writing: whether this is a new binding depends on what the
     VGPU held before the transition. *)
  let held =
    [
      get_binding ~__context ~self Resident
    ; get_binding ~__context ~self Scheduled
    ]
  in
  apply_ref_action ~__context ~self Resident eff.resident ;
  apply_ref_action ~__context ~self Scheduled eff.scheduled ;
  match chosen with
  | Some (partition, {generation; _})
    when bindings_set eff <> [] && not (List.mem partition held) ->
      Db.VGPU.set_partition_layout_generation ~__context ~self ~value:generation
  | Some _ | None ->
      ()

let apply ~__context ~self ?choose transition =
  Helpers.with_global_lock (fun () ->
      apply_nolock ~__context ~self ?choose transition
  )
