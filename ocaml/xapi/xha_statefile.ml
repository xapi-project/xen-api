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
(** Manage the lifecycle of HA statefiles
 * @group High Availability (HA)
*)

module D = Debug.Make (struct let name = "xha_statefile" end)

open D
module Redo_log = Xapi_database.Redo_log

(** Reason associated with the static VDI attach, to help identify these later *)
let reason = "HA statefile"

(** CA-17239: special signal to the backend to give us a raw block device (not not a vhd format thing, for example *)
let statefile_sm_config = [("type", "raw")]

open Client

(** Return the minimum size of an HA statefile, as of
    XenServer HA state-file description vsn 1.3 *)
let minimum_statefile_size =
  let ( ** ) = Int64.mul and ( ++ ) = Int64.add in
  let global_section_size = 4096L
  and host_section_size = 4096L
  and maximum_number_of_hosts = 64L in
  global_section_size ++ (maximum_number_of_hosts ** host_section_size)

let round_to ~align n = Int64.(div (add n @@ sub align 1L) align |> mul align)

(* SM doesn't actually allow us to create VDIs smaller than 4MiB, so we need to round up *)
let minimum_sr_size =
  [minimum_statefile_size; Redo_log.minimum_vdi_size]
  |> List.map @@ round_to ~align:Int64.(shift_left 1L 22)
  |> List.fold_left Int64.add Int64.zero

let ha_fits_sr ~__context ~what ~sr ~typ ~minimum_size =
  let ha_fits self =
    Db.VDI.get_type ~__context ~self = typ
    && Db.VDI.get_virtual_size ~__context ~self >= minimum_size
  in
  match List.filter ha_fits (Db.SR.get_VDIs ~__context ~self:sr) with
  | x :: _ ->
      debug "Would re-use existing %s: %s" what
        (Db.VDI.get_uuid ~__context ~self:x) ;
      Some x
  | [] ->
      debug "no suitable existing %s found; would have to create a fresh one"
        what ;
      let self = sr in
      let size = Db.SR.get_physical_size ~__context ~self in
      let utilisation = Db.SR.get_physical_utilisation ~__context ~self in
      let free_space = Int64.sub size utilisation in
      if free_space < minimum_sr_size then (
        let sr = Ref.string_of sr in
        info "%s: SR %s size=%Ld utilisation=%Ld free=%Ld needed=%Ld"
          __FUNCTION__ sr size utilisation free_space minimum_sr_size ;
        raise
          (Api_errors.Server_error
             (Api_errors.sr_source_space_insufficient, [sr])
          )
      ) else
        None

let check_sr_can_host_statefile ~__context ~sr ~cluster_stack =
  (* Check that each host has a PBD to this SR *)
  let pbds = Db.SR.get_PBDs ~__context ~self:sr in
  let connected_hosts =
    Xapi_stdext_std.Listext.List.setify
      (List.map (fun self -> Db.PBD.get_host ~__context ~self) pbds)
  in
  let all_hosts = Db.Host.get_all ~__context in
  if List.length connected_hosts < List.length all_hosts then (
    error "Cannot place statefile in SR %s: some hosts lack a PBD: [ %s ]"
      (Ref.string_of sr)
      (String.concat "; "
         (List.map Ref.string_of
            (Xapi_stdext_std.Listext.List.set_difference all_hosts
               connected_hosts
            )
         )
      ) ;
    raise (Api_errors.Server_error (Api_errors.sr_no_pbds, [Ref.string_of sr]))
  ) ;
  (* Check that each PBD is plugged in *)
  List.iter
    (fun self ->
      if not (Db.PBD.get_currently_attached ~__context ~self) then (
        error "Cannot place statefile in SR %s: PBD %s is not plugged"
          (Ref.string_of sr) (Ref.string_of self) ;
        (* Same exception is used in this case (see Helpers.assert_pbd_is_plugged) *)
        raise
          (Api_errors.Server_error (Api_errors.sr_no_pbds, [Ref.string_of sr]))
      )
    )
    pbds ;
  (* Check cluster stack constraints *)
  Cluster_stack_constraints.assert_sr_compatible ~__context ~cluster_stack ~sr ;
  (* Check the exported capabilities of the SR's SM plugin *)
  let srtype = Db.SR.get_type ~__context ~self:sr in
  let open Xapi_database.Db_filter_types in
  match
    Db.SM.get_internal_records_where ~__context
      ~expr:(Eq (Field "type", Literal srtype))
  with
  | [] ->
      (* This should never happen because the PBDs are plugged in *)
      raise
        (Api_errors.Server_error
           ( Api_errors.internal_error
           , [
               "SR does not have corresponding SM record"
             ; Ref.string_of sr
             ; srtype
             ]
           )
        )
  | (_, sm) :: _ ->
      if
        (not (List.mem_assoc "VDI_GENERATE_CONFIG" sm.Db_actions.sM_features))
        && not (List.mem_assoc "VDI_ATTACH_OFFLINE" sm.Db_actions.sM_features)
      then
        raise
          (Api_errors.Server_error
             (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
          ) ;
      ha_fits_sr ~__context ~what:"statefile" ~sr
        ~minimum_size:minimum_statefile_size ~typ:`ha_statefile

let assert_sr_can_host_statefile ~__context ~sr ~cluster_stack =
  let (_ : 'a option) =
    check_sr_can_host_statefile ~__context ~sr ~cluster_stack
  in
  let (_ : _ option) =
    ha_fits_sr ~__context ~what:"redo-log" ~sr
      ~minimum_size:Redo_log.minimum_vdi_size ~typ:`redo_log
  in
  ()

let list_srs_which_can_host_statefile ~__context ~cluster_stack =
  List.filter
    (fun sr ->
      try
        assert_sr_can_host_statefile ~__context ~sr ~cluster_stack ;
        true
      with _ -> false
    )
    (Db.SR.get_all ~__context)

let create ~__context ~sr ~cluster_stack =
  assert_sr_can_host_statefile ~__context ~sr ~cluster_stack ;
  let size = minimum_statefile_size in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.VDI.create ~rpc ~session_id ~name_label:"Statefile for HA"
        ~name_description:"Used for storage heartbeating" ~sR:sr
        ~virtual_size:size ~_type:`ha_statefile ~sharable:true ~read_only:false
        ~other_config:[] ~xenstore_data:[] ~sm_config:statefile_sm_config
        ~tags:[]
  )

(** Return a reference to a valid statefile VDI in the given SR.
    This function prefers to reuse existing VDIs to avoid confusing the heartbeat component:
    it expects to see a poisoned VDI but not necessarily a stale or corrupted one. Consider that
    when using LVM-based SRs the VDI could be deleted on the master but the slaves would still
    have access to stale data. *)
let find_or_create ~__context ~sr ~cluster_stack =
  match check_sr_can_host_statefile ~__context ~sr ~cluster_stack with
  | Some x ->
      info "re-using existing statefile: %s" (Db.VDI.get_uuid ~__context ~self:x) ;
      x
  | None ->
      info "no suitable existing statefile found; creating a fresh one" ;
      create ~__context ~sr ~cluster_stack

let list_existing_statefiles () =
  List.filter (fun x -> x.Static_vdis.reason = reason) (Static_vdis.list ())

(** Detach all statefiles attached with reason 'HA statefile', to clear stale state *)
let detach_existing_statefiles ~__context =
  let statefile_uuids =
    List.map (fun vdi -> vdi.Static_vdis.uuid) (list_existing_statefiles ())
  in
  List.iter
    (fun uuid -> Static_vdis.permanent_vdi_detach_by_uuid ~__context ~uuid)
    statefile_uuids
