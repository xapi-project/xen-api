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
(** Manage the lifecycle of HA metadata VDI
 * @group High Availability (HA)
*)

module D = Debug.Make(struct let name="xapi" end)
open D

open Client
open Stdext
open Listext
open Xstringext

let create ~__context ~sr =
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       Client.VDI.create ~rpc ~session_id
         ~name_label:"Metadata for HA"
         ~name_description:"Used for master failover"
         ~sR:sr ~virtual_size:Redo_log.minimum_vdi_size ~_type:`redo_log
         ~sharable:true ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:Redo_log.redo_log_sm_config ~tags:[]
    )

(** Return a reference to a valid metadata VDI in the given SR.
    This function prefers to reuse existing VDIs to avoid leaking the VDI when HA is disabled without statefile access. *)
let find_or_create ~__context ~sr =
  match
    List.filter
      (fun self -> true
                   && (Db.VDI.get_type ~__context ~self = `redo_log)
                   && (Db.VDI.get_virtual_size ~__context ~self >= Redo_log.minimum_vdi_size))
      (Db.SR.get_VDIs ~__context ~self:sr) with
  | x :: _ ->
    info "re-using existing metadata VDI: %s" (Db.VDI.get_uuid ~__context ~self:x);
    x
  | [] ->
    info "no suitable existing metadata VDI found; creating a fresh one";
    create ~__context ~sr


let list_existing () =
  List.filter (fun x -> x.Static_vdis.reason = Xapi_globs.ha_metadata_vdi_reason) (Static_vdis.list ())

(** Detach all statefiles attached with reason, to clear stale state *)
let detach_existing ~__context =
  let vdis = list_existing() in
  List.iter (fun x -> Static_vdis.permanent_vdi_detach_by_uuid ~__context ~uuid:x.Static_vdis.uuid) vdis

(** Added for CA-48539 *)
let deactivate_and_detach_existing ~__context =
  let vdi_uuids = List.map (fun vdi -> vdi.Static_vdis.uuid) (list_existing ()) in
  List.iter (fun vdi_uuid -> Static_vdis.permanent_vdi_deactivate_by_uuid ~__context ~uuid:vdi_uuid) vdi_uuids ;
  List.iter (fun vdi_uuid -> Static_vdis.permanent_vdi_detach_by_uuid ~__context ~uuid:vdi_uuid) vdi_uuids

open Pervasiveext

(** Attempt to flush the database to the metadata VDI *)
let flush_database ~__context log =
  try
    Redo_log.flush_db_to_redo_log (Db_ref.get_database (Db_backend.make ())) log
  with _ -> false
