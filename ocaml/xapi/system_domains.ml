(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
 * @group Helper functions for handling system domains
*)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

(** If a VM is a system domain then xapi will perform lifecycle operations on demand,
    and will allow this VM to start even if a host is disabled. *)

let is_system_domain snapshot = snapshot.API.vM_is_control_domain
(* NOTE: code that recognises the other_config:is_system_domain key has been dropped *)

let get_is_system_domain ~__context ~self =
  is_system_domain (Db.VM.get_record ~__context ~self)

(* The storage driver domain for a PBD is now configured through the first-class
   PBD.storage_driver_domain field (a reference to the VM hosting the SR backend).
   A null (or dangling) reference means the backend runs in dom0. This replaces the
   previous other-config:storage_driver_domain key, which was dropped for security
   hardening (XSA-489 / CVE-2026-23561). *)

open Xapi_database.Db_filter_types

let pbd_of_vm ~__context ~vm =
  match
    Db.PBD.get_refs_where ~__context
      ~expr:(Eq (Field "storage_driver_domain", Literal (Ref.string_of vm)))
  with
  | pbd :: _ ->
      Some pbd
  | [] ->
      None

let storage_driver_domain_of_pbd ~__context ~pbd =
  let domain = Db.PBD.get_storage_driver_domain ~__context ~self:pbd in
  if Db.is_valid_ref __context domain then
    domain
  else
    Helpers.get_domain_zero ~__context

let storage_driver_domain_of_vbd ~__context ~vbd =
  let dom0 = Helpers.get_domain_zero ~__context in
  let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
  if Db.is_valid_ref __context vdi then
    let sr = Db.VDI.get_SR ~__context ~self:vdi in
    let sr_pbds = Db.SR.get_PBDs ~__context ~self:sr in
    let my_pbds = List.map fst (Helpers.get_my_pbds __context) in
    match Xapi_stdext_std.Listext.List.intersect sr_pbds my_pbds with
    | pbd :: _ ->
        storage_driver_domain_of_pbd ~__context ~pbd
    | _ ->
        dom0
  else
    dom0

let storage_driver_domain_of_sr_type ~__context ~_type:_ =
  Helpers.get_domain_zero ~__context

type service = {uuid: string; ty: string; instance: string; url: string}
[@@deriving rpc]

type services = service list [@@deriving rpc]

let service_to_queue = Hashtbl.create 10

let service_to_queue_m = Mutex.create ()

let register_service service queue =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.replace service_to_queue service queue
  )

let unregister_service service =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.remove service_to_queue service
  )

let list_services () =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.fold (fun service _ acc -> service :: acc) service_to_queue []
  )
