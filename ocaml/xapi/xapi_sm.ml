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
 * @group Storage
*)

(* The SMAPIv1 plugins are a static set in the filesystem.
   The SMAPIv2 plugins are a dynamic set hosted in driver domains. *)

open Stdext
open Threadext

(* We treat versions as '.'-separated integer lists under the usual
   lexicographic ordering. *)
type version = int list

let version_of_string s = List.map int_of_string (String.split_on_char '.' s)

module D = Debug.Make (struct let name = "xapi_sm" end)

open D

let create_from_query_result ~__context q =
  let r = Ref.make () and u = Uuid.string_of_uuid (Uuid.make_uuid ()) in
  let open Storage_interface in
  if String.lowercase_ascii q.driver <> "storage_access" then (
    let features = Smint.parse_string_int64_features q.features in
    let capabilities = List.map fst features in
    info "Registering SM plugin %s (version %s)"
      (String.lowercase_ascii q.driver)
      q.version ;
    Db.SM.create ~__context ~ref:r ~uuid:u
      ~_type:(String.lowercase_ascii q.driver)
      ~name_label:q.name ~name_description:q.description ~vendor:q.vendor
      ~copyright:q.copyright ~version:q.version
      ~required_api_version:q.required_api_version ~capabilities ~features
      ~configuration:q.configuration ~other_config:[]
      ~driver_filename:(Sm_exec.cmd_name q.driver)
      ~required_cluster_stack:q.required_cluster_stack
  )

let update_from_query_result ~__context (self, r) q_result =
  let open Storage_interface in
  let _type = String.lowercase_ascii q_result.driver in
  if _type <> "storage_access" then (
    let driver_filename = Sm_exec.cmd_name q_result.driver in
    let features = Smint.parse_string_int64_features q_result.features in
    let capabilities = List.map fst features in
    info "Registering SM plugin %s (version %s)"
      (String.lowercase_ascii q_result.driver)
      q_result.version ;
    if r.API.sM_type <> _type then
      Db.SM.set_type ~__context ~self ~value:_type ;
    if r.API.sM_name_label <> q_result.name then
      Db.SM.set_name_label ~__context ~self ~value:q_result.name ;
    if r.API.sM_name_description <> q_result.description then
      Db.SM.set_name_description ~__context ~self ~value:q_result.description ;
    if r.API.sM_vendor <> q_result.vendor then
      Db.SM.set_vendor ~__context ~self ~value:q_result.vendor ;
    if r.API.sM_copyright <> q_result.copyright then
      Db.SM.set_copyright ~__context ~self ~value:q_result.copyright ;
    if r.API.sM_required_api_version <> q_result.required_api_version then
      Db.SM.set_required_api_version ~__context ~self
        ~value:q_result.required_api_version ;
    if r.API.sM_capabilities <> capabilities || r.API.sM_features <> features
    then (
      Db.SM.set_capabilities ~__context ~self ~value:capabilities ;
      Db.SM.set_features ~__context ~self ~value:features
    ) ;
    if r.API.sM_configuration <> q_result.configuration then
      Db.SM.set_configuration ~__context ~self ~value:q_result.configuration ;
    if r.API.sM_driver_filename <> driver_filename then
      Db.SM.set_driver_filename ~__context ~self ~value:driver_filename
  )

let is_v1 x = version_of_string x < [2; 0]

let _serialize_reg =
  let lock = Mutex.create () in
  let holder = ref None in
  fun f ->
    match !holder with
    | Some t when t = Thread.self () ->
        (* inside a nested layer where the lock is held by myself *)
        f ()
    | _ ->
        Mutex.execute lock (fun () ->
            holder := Some (Thread.self ()) ;
            Pervasiveext.finally f (fun () -> holder := None))

let unregister_plugin ~__context q_result =
  _serialize_reg (fun () ->
      let open Storage_interface in
      let driver = String.lowercase_ascii q_result.driver in
      if is_v1 q_result.required_api_version then
        info "Not unregistering SM plugin %s (required_api_version %s < 2.0)"
          driver q_result.required_api_version
      else
        List.iter
          (fun (rf, rc) ->
            if rc.API.sM_type = driver then
              try
                info "Unregistering SM plugin %s (version %s)" driver
                  q_result.version ;
                Db.SM.destroy ~__context ~self:rf
              with e ->
                warn "Ignore unregistering SM plugin failure: %s"
                  (Printexc.to_string e))
          (Db.SM.get_all_records ~__context))

let register_plugin ~__context q_result =
  _serialize_reg (fun () ->
      let open Storage_interface in
      let driver = String.lowercase_ascii q_result.driver in
      if is_v1 q_result.required_api_version then
        info "Not registering SM plugin %s (required_api_version %s < 2.0)"
          driver q_result.required_api_version
      else (
        unregister_plugin ~__context q_result ;
        create_from_query_result ~__context q_result
      ))
