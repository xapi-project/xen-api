(*
   Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

(** Report the state of the feature flag but ignore any restrictions.
    CP-40650: we remove any restriction to the feature *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let log_feature_flag ~__context =
  let pool = Helpers.get_pool ~__context in
  let restrictions = Db.Pool.get_restrictions ~__context ~self:pool in
  let feature = "restrict_vtpm" in
  match List.assoc_opt feature restrictions with
  | Some "false" ->
      info "%s: %s=false (unrestricted)" __FUNCTION__ feature
  | Some flag ->
      info "%s: %s=%s (ignored)" __FUNCTION__ feature flag
  | None ->
      info "%s: %s is undefined" __FUNCTION__ feature

(** The state in the xapi backend is only up-to-date when the VMs are halted *)
let assert_no_fencing ~__context ~persistence_backend =
  let pool = Helpers.get_pool ~__context in
  let ha_enabled = Db.Pool.get_ha_enabled ~__context ~self:pool in
  let clustering_enabled = Db.Cluster.get_all ~__context <> [] in
  let may_fence = ha_enabled || clustering_enabled in
  match (persistence_backend, may_fence) with
  | `xapi, true ->
      let message = "VTPM.create with HA or clustering enabled" in
      Helpers.maybe_raise_vtpm_unimplemented __FUNCTION__ message
  | _ ->
      ()

let assert_no_vtpm_associated ~__context vm =
  match Db.VM.get_VTPMs ~__context ~self:vm with
  | [] ->
      ()
  | vtpms ->
      let amount = List.length vtpms |> Int.to_string in
      raise Api_errors.(Server_error (vtpm_max_amount_reached, [amount]))

let introduce ~__context ~vM ~persistence_backend ~contents ~is_unique =
  let ref = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in
  let backend = Ref.null in
  Db.VTPM.create ~__context ~ref ~uuid ~vM ~backend ~persistence_backend
    ~is_unique ~is_protected:false ~contents ;
  ref

(** Contents from unique vtpms cannot be copied! *)
let get_contents ~__context ?from () =
  let create () = Xapi_secret.create ~__context ~value:"" ~other_config:[] in
  let copy ref =
    let contents = Db.VTPM.get_contents ~__context ~self:ref in
    Xapi_secret.copy ~__context ~secret:contents
  in
  let maybe_copy ref =
    if Db.VTPM.get_is_unique ~__context ~self:ref then
      create ()
    else
      copy ref
  in
  Option.fold ~none:(create ()) ~some:maybe_copy from

let create ~__context ~vM ~is_unique =
  let persistence_backend = `xapi in
  log_feature_flag ~__context ;
  assert_no_fencing ~__context ~persistence_backend ;
  assert_no_vtpm_associated ~__context vM ;
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vM
    ~expected:`Halted ;
  let contents = get_contents ~__context () in
  introduce ~__context ~vM ~persistence_backend ~contents ~is_unique

let copy ~__context ~vM ref =
  let vtpm = Db.VTPM.get_record ~__context ~self:ref in
  let persistence_backend = vtpm.vTPM_persistence_backend in
  let is_unique = vtpm.vTPM_is_unique in
  let contents = get_contents ~__context ~from:ref () in
  introduce ~__context ~vM ~persistence_backend ~contents ~is_unique

let destroy ~__context ~self =
  let vm = Db.VTPM.get_VM ~__context ~self in
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vm
    ~expected:`Halted ;
  let secret = Db.VTPM.get_contents ~__context ~self in
  Db.Secret.destroy ~__context ~self:secret ;
  Db.VTPM.destroy ~__context ~self

let get_contents ~__context ~self =
  let secret = Db.VTPM.get_contents ~__context ~self in
  Db.Secret.get_value ~__context ~self:secret

let set_contents ~__context ~self ~contents =
  let previous_secret = Db.VTPM.get_contents ~__context ~self in
  let _ =
    (* verify contents to be already base64-encoded *)
    try Base64.decode contents
    with Invalid_argument err ->
      raise Api_errors.(Server_error (internal_error, [err]))
  in
  let secret = Xapi_secret.create ~__context ~value:contents ~other_config:[] in
  Db.VTPM.set_contents ~__context ~self ~value:secret ;
  Db.Secret.destroy ~__context ~self:previous_secret
