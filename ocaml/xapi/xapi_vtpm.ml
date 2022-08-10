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

let assert_no_vtpm_associated ~__context vm =
  match Db.VM.get_VTPMs ~__context ~self:vm with
  | [] ->
      ()
  | vtpms ->
      let amount = List.length vtpms |> Int.to_string in
      raise Api_errors.(Server_error (vtpm_max_amount_reached, [amount]))

let choose_sr ~__context ~vM ~sR =
  if sR <> Ref.null then
    sR
  else
    let pool = Helpers.get_pool ~__context in
    let sr = Db.Pool.get_suspend_image_SR ~__context ~self:pool in
    if sr <> Ref.null then
      sr
    else
      let snapshot = Db.VM.get_record ~__context ~self:vM in
      let hosts =
        Xapi_vm_helpers.get_possible_hosts_for_vm ~__context ~vm:vM ~snapshot
      in
      match
        List.filter_map
          (fun host ->
            let sr = Db.Host.get_suspend_image_sr ~__context ~self:host in
            if sr <> Ref.null then Some sr else None
          )
          hosts
      with
      | sr :: _ ->
          sr
      | _ ->
          Xapi_vm_helpers.list_required_SRs ~__context ~self:vM |> List.hd
(* TODO: perhaps chose SR with largest amount of free space? *)

let nv_memory_size = (128 * 1024) + (65 * 704) (* NV_MEMORY_SIZE from libtpms *)

let create ~__context ~vM ~is_unique ~sR =
  assert_no_vtpm_associated ~__context vM ;
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vM
    ~expected:`Halted ;
  let ref = Ref.make () in
  let uuid = Uuid.(to_string (make ())) in
  let backend = Ref.null in
  let persistence_backend = `vdi in
  let vm_uuid = Db.VM.get_uuid ~__context ~self:vM in
  let name_label = "VTPM state for " ^ vm_uuid in
  let vDI =
    Xapi_vdi.create ~__context ~_type:`vtpm_state
      ~sR:(choose_sr ~__context ~vM ~sR)
      ~name_label ~name_description:name_label ~sharable:false ~read_only:false
      ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]
      ~virtual_size:(Int64.of_int nv_memory_size)
  in
  Db.VTPM.create ~__context ~ref ~uuid ~vM ~backend ~persistence_backend
    ~is_unique ~is_protected:false ~vDI ~contents:Ref.null ;
  ref

let destroy ~__context ~self =
  let record = Db.VTPM.get_record ~__context ~self in
  let vm = record.vTPM_VM in
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vm
    ~expected:`Halted ;
  let () =
    match record.vTPM_persistence_backend with
    | `xapi ->
        let secret = Db.VTPM.get_contents ~__context ~self in
        Db.Secret.destroy ~__context ~self:secret
    | `vdi ->
        Xapi_vdi.destroy ~__context ~self:record.vTPM_VDI
  in
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
