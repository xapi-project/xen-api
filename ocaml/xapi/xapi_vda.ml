(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Make(struct let name="xapi_vda" end)
open D

let find_vda ~__context ~vm =
  match Db.VDA.get_refs_where ~__context
    ~expr:(Db_filter_types.(Eq (Field "vm", Literal (Ref.string_of vm)))) with
  | ref::[] -> Some ref
  | ref::_ -> (* should never happen; this occurance indicates a bug *)
    let msg = "Multiple VDAs found for VM" in
    error "%s %s" msg (Db.VM.get_uuid ~__context ~self:vm);
    raise Api_errors.(Server_error(internal_error, [msg; (Ref.string_of vm)]))
  | _ -> None

let assert_vm_is_valid ~__context ~vm =
  if vm = Ref.null
  then
    let msg = "VM reference is null" in
    error "%s" msg;
    raise Api_errors.(Server_error(internal_error, [msg]))

let assert_vm_has_no_vda ~__context ~vm =
  match Db.VDA.get_records_where ~__context
    ~expr:(Db_filter_types.(Eq (Field "vm", Literal (Ref.string_of vm)))) with
  | (_,record)::_ ->
    let msg = "VDA already exists" in
    error "%s (%s)" msg record.API.vDA_uuid;
    raise Api_errors.(Server_error(internal_error, [msg; record.API.vDA_uuid]))
  | _ -> ()

let assert_vda_version_is_valid ~__context ~version =
  if version = ""
  then
    let msg = "VDA version cannot be empty" in
    error "%s" msg;
    raise Api_errors.(Server_error(internal_error, [msg]))

(** Assert that the VDA's VM is in a certain power state *)
let assert_vm_power_state_is ~__context ~self ~expected =
  let vm = Db.VDA.get_vm ~__context ~self in
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vm ~expected

let create ~__context ~vm ~version =
  assert_vm_is_valid ~__context ~vm;
  assert_vm_has_no_vda ~__context ~vm;
  assert_vda_version_is_valid ~__context ~version;
  let vda = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.VDA.create ~__context ~ref:vda ~uuid ~vm ~version;
  vda

let destroy ~__context ~self =
  Db.VDA.destroy ~__context ~self

let get_status ~__context ~self =
  assert_vm_power_state_is ~__context ~self ~expected:`Running;
  "I'm sure it's fine..."

let get_log_report ~__context ~self =
  assert_vm_power_state_is ~__context ~self ~expected:`Running;
  "DEBUG 01.01.1972 00:00:001 - beep\nDEBUG 01.01.1972 00:00:005 - boop\n"

let copy ~__context ~vm vda =
  let record = Db.VDA.get_record ~__context ~self:vda in
  create ~__context ~vm ~version:record.API.vDA_version
