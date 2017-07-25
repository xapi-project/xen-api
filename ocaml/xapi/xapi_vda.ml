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

(* TODO - fail with proper API errors? *)
let assert_vm_is_valid ~__context ~vm =
  if vm = Ref.null
  then failwith "VM reference is null."

(* TODO - fail with proper API errors? *)
let assert_vm_has_no_vda ~__context ~vm =
  match Db.VDA.get_records_where ~__context
    ~expr:(Db_filter_types.(Eq (Field "vm", Literal (Ref.string_of vm)))) with
  | (_,record)::_ -> failwith ("VDA already exists: " ^ record.API.vDA_uuid)
  | _ -> ()

(* TODO - fail with proper API errors? *)
let assert_vda_version_is_valid ~__context ~version =
  (* TODO - validate version format? *)
  if version = ""
  then failwith "VDA version cannot be empty."

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
  "I'm sure it's fine..."

let get_log_report ~__context ~self =
  "DEBUG 01.01.1972 00:00:001 - beep\nDEBUG 01.01.1972 00:00:005 - boop\n"
