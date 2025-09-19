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

open Xapi_stdext_std.Xstringext

module D = Debug.Make (struct let name = "xapi_vusb_helpers" end)

(**************************************************************************************)
(* current/allowed operations checking                                                *)

open Record_util

let all_ops : API.vusb_operations_set = [`attach; `plug; `unplug]

type table = (API.vusb_operations, (string * string list) option) Hashtbl.t

let valid_operations ~__context record _ref' : table =
  let _ref = Ref.string_of _ref' in
  let current_ops = record.Db_actions.vUSB_current_operations in
  (* Policy:
   * one operation at a time
   * a running VM can do plug depending on whether the VUSB is already attached to VM.
   * a running VM can do unplug depending on whether the VUSB is already attached to VM.
   *
   *)
  let table : table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_ops ;
  let set_errors (code : string) (params : string list)
      (ops : API.vusb_operations_set) =
    List.iter
      (fun op ->
        if Hashtbl.find table op = None then
          Hashtbl.replace table op (Some (code, params))
      )
      ops
  in
  (* Any current_operations preclude everything else *)
  ( if current_ops <> [] then
      let concurrent_op_refs, concurrent_op_types =
        List.fold_left
          (fun (refs, types) (ref, op) ->
            (ref :: refs, vusb_operations_to_string op :: types)
          )
          ([], []) current_ops
      in
      let format x = Printf.sprintf "{%s}" (String.concat "; " x) in
      let concurrent_op_refs = format concurrent_op_refs in
      let concurrent_op_types = format concurrent_op_types in
      set_errors Api_errors.other_operation_in_progress
        ["VUSB"; _ref; concurrent_op_types; concurrent_op_refs]
        all_ops
  ) ;
  let vm = Db.VUSB.get_VM ~__context ~self:_ref' in
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  ( match (power_state, record.Db_actions.vUSB_currently_attached) with
  | `Running, true ->
      set_errors Api_errors.device_already_attached [_ref] [`plug]
  | `Running, false ->
      set_errors Api_errors.device_already_detached [_ref] [`unplug]
  | _, _ ->
      let actual = Record_util.vm_power_state_to_lowercase_string power_state in
      let expected = Record_util.vm_power_state_to_lowercase_string `Running in
      set_errors Api_errors.vm_bad_power_state
        [Ref.string_of vm; expected; actual]
        [`plug; `unplug]
  ) ;
  let vm_current_ops = Db.VM.get_current_operations ~__context ~self:vm in
  List.iter
    (fun (_, op) ->
      if List.mem op [`clean_shutdown; `hard_shutdown; `suspend; `pause] then
        let current_op_str =
          "Current operation on VM:"
          ^ Ref.string_of vm
          ^ " is "
          ^ Record_util.vm_operation_to_string op
        in
        set_errors Api_errors.operation_not_allowed [current_op_str]
          [`plug; `unplug]
    )
    vm_current_ops ;
  table

let throw_error (table : table) op =
  match Hashtbl.find_opt table op with
  | None ->
      Helpers.internal_error
        "xapi_vusb_helpers.assert_operation_valid unknown operation: %s"
        (vusb_operations_to_string op)
  | Some (Some (code, params)) ->
      raise (Api_errors.Server_error (code, params))
  | Some None ->
      ()

let update_allowed_operations ~__context ~self : unit =
  let all = Db.VUSB.get_record_internal ~__context ~self in
  let valid = valid_operations ~__context all self in
  let keys =
    Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid []
  in
  Db.VUSB.set_allowed_operations ~__context ~self ~value:keys

let assert_operation_valid ~__context ~self ~(op : API.vusb_operations) =
  let all = Db.VUSB.get_record_internal ~__context ~self in
  let table = valid_operations ~__context all self in
  throw_error table op

let clear_current_operations ~__context ~self =
  if Db.VUSB.get_current_operations ~__context ~self <> [] then (
    Db.VUSB.set_current_operations ~__context ~self ~value:[] ;
    update_allowed_operations ~__context ~self
  )
