(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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

(* Checks to see if an operation is valid in this state. Returns Some exception *)
(* if not and None if everything is ok. *)
let check_operation_error ~__context record self op =
  let _ref = Ref.string_of self in
  let current_ops = record.Db_actions.vM_appliance_current_operations in
  (* Only allow one operation of [`start | `clean_shutdown | `hard_shutdown | `shutdown ] at a time. *)
  if List.length current_ops > 0 then
    Some (Api_errors.other_operation_in_progress, ["VM_appliance"; _ref])
  else
    let vms = Db.VM_appliance.get_VMs ~__context ~self in
    if List.length vms = 0 then
      Some (Api_errors.operation_not_allowed, ["Appliance has no VMs."])
    else begin
      (* Allow the op if any VMs are in a state where the op makes sense. *)
      let power_states = List.map (fun vm -> Db.VM.get_power_state ~__context ~self:vm) vms in
      let predicate, error = match op with
        (* Can start if any are halted. *)
        | `start ->
          (fun power_state -> power_state = `Halted), "There are no halted VMs in this appliance."
        (* Can clean_shutdown if any are running. *)
        | `clean_shutdown ->
          (fun power_state -> power_state = `Running), "There are no running VMs in this appliance."
        (* Can hard_shutdown/shutdown if any are not halted. *)
        | `hard_shutdown | `shutdown ->
          (fun power_state -> power_state <> `Halted), "All VMs in this appliance are halted."
      in
      if List.exists predicate power_states then
        None
      else
        Some (Api_errors.operation_not_allowed, [error])
    end

let assert_operation_valid ~__context ~self ~(op:API.vm_appliance_operation) =
  let all = Db.VM_appliance.get_record_internal ~__context ~self in
  match check_operation_error ~__context all self op with
  | None -> ()
  | Some (a,b) -> raise (Api_errors.Server_error (a,b))

let update_allowed_operations ~__context ~self =
  let all = Db.VM_appliance.get_record_internal ~__context ~self in
  let allowed_ops =
    let allowed x = match check_operation_error ~__context all self x with None -> true | _ -> false in
    List.filter allowed [`start; `clean_shutdown; `hard_shutdown; `shutdown] in
  Db.VM_appliance.set_allowed_operations ~__context ~self ~value:allowed_ops
