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
 * @group Virtual-Machine Management
*)

(** An extension of Vm_memory_constraints that provides additional database and API operations. *)
module type T = sig
  include Vm_memory_constraints.T

  val assert_valid : constraints:t -> unit
  (** Asserts for the given set of constraints [c], that
      	    [c.static_min] ≤ [c.dynamic_min] ≤ [c.dynamic_max] ≤ [c.static_max]. *)

  val assert_valid_and_pinned_at_static_max :
    constraints:t -> reason:string -> unit
  (** Asserts for the given set of constraints [c], that
      	    [c.static_min] ≤ [c.dynamic_min] = [c.dynamic_max] = [c.static_max]. *)

  val assert_valid_for_current_context :
    __context:Context.t -> vm:[`VM] Ref.t -> constraints:t -> unit
  (** Asserts that the given set of constraints [c] is valid for the current
      	    context. *)

  val extract : vm_record:API.vM_t -> t
  (** Extracts memory constraints from the given VM record. *)

  val get : __context:Context.t -> vm_ref:[`VM] Ref.t -> t
  (** Reads memory constraints for the given VM, from the database. *)

  val get_live : __context:Context.t -> vm_ref:[`VM] Ref.t -> t
  (** Reads memory constraints effective for the given running VM, from the database. *)

  val set : __context:Context.t -> vm_ref:[`VM] Ref.t -> constraints:t -> unit
  (** Writes memory constraints for the given VM, to the database. *)
end

module Vm_memory_constraints : T = struct
  include Vm_memory_constraints.Vm_memory_constraints

  let nested_virt ~__context vm =
    let metrics = Db.VM.get_metrics ~__context ~self:vm in
    Xapi_vm_lifecycle.nested_virt ~__context vm metrics

  let assert_valid ~constraints =
    if not (are_valid ~constraints) then
      raise
        (Api_errors.Server_error
           (Api_errors.memory_constraint_violation_order, [])
        )

  let assert_valid_and_pinned_at_static_max ~constraints ~reason =
    if not (are_valid_and_pinned_at_static_max ~constraints) then
      raise
        (Api_errors.Server_error
           (Api_errors.memory_constraint_violation_maxpin, [reason])
        )

  let assert_valid_for_current_context ~__context ~vm ~constraints =
    let is_control_domain = Db.VM.get_is_control_domain ~__context ~self:vm in
    if not (Pool_features.is_enabled ~__context Features.DMC) then
      assert_valid_and_pinned_at_static_max ~constraints
        ~reason:"DMC unavailable"
    else if is_control_domain then
      assert_valid_and_pinned_at_static_max ~constraints
        ~reason:"control domain"
    else if nested_virt ~__context vm then
      assert_valid_and_pinned_at_static_max ~constraints ~reason:"nested virt"
    else
      assert_valid ~constraints

  let extract ~vm_record =
    {
      static_min= vm_record.API.vM_memory_static_min
    ; dynamic_min= vm_record.API.vM_memory_dynamic_min
    ; target= vm_record.API.vM_memory_target
    ; dynamic_max= vm_record.API.vM_memory_dynamic_max
    ; static_max= vm_record.API.vM_memory_static_max
    }

  let get ~__context ~vm_ref =
    let vm_record = Db.VM.get_record ~__context ~self:vm_ref in
    extract vm_record

  let get_live ~__context ~vm_ref =
    let vm_record = Db.VM.get_record ~__context ~self:vm_ref in
    {
      static_min= vm_record.API.vM_memory_static_min
    ; dynamic_min= vm_record.API.vM_memory_dynamic_min
    ; target= vm_record.API.vM_memory_target
    ; dynamic_max= vm_record.API.vM_memory_dynamic_max
    ; static_max= vm_record.API.vM_memory_static_max
    }

  let set ~__context ~vm_ref ~constraints =
    Db.VM.set_memory_static_min ~__context ~self:vm_ref
      ~value:constraints.static_min ;
    Db.VM.set_memory_dynamic_min ~__context ~self:vm_ref
      ~value:constraints.dynamic_min ;
    Db.VM.set_memory_target ~__context ~self:vm_ref ~value:constraints.target ;
    Db.VM.set_memory_dynamic_max ~__context ~self:vm_ref
      ~value:constraints.dynamic_max ;
    Db.VM.set_memory_static_max ~__context ~self:vm_ref
      ~value:constraints.static_max
end
