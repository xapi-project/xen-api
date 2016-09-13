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

(** Operations for transforming and validating memory constraints. *)
module type T = sig

  (** Represents a set of memory constraints for a guest. Constraints
      	  * are in valid order if (and only if) they satisfy the following:
      	  * static_min <= dynamic_min <= dynamic_max <= static_max
      	  *)
  type t =
    {
      static_min  : Int64.t;
      dynamic_min : Int64.t;
      target      : Int64.t;
      dynamic_max : Int64.t;
      static_max  : Int64.t;
    }

  (** Given a set of constraints [c], returns [true] if and only if
      	    [c.dynamic_min] = [c.dynamic_max]. *)
  val are_pinned : constraints:t -> bool

  (** Given a set of constraints [c], returns [true] if and only if
      	    [c.dynamic_min] = [c.dynamic_max] = [c.static-max]. *)
  val are_pinned_at_static_max : constraints:t -> bool

  (** Given a set of constraints [c], returns [true] if and only if
      	    [c.static_min] ≤ [c.dynamic_min] ≤ [c.dynamic_max] ≤ [c.static_max]. *)
  val are_valid : constraints:t -> bool

  (** Given a set of constraints [c], returns [true] if and only if
      	    [c.static_min] ≤ [c.dynamic_min] = [c.dynamic_max] = [c.static-max]. *)
  val are_valid_and_pinned_at_static_max : constraints:t -> bool

  (** Creates a set of memory constraints from the given tuple whose
      	  * elements appear in order of increasing size.
      	  *)
  val create : (int64 * int64 * int64 * int64 * int64) -> t

  (** Transforms the given set of memory constraints into a valid set, if
      	  * possible, or else returns None. Constraints returned by this function
      	  * are guaranteed to be in valid order such that:
      	  *
      	  * static_min <= dynamic_min <= target <= dynamic_max <= static_max
      	  *
      	  * If the given constraints are valid, this function simply returns a copy
      	  * of those constraints.
      	  *
      	  * If the given constraints are invalid, but can be made valid by adjusting
      	  * [(dynamic_min, dynamic_max)] to be in the range defined by [static_min,
      	  * static_max], or by adjusting [target] to be within the range defined by
      	  * [(dynamic_min, dynamic_max)], this function returns such a modified set
      	  * of constraints.
      	  *
      	  * If the given constraints are invalid and they cannot be made valid by
      	  * modifying the dynamic constraints, this function function returns None.
      	  *)
  val transform : constraints:t -> t option

  (** Takes the given set of possibly-invalid memory constraints {i s}, and
      	  * returns a new set of valid and unballooned constraints {i t} s.t.:
      	  * {ol
      	  * {- t.dynamic_max := s.static_max}
      	  * {- t.target      := s.static_max}
      	  * {- t.dynamic_min := s.static_max}
      	  * {- t.static_min  := minimum (s.static_min, s.static_max)}}
      	  *)
  val reset_to_safe_defaults : constraints:t -> t

end

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( ** ) = Int64.mul
let ( // ) = Int64.div

module Vm_memory_constraints : T = struct

  type t =
    {
      static_min  : Int64.t;
      dynamic_min : Int64.t;
      target      : Int64.t;
      dynamic_max : Int64.t;
      static_max  : Int64.t;
    }

  let create (static_min, dynamic_min, target, dynamic_max, static_max) =
    {
      static_min  = static_min;
      dynamic_min = dynamic_min;
      target      = target;
      dynamic_max = dynamic_max;
      static_max  = static_max;
    }

  let transform ~constraints:c =
    (* Constrains a value between two limits. *)
    let constrain value minimum maximum =
      if value < minimum then minimum else
      if value > maximum then maximum else value in
    (* Fail if either maximum is less than its corresponding minimum. *)
    if c.static_max < c.static_min then None else
    if c.dynamic_max < c.dynamic_min then None else
      (* Ensure dynamic constraints are within static constraints. *)
      let dynamic_min = constrain c.dynamic_min c.static_min c.static_max in
      let dynamic_max = constrain c.dynamic_max c.static_min c.static_max in
      (* Ensure target is within dynamic constraints. *)
      let target = constrain c.target dynamic_min dynamic_max in
      Some {c with
            dynamic_min = dynamic_min;
            target      = target;
            dynamic_max = dynamic_max;
           }

  let are_pinned ~constraints =
    constraints.dynamic_min = constraints.dynamic_max

  let are_pinned_at_static_max ~constraints = true
                                              && constraints.dynamic_max = constraints.static_max
                                              && are_pinned constraints

  let are_valid ~constraints = true
                               && constraints.static_min <= constraints.dynamic_min
                               && constraints.dynamic_min <= constraints.dynamic_max
                               && constraints.dynamic_max <= constraints.static_max

  let are_valid_and_pinned_at_static_max ~constraints = true
                                                        && constraints.static_min <= constraints.dynamic_min
                                                        && are_pinned_at_static_max constraints

  let reset_to_safe_defaults ~constraints =
    let max = constraints.static_max in
    let min = constraints.static_min in
    {
      static_max  = max;
      dynamic_max = max;
      target      = max;
      dynamic_min = max;
      static_min  = if min < max then min else max
    }

end
