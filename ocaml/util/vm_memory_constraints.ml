module type T = sig

	(** Represents a set of memory constraints for a virtual machine. Constraints  *)
	(** are in valid order if (and only if) they satisfy the following inequality: *)
	(** static_min <= dynamic_min <= target <= dynamic_max <= static_max           *)
	type t =
	{
		static_min  : Int64.t;
		dynamic_min : Int64.t;
		target      : Int64.t;
		dynamic_max : Int64.t;
		static_max  : Int64.t;
	}

	(** Transforms the given set of memory constraints into a valid set,  *)
	(** if possible, or else returns None. Any constraints returned by    *)
	(** this function are guaranteed to be in valid order such that:      *)
	(** static_min <= dynamic_min <= target <= dynamic_max <= static_max  *)
	(**                                                                   *)
	(**  1. If the given constraints are valid, this function returns a   *)
	(**     copy of those constraints.                                    *)
	(**                                                                   *)
	(**  2. If the given constraints are invalid, but can be made valid   *)
	(**     by adjusting {dynamic_min, dynamic_max} to be in the range    *)
	(**     defined by [static_min, static_max], or by adjusting {target} *)
	(**     to be within the range defined by [dynamic_min, dynamic_max], *)
	(**     this function returns such a modified set of constraints.     *)
	(**                                                                   *)
	(**  3. If the given constraints are invalid and they cannot be made  *)
	(**     valid by modifying the dynamic constraints, this function     *)
	(**     function returns None.                                        *)
	(**                                                                   *)
	val transform : constraints:t -> t option

	(** Returns true if and only if the given memory constraints are in  *)
	(** valid order such that:                                           *)
	(** static_min <= dynamic_min <= target <= dynamic_max <= static_max *)
	val valid : constraints:t -> bool

end

module Vm_memory_constraints : T = struct

	type t =
	{
		static_min  : Int64.t;
		dynamic_min : Int64.t;
		target      : Int64.t;
		dynamic_max : Int64.t;
		static_max  : Int64.t;
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

	let valid ~constraints =
		Listext.List.is_sorted compare [
			constraints.static_min;
			constraints.dynamic_min;
			constraints.target;
			constraints.dynamic_max;
			constraints.static_max
		]

end