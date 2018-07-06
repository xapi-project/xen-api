open Import

val care_about_ite_branch : bool ref
(** If set to [true] then the check regarding if-then-else branches is
    performed. *)

val about_ite_branch_ref : (Location.t -> unit) ref
(** Used to override what happens for problematic locations.
    By default just prints an error message on stderr. *)

val about_ite_branch : Location.t -> unit
(** This function is called by the parser when it wants to warn about an
    if-then-else branch. *)
