(** Represents an idempotent background operation which needs to be re-executed when any of a number of external
    factors/ dependencies change. We attempt to minimise the number of times this background function is called. *)
type operation = unit -> unit

(** Instances of this type manage the background execution of the operation *)
type manager

(** Return a human-readable name for this background operation for debugging purposes *)
val name_of_t: manager -> string

(** Create a manager instance *)
val make: string -> operation -> manager

(** Signal the manager that some external factor has changed and therefore the background operation needs re-execution *)
val again: manager -> unit
