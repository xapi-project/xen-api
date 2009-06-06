(** Produce a string name -> string mean, standard deviation summary for each population *)
val summarise : unit -> (string * string) list

(** Time the given function and attribute the result to the named population *)
val time_this : string -> (unit -> unit) -> unit

type dbcallty = Read | Write | Create | Drop
val log_db_call : string option -> string -> dbcallty -> unit
val summarise_db_calls : unit -> (string list * string list * string list * string list * (string * ((string * string) list)) list * (int * ((string * string) list)) list)

val log_stats : bool ref
