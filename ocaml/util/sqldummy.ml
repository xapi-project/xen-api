(** Dummy SQL interface *)

let do_select sql db = []

let do_insert table keys_and_values db = ()
let begin_transaction db = ()
let end_transaction db = ()

(** Runs a side-effecting query *)
let run_query sql params db = ()

(** Opens the database, executes a query and closes it again. NB must
    commit any open transactions before returning and allowing the database to
    close. *)
let open_database f = f ()

(** create empty database from schema *)
let create_empty_db () = ()
