(** A map of key-value pairs stored locally. Data stored here can still be
    accessed even when the master connection is down. Writes are immediately
    flushed to disk. *)

(** Thrown when a particular named key could not be found. *)
exception Missing_key of string

(** Retrieves a value *)
val get: string -> string

(** Inserts a value into the database, only returns when the insertion has
    been persisted. *)
val put: string -> string -> unit

(** Insert a set of values into the database, only returns when the insertions
    have been persisted. *)
val putv: (string * string) list -> unit

(** Delete a key from the local database *)
val del: string -> unit
