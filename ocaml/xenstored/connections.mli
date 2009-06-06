(** Connections Management *)

type t
(** The type of connections. *)

val create : unit -> t
(** Constuctor *)

(** {2 Accessors} *)

val find : t -> Unix.file_descr -> Connection.t
val select : t -> Unix.file_descr list * Unix.file_descr list
val iter : t -> (Connection.t -> unit) -> unit
val has_more_work : t -> Connection.t list

(** {2 Anonymous Connections} *)

val add_anonymous : t -> Unix.file_descr -> bool -> unit
val iter_anonymous : t -> (Connection.t -> unit) -> unit
val del_anonymous : t -> Connection.t -> unit

(** {2 Domain Connections} *)

val add_domain : t -> Domain.t -> unit
val find_domain : t -> int -> Connection.t
val iter_domains : t -> (Connection.t -> unit) -> unit
val del_domain : t -> int -> unit

(** {2 Watches Management} *)

val add_watch : t -> Connection.t -> string -> string -> Connection.watch
val del_watch : t -> Connection.t -> string -> string -> Connection.watch
val fire_watches : t -> Store.Path.t -> bool -> unit
val fire_spec_watches : t -> string -> unit

(** {2 Permissions Management} *)

val set_target : t -> int -> Xc.domid -> unit

(** {2 Statistics} *)

val number_of_transactions : t -> int
val stats : t -> int * int * int * int * int * int
val debug : t -> string
