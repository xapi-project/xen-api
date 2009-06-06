(** Single Connection Management *)

(** {2 Types} *)

type t
(** Type of single connections *)

type watch
(** Type of watches. *)

(** {2 Basic constructors} *)

val watch_create : con:t -> path:string -> token:string -> watch
val create : Xb.t -> Domain.t option -> t
val close : t -> unit
val get_con : watch -> t

(** {8 Accessors}*)

val get_domain : t -> Domain.t option
val get_domstr : t -> string
val get_path : t -> string
val get_perm : t -> Perms.Connection.t
val get_fd : t -> Unix.file_descr

val dump : t -> out_channel -> unit

val restrict : t -> Xc.domid -> unit
val set_target : t -> Xc.domid -> unit

(** {8 Replies}*)

val send_reply : t -> int -> int -> Xb.Op.operation -> string -> unit
val send_error : t -> int -> int -> string -> unit
val send_ack : t -> int -> int -> Xb.Op.operation -> unit

val is_dom0 : t -> bool

(** {8 Watches}*)

val get_watches : t -> string -> watch list
val get_children_watches : t -> string -> watch list
val add_watch : t -> string -> string -> string * watch
val del_watch : t -> string -> string -> string * watch
val list_watches : t -> (string * string) list
val fire_single_watch : watch -> unit
val fire_watch : watch -> string -> unit
val debug : t -> string

(** {8 Transactions}*)

val start_transaction : t -> Store.t -> int
val end_transaction : t -> int -> bool -> bool
val get_transaction : t -> int -> Transaction.t

(** {8 Inputs}*)

val has_input : t -> bool
val do_input : t -> bool
val pop_in : t -> Xb.Packet.t
val has_more_input : t -> bool

(** {8 Outputs}*)

val has_output : t -> bool
val has_new_output : t -> bool
val peek_output : t -> Xb.Packet.t
val do_output : t -> bool
val incr_ops : t -> unit

(** {8 Statistics} *)

val stats : t -> int * int
val number_of_transactions : t -> int

(** {8 Garbage Collection of Symbols} *)
val mark_symbols : t -> unit
