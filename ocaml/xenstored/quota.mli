(** Quota Management *)

(** {2 Exceptions} *)

exception Limit_reached
exception Data_too_big
exception Transaction_opened

(** {2 Global Variables} *)

val activate : bool ref
(** If [Quota.activate] is set to true, then the permission management is activated. *)

val maxent : int ref
(** [Quota.maxent] is the default value for the maximal number of nodes a domain can own. *)

val maxsize : int ref
(** [Quota.maxsize] is the default value for the maximal size of values stored into the tree. *)

(** {2 Quota} *)

type t
(** The type of quota. *)

val create : unit -> t
(** Creates a new quota which allows domains to own at most [Quota.maxent] nodes and which allows values
	of size at most [Quota.maxsize]. *)

val copy : t -> t
(** Copy a quota. That function creates a fresh object. *)

val check : t -> Xc.domid -> int -> unit
(** if [Quota.activate] is set to true, then [Quota.check quota dom size] checks if domain [dom] can create
	a new node with a value of size [size]. *)

val del_entry : t -> Xc.domid -> unit
(** [Quota.del_entry quota dom] decreases by one the entry associated to [dom] in [quota]. *)

val add_entry : t -> Xc.domid -> unit
(** [Quota.add_entry quota dom] increases by one the entry associated to [dom] in [quota]. *)

val add : t -> t -> unit
(** [Quota.add q1 q2] merges the entry associated with each domains in quota [q1] and [q2]. *)

val debug : t -> string
(** [Quota.debug quota] exports the quota table as a string. *)
