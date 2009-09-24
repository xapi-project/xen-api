(** Permission management *)

(** There are two kinds of permissions: the first one is related to nodes and the second one is related to connections. 
    Basically, nodes and connections have an owner and are associated to read/write permissions. *)

val activate : bool ref
(** If [activate] is true, then xenstored will check the permissions on each opeation it is necessary. *)

(** The type of access permissions. *)
type permty = READ | WRITE | RDWR | NONE

(** {6 Node Permissions} *)

module Node :
sig
	type t
	(** The type of node permissions *)

	val default0 : t
	(** Default permission for nodes. These are owned by dom0 and accessible by no other domains. *)

	val get_owner : t -> Xc.domid
	(** [get_owner p] gets the owner of node permission [p]. *)

	val of_strings : string list -> t
	(** [of_string l] converts a list of strings into a node permission. These string have the following format: "rX", "wX", "bX" or "nX",
	    where "X" is an int representing a domain id and the letter 'r' meaning read access, 'w' meaning write acces, 'b'
	    meaning read/write access and 'n' means no access at all. The node permission is built as follows:
-         if "cX" is the head of [l], then "X" is the permission owner and any domains which does not appear in [l] have access permission "c" to the current node; and
-         if "cX" is in the tail of [l], then domain "X" has the access permisson "c" to the current node. *)

	val of_string : string -> t
	(** Convert a string into a node permission. The string must be obtained by concatening a list of strings having the same format as in [of_strings],
	    with the character '\000' inserted between each string. Moreover, the string MUST ends by '\000'. *)

	val to_string : t -> string
	(** Convert a node permission into a string. *)
end


(** {6 Connection Permissions} *)

module Connection :
sig
	type t
	(** The type of connection permissions. *)

	val full_rights : t
	(** A connection having the permission [full_right] can read and modify any part of the store. *)

	val create : ?perms:permty list -> Xc.domid -> t
	(** [create ~p id] creates a connection permission having [id] as owner and [p] as access/modification rights on the store. *)

	val set_target : t -> ?perms:permty list -> Xc.domid -> t
	(** [set_target conperm ~p id] sets the owner of [conperm] as an owner of all of the node of domain [id]. That call doesn't change the existing ownership.
	    Moreover, any further call of [set_target] to [conperm] will fail. *)

	val restrict : t -> Xc.domid -> t
	(** [restrict conperm id] can be applied only on file descriptor connections which have normally full rights and are owned by dom0. After the call,
	    the owner of [conperm] is set to [id]. *)

    val is_dom0 : t -> bool
	(** [is_dom0 conperm] checks if domain 0 is the owner of conperm. *)

	val to_string : t -> string
  end

(** {6 Permission Checks} *)

val check_owner : Connection.t -> Node.t -> bool
(** [check_owner c n] checks that connection [p] has the same owner as node [n] *)

val check : Connection.t -> permty -> Node.t -> unit
(** [check c p n] checks if the connection [c] can access the node [n] with access permission [p]. If not, this function raises {!Define.Permission_denied}. *)

val equiv : Node.t -> Node.t -> bool
(** [equiv n1 n2] checks if the nodes [n1] and [n2] have the same permissions *)
