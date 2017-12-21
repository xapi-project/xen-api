(** A radix tree mapping strings to ['a]. *)
type 'a t

(** An empty tree *)
val empty : 'a t

(** [Duplicate_key key] is thrown by [insert] if [key]
    	already exists in the tree. *)
exception Duplicate_key of string

(** [insert key value tree] returns a new tree with the
    mapping [key] to [value] *)
val insert : string -> 'a -> 'a t -> 'a t

(** [longest_prefix key tree] finds the key [k] which shares
    the longest prefix with [key] and returns the associated
    value. *)
val longest_prefix : string -> 'a t -> 'a option

(** [fold f initial t] folds [f] over all bindings in [t] *)
val fold : (string -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b

(** [is_prefix a b] returns true if [a] is a prefix of [b] *)
val is_prefix: string -> string -> bool
