(** A radix tree mapping strings to ['a]. *)
type 'a t

val empty : 'a t
(** An empty tree *)

(** [Duplicate_key key] is thrown by [insert] if [key]
    	already exists in the tree. *)
exception Duplicate_key of string

val insert : string -> 'a -> 'a t -> 'a t
(** [insert key value tree] returns a new tree with the
    mapping [key] to [value] *)

val longest_prefix : string -> 'a t -> 'a option
(** [longest_prefix key tree] finds the key [k] which shares
    the longest prefix with [key] and returns the associated
    value. *)

val longest_prefix_with_boundary : string -> char -> 'a t -> 'a option
(** [longest_prefix_with_boundary key boundary tree] returns the value associated
    with the entry whose key shares the longest prefix with [key], such that the
    next character after the prefix (if any) matches the given [boundary]. *)

val fold : (string -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
(** [fold f initial t] folds [f] over all bindings in [t] *)

val is_prefix : string -> string -> bool
(** [is_prefix a b] returns true if [a] is a prefix of [b] *)
