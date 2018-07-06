(** A stack implemented with an array.

    The implementation will grow the array as necessary, and will never automatically
    shrink the array. One can use [set_capacity] to explicitly resize the array.
*)

open! Import

include Stack_intf.S (** @open *)

(** [capacity t] returns the length of the array backing [t]. *)
val capacity : _ t -> int

(** [set_capacity t capacity] sets the length of the array backing [t] to [max capacity
    (length t)].  To shrink as much as possible, do [set_capacity t 0]. *)
val set_capacity : _ t -> int -> unit
