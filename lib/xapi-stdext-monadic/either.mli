(* Inspired by Haskell's Either, as a way to enhance option with
   information about what went wrong.

   Right is commonly used for success
   Left is commonly used for failure.
*)

type ('a,'b) t = Left of 'a | Right of 'b
module Monad : sig include Monad.M2.MONAD with type ('a, 'b) m = ('b, 'a) t end

val left : 'a -> ('a, 'b) t
val right: 'b -> ('a, 'b) t
val is_left: ('a, 'b) t -> bool
val is_right: ('a, 'b) t -> bool

val cat_right: ('a, 'b) t list -> 'b list
(* Brings Right values closer to the surface. *)
val join: ('a, ('b, 'c) t) t -> (('a, 'b) t, 'c) t

val swap : ('a, 'b) t -> ('b, 'a) t
val of_exception : (unit -> 'a) -> (exn, 'a) t
