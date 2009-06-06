(* High-level xenstore watch functions *)

(* Examples:
     let port = Watch.wait_for ~xs (Watch.value_to_appear "/local/domain/1/vnc-port") in ...

     ignore(Watch.wait_for ~xs (Watch.all_of [ Watch.value_to_appear ".../shutdown-done"; 
                                               Watch.value_to_appear "../shutdown-done" ]))
*)

type path = string

(** A result to wait for *)
type 'a t

val map: ('a -> 'b) -> 'a t -> 'b t

(** Represents a value appearing in the store *)
val value_to_appear : path -> string t
(** Represents a key being deleted *)
val key_to_disappear : path -> unit t
(** Represents a particular value appearing at a particular key *)
val value_to_become : path -> string -> unit t

(** Wait for all results *)
val all_of : 'a t list -> 'a list t
(** Wait for any of a set of possible results *)
val any_of : ('a * 'b t) list -> ('a * 'b) t

exception Timeout of float

(** Wait for a result *)
val wait_for : xs:Xs.xsh -> ?timeout:float -> 'a t -> 'a

