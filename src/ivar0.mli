(** Internal to Async -- see {!Ivar} for the public API. *)

open! Core_kernel
open! Import

type 'a t = 'a Types.Ivar.t [@@deriving sexp_of]
type 'a ivar = 'a t

include Invariant.S1 with type 'a t := 'a t

val create : unit -> _ t
val create_full : 'a -> 'a t

val peek      : 'a t -> 'a option
val value_exn : 'a t -> 'a
val value     : 'a t -> if_empty_then_failwith:string -> 'a

val is_empty : _ t -> bool
val is_full  : _ t -> bool

val equal : 'a t -> 'a t -> bool

val connect : bind_result:'a t -> bind_rhs:'a t -> unit
val fill : 'a t -> 'a  -> unit

module Handler : sig
  type 'a t [@@deriving sexp_of]
end

val add_handler : 'a t -> ('a -> unit) -> Execution_context.t -> 'a Handler.t
val remove_handler : 'a t -> 'a Handler.t -> unit
val has_handlers : _ t -> bool

val upon  : 'a t -> ('a -> unit) -> unit
val upon' : 'a t -> ('a -> unit) -> 'a Handler.t

val indir : 'a t -> 'a t
