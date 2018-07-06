open! Core_kernel
open! Import

module Deferred = Deferred1

(** The interface that must be satisfied by a sequence used to compare a
    monad sequence implementation against. *)
module type Comparison_sequence = sig
  type 'a t [@@deriving compare, sexp_of]

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b
  val fold  : 'a t -> init:'b -> f:(       'b -> 'a -> 'b) -> 'b

  val init       :  int -> f:(int       ->         'a) -> 'a t
  val iter       : 'a t -> f:(       'a ->       unit) -> unit
  val iteri      : 'a t -> f:(int -> 'a ->       unit) -> unit
  val map        : 'a t -> f:(       'a ->         'b) -> 'b t
  val mapi       : 'a t -> f:(int -> 'a ->         'b) -> 'b t
  val filter     : 'a t -> f:(       'a ->       bool) -> 'a t
  val filteri    : 'a t -> f:(int -> 'a ->       bool) -> 'a t
  val filter_map : 'a t -> f:(       'a  -> 'b option) -> 'b t
  val filter_mapi: 'a t -> f:(int -> 'a  -> 'b option) -> 'b t
  val concat_map : 'a t -> f:(       'a  -> 'b      t) -> 'b t
  val concat_mapi: 'a t -> f:(int -> 'a  -> 'b      t) -> 'b t

  val find     : 'a t -> f:(       'a -> bool     ) -> 'a option
  val findi    : 'a t -> f:(int -> 'a -> bool     ) -> (int * 'a) option
  val find_map : 'a t -> f:(       'a -> 'b option) -> 'b option
  val find_mapi: 'a t -> f:(int -> 'a -> 'b option) -> 'b option

  val exists  : 'a t -> f:(       'a -> bool     ) -> bool
  val existsi : 'a t -> f:(int -> 'a -> bool     ) -> bool
  val for_all : 'a t -> f:(       'a -> bool     ) -> bool
  val for_alli: 'a t -> f:(int -> 'a -> bool     ) -> bool
end

module Make
    (M : Comparison_sequence)
    (S : Deferred.Monad_sequence with type 'a t := 'a M.t)
  (*_ This signature is here to remind us to update the unit tests whenever we change
    [Monad_sequence.S]. *)
  : sig include Monad_sequence.S with type 'a monad := 'a Deferred.t end

