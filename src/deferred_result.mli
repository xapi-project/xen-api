open! Core_kernel

include Monad.S2 with type ('a, 'b) t = ('a, 'b) Result.t Deferred1.t (** @open *)

val ignore : (_, 'err) t -> (unit, 'err) t

val map_error : ('ok, 'error1) t -> f:('error1 -> 'error2) -> ('ok, 'error2) t

(** [combine] waits on both inputs and combines their results using [Result.combine]. *)
val combine
  :  ('ok1, 'err) t
  -> ('ok2, 'err) t
  -> ok  : ('ok1 -> 'ok2 -> 'ok3)
  -> err : ('err -> 'err -> 'err)
  -> ('ok3, 'err) t
