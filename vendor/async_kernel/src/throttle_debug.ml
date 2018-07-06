open! Core_kernel
open! Import

module type Throttle = module type of Throttle

module Debug (Throttle : Throttle) : Throttle = struct

  module Debug = Core_kernel.Debug.Make ()
  let () = Debug.show_messages := false

  open Throttle

  module Deferred = Deferred

  let debug x = Debug.debug (T2.invariant ignore ignore) ~module_name:"Throttle" x

  module T2 = T2

  type nonrec 'a t = 'a t [@@deriving sexp_of]

  type nonrec 'a outcome = 'a outcome [@@deriving sexp_of]

  let capacity_available        = capacity_available
  let cleaned                   = cleaned
  let create                    = create
  let create_with               = create_with
  let invariant                 = invariant
  let is_dead                   = is_dead
  let max_concurrent_jobs       = max_concurrent_jobs
  let num_jobs_running          = num_jobs_running
  let num_jobs_waiting_to_start = num_jobs_waiting_to_start
  let prior_jobs_done           = prior_jobs_done

  let enqueue t f =
    debug "enqueue" [t] () [%sexp_of: unit] [%sexp_of: _ Deferred.t]
      (fun () -> enqueue t f)
  ;;

  let enqueue' t f =
    debug "enqueue'" [t] () [%sexp_of: unit] [%sexp_of: _ outcome Deferred.t]
      (fun () -> enqueue' t f)
  ;;

  let kill t =
    debug "kill" [t] () [%sexp_of: unit] [%sexp_of: unit]
      (fun () -> kill t)
  ;;

  let at_kill t f =
    debug "at_kill" [t] () [%sexp_of: unit] [%sexp_of: unit]
      (fun () -> at_kill t f)
  ;;

  let monad_sequence_how  = monad_sequence_how
  let monad_sequence_how2 = monad_sequence_how2

  module Sequencer = Sequencer
end
