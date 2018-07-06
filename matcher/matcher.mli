open Base
open Expect_test_common.Std

module Saved_output : sig
  type t

  val of_nonempty_list_exn : string list -> t
end

module Test_outcome : sig
  (** Outcome of a group of test. Either a single [let%expect_test], or a whole file for
      toplevel expect test. *)
  type t =
    { expectations            : Fmt.t Cst.t Expectation.t Map.M(File.Location).t
    ; saved_output            : Saved_output.t Map.M(File.Location).t
    ; trailing_output         : Saved_output.t
    ; upon_unreleasable_issue : Expect_test_config.Upon_unreleasable_issue.t
    }

  (* Merge two [t]s with the same expectations *)
  val merge_exn : t -> t -> t
end

module Test_correction : sig
  (** Correction for one [Test_outcome.t] *)
  type t

  val map_corrections : t -> f:(Fmt.t Cst.t -> Fmt.t Cst.t) -> t

  (** Single node correction *)
  type node_correction =
    | Collector_never_triggered
    | Correction of Fmt.t Cst.t Expectation.Body.t

  val make
    :  location        : File.Location.t
    -> corrections     : (Fmt.t Cst.t Expectation.t * node_correction) list
    -> trailing_output : Fmt.t Cst.t Expectation.Body.t Reconcile.Result.t
    -> t Reconcile.Result.t
end

(** Evaluate the results of all the tests run through Expect_test_runner. *)
val evaluate_test
  :  file_contents:string
  -> location:File.Location.t
  -> allow_output_patterns:bool
  -> Test_outcome.t
  -> Test_correction.t Reconcile.Result.t

type mode = Inline_expect_test | Toplevel_expect_test

(** Write a list of correction to a file. *)
val write_corrected
  :  file:string
  -> file_contents:string
  -> mode:mode
  -> Test_correction.t list
  -> unit
