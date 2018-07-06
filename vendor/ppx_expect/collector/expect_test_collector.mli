open Expect_test_common.Std

module Test_outcome : sig
  type t =
    { file_digest             : File.Digest.t
    ; location                : File.Location.t
    ; expectations            : Expectation.Raw.t list
    ; saved_output            : (File.Location.t * string) list
    ; trailing_output         : string
    ; upon_unreleasable_issue : Expect_test_config.Upon_unreleasable_issue.t
    }
end

module Make(Config : Expect_test_config.S) : sig

  module Instance : sig
    type t

    (** Collect the output that has been run since the last call to [save_output], or
        since the current expect-test started running.

        This function should only be called while a test is running. It is meant to be
        called as a result of ppx_expect translating an expect-test, and is not intended
        to be called manually. *)
    val save_output : t -> File.Location.t -> unit Config.IO.t
  end

  (** Run an expect-test *)
  val run
    :  file_digest        : File.Digest.t
    -> location           : File.Location.t
    -> absolute_filename  : string
    -> description        : string option
    -> tags               : string list
    -> expectations       : Expectation.Raw.t list
    -> inline_test_config : Ppx_inline_test_lib.Runtime.config
    -> (Instance.t -> unit Config.IO.t)
    -> unit
end

(** The tests that ran, in the order they ran *)
val tests_run : unit -> Test_outcome.t list

module Current_file : sig
  val set : absolute_filename:string -> unit
  val unset : unit -> unit
end
