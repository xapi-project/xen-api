(** This module provides a small framework for selecting the inputs of each
    test case using filters.

    These quicktest filters map a set of Alcotest test cases to another.
    What these filters usually do is select a set of objects that can be passed
    to the test function as the next input it's expecting, and then return a
    new set of tests specialized for those inputs.

    For example, the [sr] filter, which itself is a small domain-specific
    language, allows specifying which SRs a storage test should be run on, and
    it maps each storage test to a set of tests, one for each SR it supports.

    Filters can be chained together using the [|>] operator, to specify each
    input of a test case in turn, including the connection details - the
    session reference and the RPC function.

    The arguments of the quicktest executable have to be processed first before
    these filters are used. *)

val wrap : (unit -> unit) -> unit
(** This has to wrap the quicktest run *)

type 'a test_case = string * Alcotest.speed_level * 'a
(** A slightly different definition of Alcotest.test_case, to ensure we can
    reason about the entire type of the test function *)

type ('a, 'b) filter = 'a test_case list -> 'b test_case list
(** A filter takes a list of tests and returns a new list of transformed test cases *)

val conn : (Qt.rpc -> [ `session ] Ref.t -> 'c, 'c) filter
(** Provides connection details: passes the rpc and session_id to the test *)

module SR : sig
  (** Filters to help write test cases that run on a given SR. The idea is that
      one [test_case] is mapped to multiple test cases, one for each SR
      the storage test should run on. Which SRs a storage test can run on is
      defined by listing the supported SRs using the below combinators. *)

  type srs

  val all : srs
  (** All connected SRs, or only the default if the corresponding CLI option is
      enabled. The output of this can be piped with [|>] into the below filters
      to refine the set of SRs. *)

  val random : srs -> srs
  val not_iso : srs -> srs

  val with_any_vdi : srs -> srs
  (** Selects SRs that either have a VDI or we can create & destroy a VDI on them.
      This filter should be called from tests using [VDI.with_any] *)

  val can_unplug : srs -> srs
  (** Selects SRs that can be unplugged *)

  val allowed_operations : API.storage_operations_set -> srs -> srs
  val has_capabilities : string list -> srs -> srs

  val has_type : string -> srs -> srs
  (** Selects SRs of the given type *)

  val not_type : string -> srs -> srs
  (** Filters out SRs of the given type *)

  val smapiv1 : srs -> srs
  (** Selects SMAPIv1 SRs *)

  val smapiv3 : srs -> srs
  (** Selects SMAPIv3 SRs *)

  val thin_pro : srs -> srs
  (** Selects thinly-provisioned SRs *)

  val list_srs : srs -> Qt.sr_info list
end

val sr : SR.srs -> (Qt.sr_info -> 'b, 'b) filter

val vm_template : string -> (API.ref_VM -> 'b, 'b) filter
