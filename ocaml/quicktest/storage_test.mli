(** Module to help write test cases that run on a given SR. The idea is that
    one [test_case] is mapped to multiple Alcotest test cases - one for each SR
    the storage test should run on. Which SRs a storage test can run on is
    defined by filters of type [Sr_filter.t] that can be combined. *)

(** This is the information we collect for each SR for selecting and running
    the storage quicktests. *)
type sr_info =
  { sr: API.ref_SR
  ; allowed_operations: API.storage_operations_set
  ; capabilities: string list
  }

module VDI : sig

  (** Runs the given function with a new temporary VDI if
      VDI.create is supported, otherwise it will pass an existing one to the
      function. *)
  val with_any : API.ref_session -> sr_info -> (API.ref_VDI -> 'a) -> 'a

  (** Makes sure the given VDI is destroyed after the function processed it. *)
  val with_destroyed : API.ref_session -> API.ref_VDI -> (unit -> 'a) -> 'a

  (** Runs the given function with a new temporary VDI *)
  val with_new : API.ref_session -> ?virtual_size:int64 -> API.ref_SR -> (API.ref_VDI -> 'a) -> 'a

  (** Verify that the fields of the two VDIs changed as expected *)
  val check_fields : ([`Same | `Different] * string * (API.vDI_t -> string)) list -> API.vDI_t -> API.vDI_t -> unit

  (** Calls VDI.update and checks that the VDI fields that must be the same are
      the same before and after the update. *)
  val test_update : API.ref_session -> API.ref_VDI -> unit
end

module Sr_filter : sig
  type t
  (** A filter that defines which SRs a storage test can run on. It takes a list
      of SRs as input and returns another list of SRs that the test should run
      on. *)

  val all : t

  val random : t

  val not_iso : t

  val only_sr : API.ref_SR -> t

  val default_sr : t

  val with_smallest_disk_size : t

  (** Selects SRs that either have a VDI or we can create & destroy a VDI on them.
      This filter should be called from tests using [VDI.with_any] *)
  val with_any_vdi : t

  (** Selects SRs that can be unplugged *)
  val can_unplug : t

  val allowed_operations : API.storage_operations_set -> t

  val has_capabilities : string list -> t

  (** Filters out SRs of the given type *)
  val not_type : string -> t

  (** [a ||> b] creates a new filter by chaining a and b together. First [a]
      will filter the SRs, and then [b] will filter the output of a.
      This operator is left associative, so [a ||> b ||> c] will filter the SRs
      with [a], then [b], then [c]. *)
  val (||>) : t -> t -> t

  val filter : (Rpc.call -> Rpc.response) -> API.ref_session -> t -> sr_info list -> sr_info list
end

(** A storage test case is analogous to a [Alcotest.test_case]: is a tuple of a
    test name, test speed, test function taking a session ID and an SR, and a
    filter specifying which SRs support the test *)
type test_case = string * Alcotest.speed_level * (API.ref_session -> sr_info -> unit) * Sr_filter.t

(** Return a [sr_info] list of all SRs which have been selected by the
    specified filter and have at least one plugged-in PBD, ie those which we
    can use for stuff. If the corresponding CLI flag is present, only the
    default SR will be considered instead of filtering all SRs. *)
val list_srs : API.ref_session -> Sr_filter.t -> sr_info list

(** Returns an [Alcotest.test_case] for all the possible (storage test, SR)
    combinations. By default, all the plugged SRs will be considered. If the
    corresponding CLI argument is present, only the default SR will be used. *)
val get_test_cases : API.ref_session -> test_case list -> unit Alcotest.test_case list
