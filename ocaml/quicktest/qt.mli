(** Common types and helpers used by the various quicktests. *)

type rpc = Rpc.call -> Rpc.response

(** This is the information we collect for each SR for selecting and running
    the storage quicktests. *)
type sr_info =
  { sr: API.ref_SR
  ; allowed_operations: API.storage_operations_set
  ; capabilities: string list
  ; required_sm_api_version : string
  }

val init_session : rpc -> string -> string -> API.ref_session

val get_pool : rpc -> API.ref_session -> API.ref_pool

val http : Http.Request.t -> (Http.Response.t * Unix.file_descr -> 'a) -> 'a

val cli_cmd : string list -> string

module Test : sig
  val assert_raises_match : (exn -> bool) -> (unit -> 'a) -> unit
  (** This test succeeds if an exception is raised for which the given
      exception matching function returns true *)
end

module Time : sig
  type t
  (** A time value that can be compared against the dates stored in XenAPI
      objects' fields *)

  val now : unit -> t
  val of_field : Xapi_stdext_date.Date.iso8601 -> t
  val pp : t -> string
  val check : t -> after:t -> before:t -> unit
end

module VM : sig
  module Template : sig
    val other : string
    val find : rpc -> API.ref_session -> string -> API.ref_VM option
    (** Returns the first template, if any, whose name starts with the given string *)
  end

  val with_new : rpc -> API.ref_session -> template:API.ref_VM -> (API.ref_VM -> 'a) -> 'a

  val dom0_of_host : rpc -> API.ref_session -> API.ref_host -> API.ref_VM
  (** Return a host's domain zero *)

  val get_dom0 : rpc -> API.ref_session -> API.ref_VM
  (** Return the localhost's domain zero *)
end

module VDI : sig

  val with_destroyed : rpc -> API.ref_session -> API.ref_VDI -> (unit -> 'a) -> 'a
  (** Makes sure the given VDI is destroyed after the function processed it. *)

  val with_new : rpc -> API.ref_session -> ?virtual_size:int64 -> API.ref_SR -> (API.ref_VDI -> 'a) -> 'a
  (** Runs the given function with a new temporary VDI *)

  val with_any : rpc -> API.ref_session -> sr_info -> (API.ref_VDI -> 'a) -> 'a
  (** Runs the given function with a new temporary VDI if
      VDI.create is supported, otherwise it will pass an existing one to the
      function. *)

  val with_attached : rpc -> API.ref_session -> API.ref_VDI -> [`RO|`RW] -> (string -> 'a) -> 'a
  (** Attaches the VDI to dom0 and passes the block device path to the given
      function *)

  val with_open : rpc -> API.ref_session -> API.ref_VDI -> [`RO|`RW] -> (Unix.file_descr -> 'a) -> 'a
  (** Attaches the VDI to dom0, opens the block device, and passes the file
      descriptor to the given function *)

  val check_fields : ([`Same | `Different] * string * (API.vDI_t -> string)) list -> API.vDI_t -> API.vDI_t -> unit
  (** Verify that the fields of the two VDIs changed as expected *)

  val test_update : rpc -> API.ref_session -> API.ref_VDI -> unit
  (** Calls VDI.update and checks that the VDI fields that must be the same are
      the same before and after the update. *)
end

module SR : sig

  val check_fields : ([`Same | `Different] * string * (API.sR_t -> string)) list -> API.sR_t -> API.sR_t -> unit
  (** Verify that the fields of the two VDIs changed as expected *)

  val test_update : rpc -> API.ref_session -> API.ref_SR -> unit
  (** Calls VDI.update and checks that the VDI fields that must be the same are
      the same before and after the update. *)
end
