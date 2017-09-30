(** Provides helpers required for testing functions that use the
    event mechanism. *)

val event_setup_common : unit -> Context.t * [`session] Ref.t
(** Creates a new test environment suitable for testing functions
    interacting with xapi's event mechanism.
    Calls {!Test_common.make_test_database} to setup an initial
    test database. *)
