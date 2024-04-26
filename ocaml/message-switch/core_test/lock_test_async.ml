open Core
open Async
open Message_switch_async

let ( >>= ) = Deferred.( >>= )

let test_async_lock () = Protocol_async.Mtest.mutex_provides_mutal_exclusion ()

let () =
  don't_wait_for
    (test_async_lock () >>= fun () -> shutdown 0 ; Deferred.return ())

let () = never_returns (Scheduler.go ())
