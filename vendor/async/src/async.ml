(** {2 Async_kernel} *)

include Async_kernel (** @open *)

(** {2 Async_unix} *)

include Async_unix (** @open *)

(** {2 Async_extra} *)

include Async_extra (** @open *)

let%test "Async library initialization does not initialize the scheduler" =
  Scheduler.is_ready_to_initialize ()
;;

module Expect_test_config
  : Expect_test_config.S with type 'a IO.t = 'a Deferred.t =
struct
  module IO = Deferred

  let flush () = return ()

  let run f = Thread_safe.block_on_async_exn f

  let flushed () = true

  let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
end
