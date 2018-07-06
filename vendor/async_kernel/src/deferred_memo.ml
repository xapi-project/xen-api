open Core_kernel
open Deferred_std

module Deferred = Deferred1

let reraise = function
  | Ok x -> x
  | Error exn -> Exn.reraise exn "caught exception in memoized function"
;;

let general (type a) (hashable : (module Hashable with type t = a)) f =
  let module Hashable = (val hashable) in
  let f =
    Memo.general ~hashable:Hashable.hashable (fun a ->
      Monitor.try_with ~run:`Now (fun () -> f a))
  in
  fun a -> f a >>| reraise
;;

let unit f =
  let f = Memo.unit (fun () -> Monitor.try_with ~run:`Now f) in
  fun () -> f () >>| reraise
;;

let%test_module _ =
  (module struct
    let test memo ~should_raise =
      let num_calls_to_f = ref 0 in
      let f =
        memo (fun () ->
          incr num_calls_to_f;
          (* We bind here so that exceptions are raised asynchronously.  It is in this case
             that functions from [Core_kernel.Memo] behave badly and fail this test. *)
          let%bind () = return () in
          if should_raise then (raise_s [%message "boom!"]) else (return 7))
      in
      let a = Monitor.try_with (fun () -> f ()) in
      let b = Monitor.try_with (fun () -> f ()) in
      Scheduler.run_cycles_until_no_jobs_remain ();
      [%test_result: int] !num_calls_to_f ~expect:1;
      let is_correct =
        if should_raise
        then (function Some (Error _) -> true | _ -> false)
        else (function Some (Ok 7)    -> true | _ -> false)
      in
      [%test_pred: (int, exn) Result.t option] is_correct (Deferred.peek a);
      [%test_pred: (int, exn) Result.t option] is_correct (Deferred.peek b);
    ;;

    let general = general (module Unit)

    let%test_unit _ = test general ~should_raise:false
    let%test_unit _ = test unit    ~should_raise:false
    let%test_unit _ = test general ~should_raise:true
    let%test_unit _ = test unit    ~should_raise:true
  end)
