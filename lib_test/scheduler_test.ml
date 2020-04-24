let assert_bool msg = Alcotest.(check bool) msg true

let global_scheduler = Scheduler.make ()

(* Tests the basic delay functionality. *)
let test_delay () =
  let open Scheduler.Delay in
  let x = make () in
  let before = Unix.gettimeofday () in
  ignore (wait x 0.5) ;
  let after = Unix.gettimeofday () in
  let elapsed = after -. before in
  assert_bool "elapsed_time1" (elapsed < 0.6) ;
  assert_bool "elapsed_time2" (elapsed > 0.4)

(* Tests that 'wait' can be cancelled *)
let test_delay_cancel () =
  let open Scheduler.Delay in
  let x = make () in
  let before = Unix.gettimeofday () in
  let th = Thread.create (fun () -> wait x 0.5) () in
  signal x ;
  Thread.join th ;
  let after = Unix.gettimeofday () in
  let elapsed = after -. before in
  assert_bool "elapsed_time1" (elapsed < 0.4)

let timed_wait_callback ~msg ?(time_min = 0.) ?(eps = 0.1) ?(time_max = 60.) f =
  let rd, wr = Unix.pipe () in
  let finally () = Unix.close rd ; Unix.close wr in
  Fun.protect ~finally (fun () ->
      let before = Unix.gettimeofday () in
      let after = ref None in
      let callback () =
        after := Some (Unix.gettimeofday ()) ;
        let (_ : int) = Unix.write_substring wr " " 0 1 in
        ()
      in
      f callback ;
      let ready = Thread.wait_timed_read rd time_max in
      match (ready, !after) with
      | true, None ->
          Alcotest.fail "pipe ready to read, but after is not set"
      | false, None ->
          Alcotest.fail
            (Printf.sprintf "%s: callback not invoked within %gs" msg time_max)
      | _, Some t ->
          let actual_minimum = min (t -. before) time_min in
          Alcotest.(check (float eps))
            (Printf.sprintf "%s: callback invoked earlier than expected" msg)
            time_min actual_minimum)

(* Test the injection of a one-shot function at a time in the future *)
let test_one_shot () =
  timed_wait_callback ~msg:"one_shot_success" ~time_min:1.0 (fun callback ->
      ignore
      @@ Scheduler.one_shot global_scheduler (Scheduler.Delta 1) "test_one_shot"
           callback)

(* Test the injection of a one-shot function at an absolute time *)
let test_one_shot_abs () =
  timed_wait_callback ~msg:"one_shot_abs_success" ~time_min:1.0 (fun callback ->
      let now = Scheduler.now () in
      ignore
      @@ Scheduler.one_shot global_scheduler
           (Scheduler.Absolute (Int64.add 1L now))
           "test_one_shot" callback)

(* Tests that the scheduler still works even after a failure occurs in the
   injected function *)
let test_one_shot_failure () =
  timed_wait_callback ~msg:"one_show_failure" ~time_min:1.0 (fun callback ->
      let _ =
        Scheduler.one_shot global_scheduler (Scheduler.Delta 0) "test_one_shot"
          (fun () -> failwith "Error")
      in
      ignore
      @@ Scheduler.one_shot global_scheduler (Scheduler.Delta 1) "test_one_shot"
           callback)

(* Checks that one-shot functions can cancelled and are then not executed *)
let test_one_shot_cancel () =
  let after = ref None in
  let x =
    Scheduler.one_shot global_scheduler (Scheduler.Delta 1)
      "test_one_shot_cancel" (fun () -> after := Some (Unix.gettimeofday ()))
  in
  Scheduler.cancel global_scheduler x ;
  Thread.delay 2.0 ;
  let success = match !after with Some _ -> false | None -> true in
  assert_bool "one_shot_cancelled" success

(* Check that dumping the state of the scheduler contains a reference to a test
   function that has been injected *)
let test_dump () =
  let after = ref None in
  let _before = Unix.gettimeofday () in
  let _ =
    Scheduler.one_shot global_scheduler (Scheduler.Delta 1) "test_dump"
      (fun () -> after := Some (Unix.gettimeofday ()))
  in
  let dump = Scheduler.Dump.make global_scheduler in
  assert_bool "dump_contains_item"
    (List.exists (fun x -> x.Scheduler.Dump.thing = "test_dump") dump)

let tests =
  [
    ("Test Delay", `Slow, test_delay)
  ; ("Test Delay cancellation", `Quick, test_delay_cancel)
  ; ("Test One shot", `Slow, test_one_shot)
  ; ("Test One shot absolute", `Slow, test_one_shot_abs)
  ; ("Test One shot failure", `Slow, test_one_shot_failure)
  ; ("Test One shot cancellation", `Slow, test_one_shot_cancel)
  ; ("Test dump", `Quick, test_dump)
  ]
