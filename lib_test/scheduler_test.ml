open OUnit

let _ = Scheduler.start ()

let test_delay () =
  let open Scheduler.Delay in
  let x = make () in
  let before = Unix.gettimeofday () in
  wait x 0.5;
  let after = Unix.gettimeofday () in
  let elapsed = after -. before in
  Printf.printf "elapsed: %f\n" elapsed;
  assert_bool "elapsed_time1" (elapsed < 0.6);
  assert_bool "elapsed_time2" (elapsed > 0.4)

let test_delay_cancel () =
  let open Scheduler.Delay in
  let x = make () in
  let before = Unix.gettimeofday () in
  let th = Thread.create (fun () -> wait x 0.5) () in
  signal x;
  Thread.join th;
  let after = Unix.gettimeofday () in
  let elapsed = after -. before in
  Printf.printf "elapsed: %f\n" elapsed;
  assert_bool "elapsed_time1" (elapsed < 0.4)

let test_one_shot () =
  Printf.printf "Test one shot\n%!";
  let after = ref None in
  let before = Unix.gettimeofday () in
  Printf.printf "About to one_shot\n%!";
  let x = Scheduler.one_shot (Delta 10) "test_one_shot" (fun () -> Printf.printf "In the one_shot function\n%!"; after := Some (Unix.gettimeofday ())) in
  let dump = Scheduler.Dump.make () in
  Printf.printf "Dumping:\n%!";
  List.iter (fun i -> Printf.printf "time: %Ld thing: %s\n%!" i.Scheduler.Dump.time i.Scheduler.Dump.thing) dump;
  Thread.delay 20.0;
  Printf.printf "Delayed\n%!";
  let success =
    match !after with
    | Some x ->
      Printf.printf "Got it: %f\n%!" x;
      let elapsed = x -. before in elapsed > 0.99 && elapsed < 2.01
    | None ->
      Printf.printf "Nothing...\n%!"; false
  in
  assert_bool "one_shot_success" success

let test_one_shot_cancel () =
  let after = ref None in
  let before = Unix.gettimeofday () in
  let x = Scheduler.one_shot (Delta 1) "test_one_shot_cancel" (fun () -> after := Some (Unix.gettimeofday ())) in
  Scheduler.cancel x;
  Thread.delay 2.0;
  let success =
    match !after with
    | Some _ -> false
    | None -> true
  in
  assert_bool "one_shot_cancelled" success

let tests =
  "scheduler" >:::
    [
      "Test Delay" >:: test_delay;
      "Test Delay cancellation" >:: test_delay_cancel;
      "Test One shot" >:: test_one_shot;
(*      "Test One shot cancellation" >:: test_one_shot_cancel; *)
    ]
