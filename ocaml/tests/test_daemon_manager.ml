(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type stop_failure = {
  error: exn;
  (** The exception thrown when trying to stop the daemon. *)
  time_until_stopped: float;
  (** The mock daemon will be marked as not running [t] seconds after the
      	    exception is thrown. *)
}

module Mock_daemon = struct
  let running = ref true

  let stop_failure : stop_failure option ref = ref None

  let times_called_start = ref 0
  let times_called_stop = ref 0

  let reset ~is_running =
    running := is_running;
    stop_failure := None;
    times_called_start := 0;
    times_called_stop := 0

  let check = Daemon_manager.Function (fun () -> !running)

  let start () =
    incr times_called_start;
    running := true

  let stop () =
    incr times_called_stop;
    match !stop_failure with
    | Some {error; time_until_stopped} -> begin
        (* Raise the exception after spawning a thread which will set running to
           			   false after a specified time. *)
        let (_: Thread.t) =
          Thread.create
            (fun () ->
               Thread.delay time_until_stopped;
               running := false)
            ()
        in
        raise error
      end
    | None ->
      running := false
end

module Mock_manager = Daemon_manager.Make(Mock_daemon)

let check_times_called ~start ~stop =
  Alcotest.(check int) "times_called_start" !Mock_daemon.times_called_start start;
  Alcotest.(check int) "time_until_stopped" !Mock_daemon.times_called_stop stop

(* Test that the daemon is restarted, and that the return value of the function
   passed to with_daemon_stopped is propagated. *)
let test_basic_operation () =
  Mock_daemon.reset ~is_running:true;
  let result = Mock_manager.with_daemon_stopped (fun () -> 123) in
  Alcotest.(check int) "result" result 123;
  check_times_called ~start:1 ~stop:1

(* Two sequential calls to with_daemon_stopped should restart the daemon
   twice. *)
let test_two_restarts () =
  Mock_daemon.reset ~is_running:true;
  Mock_manager.with_daemon_stopped (fun () -> ());
  Mock_manager.with_daemon_stopped (fun () -> ());
  check_times_called ~start:2 ~stop:2

(* Test that if the daemon is stopped, calling with_daemon_stopped does not
   attempt to stop or start it. *)
let test_already_stopped () =
  Mock_daemon.reset ~is_running:false;
  let result = Mock_manager.with_daemon_stopped (fun () -> 123) in
  Alcotest.(check int) "result" result 123;
  check_times_called ~start:0 ~stop:0

let test_exception () =
  Mock_daemon.reset ~is_running:true;
  Alcotest.check_raises "exception is propagated by with_daemon_stopped"
    (Failure "fail")
    (fun () -> Mock_manager.with_daemon_stopped (fun () -> failwith "fail"));
  check_times_called ~start:1 ~stop:1

let spawn_threads_and_wait task count =
  let rec spawn_threads task count acc =
    if count > 0 then begin
      let thread = Thread.create task () in
      spawn_threads task (count - 1) (thread :: acc)
    end
    else acc
  in
  spawn_threads task count []
  |> List.iter Thread.join

(* Run with_daemon_stopped multiple times in parallel. The daemon should only
   be restarted once. *)
let test_threads () =
  Mock_daemon.reset ~is_running:true;
  let delay_thread () =
    Mock_manager.with_daemon_stopped (fun () -> Thread.delay 5.0)
  in
  spawn_threads_and_wait delay_thread 5;
  check_times_called ~start:1 ~stop:1

(* The daemon initially fails to stop, but it stops within the timeout. *)
let test_timeout_succeed () =
  Mock_daemon.reset ~is_running:true;
  Mock_daemon.stop_failure := Some {
      error = Failure "stop failed";
      time_until_stopped = 2.0;
    };
  Mock_manager.with_daemon_stopped ~timeout:5.0 (fun () -> ());
  check_times_called ~start:1 ~stop:1

(* The daemon does not stop within the timeout, so the exception is raised. *)
let test_timeout_fail () =
  Mock_daemon.reset ~is_running:true;
  Mock_daemon.stop_failure := Some {
      error = Failure "stop failed";
      time_until_stopped = 5.0;
    };
  Alcotest.check_raises "does not stop within timeout"
    (Failure "stop failed")
    (fun () -> Mock_manager.with_daemon_stopped ~timeout:2.0 (fun () -> ()));
  check_times_called ~start:0 ~stop:1

let test =
  [
    "test_basic_operation", `Quick, test_basic_operation;
    "test_two_restarts", `Quick, test_two_restarts;
    "test_already_stopped", `Quick, test_already_stopped;
    "test_exception", `Quick, test_exception;
    (* "test_threads", `Slow, test_threads; CA-316165: Thread.delay in test *)
    "test_timeout_succeed", `Slow, test_timeout_succeed;
    "test_timeout_fail", `Slow, test_timeout_fail;
  ]
