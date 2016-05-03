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

open OUnit

module Mock_daemon = struct
	let running = ref true

	let times_called_start = ref 0
	let times_called_stop = ref 0

	let reset ~is_running =
		running := is_running;
		times_called_start := 0;
		times_called_stop := 0

	let check = Daemon_manager.Function (fun () -> !running)

	let start () =
		incr times_called_start;
		running := true

	let stop () =
		incr times_called_stop;
		running := false
end

module Mock_manager = Daemon_manager.Make(Mock_daemon)

(* Test that the daemon is restarted, and that the return value of the function
   passed to with_daemon_stopped is propagated. *)
let test_basic_operation () =
	Mock_daemon.reset ~is_running:true;
	let result = Mock_manager.with_daemon_stopped (fun () -> 123) in
	assert_equal result 123;
	assert_equal !Mock_daemon.times_called_start 1;
	assert_equal !Mock_daemon.times_called_stop 1

(* Two sequential calls to with_daemon_stopped should restart the daemon
   twice. *)
let test_two_restarts () =
	Mock_daemon.reset ~is_running:true;
	Mock_manager.with_daemon_stopped (fun () -> ());
	Mock_manager.with_daemon_stopped (fun () -> ());
	assert_equal !Mock_daemon.times_called_start 2;
	assert_equal !Mock_daemon.times_called_stop 2

(* Test that if the daemon is stopped, calling with_daemon_stopped does not
   attempt to stop or start it. *)
let test_already_stopped () =
	Mock_daemon.reset ~is_running:false;
	let result = Mock_manager.with_daemon_stopped (fun () -> 123) in
	assert_equal result 123;
	assert_equal !Mock_daemon.times_called_start 0;
	assert_equal !Mock_daemon.times_called_stop 0

(* Test that an exception is propagated by with_daemon_stopped. *)
let test_exception () =
	Mock_daemon.reset ~is_running:true;
	assert_raises (Failure "fail")
		(fun () -> Mock_manager.with_daemon_stopped (fun () -> failwith "fail"));
	assert_equal !Mock_daemon.times_called_start 1;
	assert_equal !Mock_daemon.times_called_stop 1

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
	assert_equal !Mock_daemon.times_called_start 1;
	assert_equal !Mock_daemon.times_called_stop 1

let test =
	"daemon_manager" >:::
		[
			"test_basic_operation" >:: test_basic_operation;
			"test_two_restarts" >:: test_two_restarts;
			"test_already_stopped" >:: test_already_stopped;
			"test_exception" >:: test_exception;
			"test_threads" >:: test_threads;
		]
