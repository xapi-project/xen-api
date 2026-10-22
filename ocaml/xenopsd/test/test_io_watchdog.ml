(*
 * Copyright (c) Cloud Software Group, Inc.
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

(* Run [script] under sh, with stdout going nowhere, and wait on it under the
   watchdog. The abort kills it, as emu-manager exits when told to. *)
let watch ?(timeout = 1.) ?(after = fun () -> ()) script =
  let null = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
  let pid =
    Fun.protect
      ~finally:(fun () -> Unix.close null)
      (fun () ->
        Unix.create_process "sh" [|"sh"; "-c"; script|] Unix.stdin null
          Unix.stderr
      )
  in
  let aborted = ref false in
  let abort () =
    aborted := true ;
    Unix.kill pid Sys.sigkill
  in
  let result =
    try
      Ok
        (Io_watchdog.with_watchdog ~name:"test" ~pid ~timeout ~abort (fun () ->
             let _, status = Unix.waitpid [] pid in
             after () ; status
         )
        )
    with e -> Error e
  in
  (!aborted, result)

(* Writes a line every 0.2s for 3s: three times the timeout overall, but never
   quiet for long *)
let slow_writer =
  "for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do echo x; sleep 0.2; done"

let test_idle_is_aborted () =
  let aborted, result = watch "sleep 30" in
  Alcotest.(check bool) "aborted" true aborted ;
  match result with
  | Ok (Unix.WSIGNALED _) ->
      ()
  | _ ->
      Alcotest.fail "expected the process to be killed by the abort"

let test_slow_writer_is_not_aborted () =
  let aborted, result = watch slow_writer in
  Alcotest.(check bool) "aborted" false aborted ;
  match result with
  | Ok (Unix.WEXITED 0) ->
      ()
  | _ ->
      Alcotest.fail "expected the process to finish by itself"

(* The shell only waits; the work happens in a child, as with emu-manager and
   xenguest *)
let test_child_activity_counts () =
  let aborted, _ = watch (Printf.sprintf "(%s); exit 0" slow_writer) in
  Alcotest.(check bool) "aborted" false aborted

let test_failure_after_abort_is_reported () =
  let _, result = watch ~after:(fun () -> raise End_of_file) "sleep 30" in
  match result with
  | Error Xenops_interface.(Xenopsd_error (Errors.Internal_error msg)) ->
      Alcotest.(check string)
        "message" "test made no progress for 1s and was aborted" msg
  | Error e ->
      Alcotest.failf "unexpected exception %s" (Printexc.to_string e)
  | Ok _ ->
      Alcotest.fail "expected an exception"

let test_returns_promptly () =
  let started = Unix.gettimeofday () in
  let aborted, _ = watch ~timeout:600. "exit 0" in
  let elapsed = Unix.gettimeofday () -. started in
  Alcotest.(check bool) "aborted" false aborted ;
  if elapsed > 0.5 then
    Alcotest.failf "took %.2fs to return; it should not wait for the watchdog"
      elapsed

let test_no_abort_after_return () =
  let pid =
    Unix.create_process "sleep" [|"sleep"; "30"|] Unix.stdin Unix.stdout
      Unix.stderr
  in
  let aborted = ref false in
  Io_watchdog.with_watchdog ~name:"test" ~pid ~timeout:0.1
    ~abort:(fun () -> aborted := true)
    (fun () -> ()) ;
  Thread.delay 0.5 ;
  Unix.kill pid Sys.sigkill ;
  ignore (Unix.waitpid [] pid) ;
  Alcotest.(check bool) "aborted" false !aborted

let tests =
  [
    ("idle process is aborted", `Quick, test_idle_is_aborted)
  ; ("slow writer is not aborted", `Quick, test_slow_writer_is_not_aborted)
  ; ("activity in a child counts", `Quick, test_child_activity_counts)
  ; ( "failure after abort is reported"
    , `Quick
    , test_failure_after_abort_is_reported
    )
  ; ("returns without waiting for the watchdog", `Quick, test_returns_promptly)
  ; ("no abort once f has returned", `Quick, test_no_abort_after_return)
  ]

let () = Alcotest.run "Io_watchdog" [("with_watchdog", tests)]
