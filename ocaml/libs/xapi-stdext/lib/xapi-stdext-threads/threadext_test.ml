(*
 * Copyright (C) 2006-2024 Citrix Systems Inc.
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

module Delay = Xapi_stdext_threads.Threadext.Delay

let delay_wait_check ~min ~max delay timeout expected =
  let cnt = Mtime_clock.counter () in
  let res = Delay.wait delay timeout in
  let elapsed = (Mtime_clock.count cnt |> Mtime.Span.to_float_ns) *. 1e-9 in
  Alcotest.(check bool) "expected result" expected res ;
  if elapsed < min || elapsed > max then
    let msg = Printf.sprintf "%f not in range %f-%f" elapsed min max in
    Alcotest.(check bool) msg true false

(*
Single simple signal stored
- signal
- wait on same thread should succeed quickly
*)
let simple () =
  let d = Delay.make () in
  Delay.signal d ;
  delay_wait_check ~min:0. ~max:0.01 d 1.0 false

(*
No signal
- wait on same thread should timeout more or less on delay
*)
let no_signal () =
  let d = Delay.make () in
  delay_wait_check ~min:0.1 ~max:0.11 d 0.1 true

(*
Signal twice, collapsed
- signal
- signal
- wait on same thread should succeed quickly
- wait on same thread should timeout
*)
let collapsed () =
  let d = Delay.make () in
  Delay.signal d ;
  Delay.signal d ;
  delay_wait_check ~min:0. ~max:0.01 d 0.1 false ;
  delay_wait_check ~min:0.1 ~max:0.11 d 0.1 true

(*
Signal from another thread
- signal on another thread after a while
- wait on same thread should succeed more or less on other thread sleep
*)
let other_thread () =
  let d = Delay.make () in
  let th = Thread.create (fun d -> Thread.delay 0.1 ; Delay.signal d) d in
  delay_wait_check ~min:0.1 ~max:0.11 d 1.0 false ;
  Thread.join th

let tests =
  [
    ("simple", `Quick, simple)
  ; ("no_signal", `Quick, no_signal)
  ; ("collapsed", `Quick, collapsed)
  ; ("other_thread", `Quick, other_thread)
  ]

let () = Alcotest.run "Threadext" [("Delay", tests)]
