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

open Test_common
open Event_types

let test_wakeup_twice () =
  let __context = make_test_database () in
  let _vm = make_vm __context () in
  let woken_up_by_task = ref false in
  Xapi_event.with_wakeup __context "Some location string" (fun wakeup_function wakeup_classes task ->
      let init_event = Xapi_event.from __context ["vm"] "" 1.0 |> Event_types.parse_event_from in
      let classes = wakeup_classes @ ["vm"] in

      let from_thread = Thread.create (fun () ->
          while (Db.is_valid_ref __context task) do
            let t = ref init_event.token in
            let evs = Xapi_event.from __context classes !t 1.0 |> Event_types.parse_event_from in
            List.iter (function
                | {ty = "task"; reference = t_ref} ->
                  woken_up_by_task := t_ref = (Ref.string_of task)
                | {ty = "vm"; reference = vm_ref} -> (Printf.printf "thigns";)
                | _ -> (Printf.printf "fads";)
              ) evs.events;
            Thread.delay 0.01;
            t := evs.token;
          done;) () in

      Thread.delay 0.01;
      wakeup_function ();
      wakeup_function ();

      Thread.join from_thread
    );
  Alcotest.(check bool) "Task kill function can be safely called repeatedly" true !woken_up_by_task

let test =
  [
    "test_wakeup_twice", `Slow, test_wakeup_twice;
  ]


