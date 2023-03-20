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
open Tracing

let hashtable_test : bool =
  enable_span_garbage_collector ~timeout:3. () ;
  let task_name = "test_task" in
  let tracer = TracerProviders.get_default_tracer ~name:task_name in
  let span0 = Tracer.start ~tracer ~name:task_name ~parent:None () in
  let span1 = Tracer.start ~tracer ~name:task_name ~parent:None () in
  let span2 = Tracer.start ~tracer ~name:task_name ~parent:None () in
  let span3 = Tracer.start ~tracer ~name:task_name ~parent:None () in
  try
    List.iter
      (fun s ->
        match s with
        | Ok span ->
            if Tracer.assert_finished span then
              failwith "unfinished span in finished_spans" ;
            let _ = Tracer.finish span in
            if not (Tracer.assert_finished span) then
              failwith "finished span not in finished_spans" ;
            ()
        | Error _ ->
            failwith "span failed"
      )
      [span0; span1; span2] ;
    Unix.sleep 5 ;
    match span3 with
    | Ok span ->
        if not (Tracer.assert_finished span) then
          failwith "timed-out span not in finished_spans"
        else
          true
    | Error _ ->
        failwith "span failed"
  with Failure _ -> false

let test_tracing () =
  let success = hashtable_test in
  Alcotest.(check bool) "created spans in the correct hash table" true success

let test = [("test_tracing", `Quick, test_tracing)]
