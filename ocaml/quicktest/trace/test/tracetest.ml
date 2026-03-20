(*
 * Copyright (c) Cloud Software Group, Inc
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

open Quicktest_trace
open Quicktest_trace_rpc

let () =
  Printexc.record_backtrace true ;
  Fmt_tty.setup_std_outputs () ;
  Opentelemetry.Globals.service_name := "tracetest" ;
  Logs.set_level (Some Logs.Debug) ;
  Logs.set_reporter Logs_fmt.(reporter ~pp_header ()) ;
  TeeBackend.with_default_setup () @@ fun () ->
  let (_ : _ result) =
    Trace.with_result Fun.id "OK (not shown)" @@ fun _ -> Ok ()
  in
  let (_ : _ result) =
    Trace.with_result Fun.id "OK (sampled)" @@ fun scope ->
    Scope.set_decision scope Sampling.RECORD_AND_SAMPLE ;
    Ok ()
  in
  let (_ : _ result) =
    Trace.with_result Fun.id "Error (shown)" @@ fun _ -> Error "test"
  in
  let () = Trace.with_ "OK (not shown)" @@ fun _ -> () in
  let () =
    try
      Trace.with_ "Exception (shown)" @@ fun scope ->
      Scope.add_event scope (fun () -> Opentelemetry.Event.make "foo") ;
      Scope.add_log scope (fun () -> Opentelemetry.Logs.make_str "log1") ;
      Scope.add_log scope (fun () -> Opentelemetry.Logs.make_str "log2") ;
      failwith "TEST"
    with Failure _ -> ()
  in
  let (_ : (_, _) result) =
    Trace.with_result (fun _ -> "") "rpc" @@ fun scope ->
    Scope.set_decision scope Sampling.RECORD_AND_SAMPLE ;
    Error
      (RPC.wrap ~log_body:true
         (fun _ ->
           let (_ : _ list) = RPC.http_headers () in
           Rpc.
             {
               success= false
             ; contents= Rpc.String "failed"
             ; is_notification= false
             }
         )
         Rpc.
           {
             name= "test1"
           ; params= [Rpc.Int 4L; Rpc.Int 5L]
           ; is_notification= false
           }
      )
  in
  ()
