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

open Opentelemetry

module Backend (B1 : Collector.BACKEND) (B2 : Collector.BACKEND) :
  Collector.BACKEND = struct
  let send_trace =
    Collector.
      {
        send=
          (fun msg ~ret ->
            B1.send_trace.send msg ~ret:Fun.id ;
            B2.send_trace.send msg ~ret
          )
      }

  let send_metrics =
    Collector.
      {
        send=
          (fun msg ~ret ->
            B1.send_metrics.send msg ~ret:Fun.id ;
            B2.send_metrics.send msg ~ret
          )
      }

  let send_logs =
    Collector.
      {
        send=
          (fun msg ~ret ->
            B1.send_logs.send msg ~ret:Fun.id ;
            B2.send_logs.send msg ~ret
          )
      }

  let signal_emit_gc_metrics () =
    B1.signal_emit_gc_metrics () ;
    B2.signal_emit_gc_metrics ()

  let tick () = B1.tick () ; B2.tick ()

  let set_on_tick_callbacks t =
    B1.set_on_tick_callbacks t ; B2.set_on_tick_callbacks t

  let cleanup () = B1.cleanup () ; B2.cleanup ()
end

let with_setup (module B1 : Collector.BACKEND) (module B2 : Collector.BACKEND)
    ?enable () f =
  let module B = Backend (B1) (B2) in
  Collector.with_setup_debug_backend ?enable (module B) () f

let ticker interval =
  while true do
    let () =
      try
        match Collector.get_backend () with
        | Some (module B : Collector.BACKEND) ->
            B.tick ()
        | None ->
            ()
      with _ -> ()
    in
    (* this will drift, but during load that is what we want:
       we don't want the tick thread to add extra load
      *)
    Unix.sleepf interval
  done

let setup_tick ?(interval = 60.) () =
  let (_ : Thread.t) = Thread.create ticker interval in
  ()

let with_default_setup ?(filename = "trace") ?interval ?enable () f =
  Out_channel.with_open_text (filename ^ ".log") @@ fun trace_log ->
  let formatter = Format.formatter_of_out_channel trace_log in
  let module Text = (val ConsoleBackend.create_backend ~formatter ()) in
  let module OTLP = (val DiskBackend.create_backend ~filename ()) in
  let module Disk = Backend (OTLP) (Text) in
  setup_tick ?interval () ;
  with_setup
    (ConsoleBackend.create_backend ~severity:Severity_number_info ())
    (module Disk)
    ?enable () f
