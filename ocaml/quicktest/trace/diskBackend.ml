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

module type BACKEND = sig
  include Opentelemetry.Collector.BACKEND

  val register_metrics : unit -> unit
end

let create_backend' ~filename () : (module BACKEND) =
  (module struct
    type 'a t = {
        file: Out_channel.t
      ; ch: 'a Event.channel
      ; send_ns: int Atomic.t
    }

    let measure sum f =
      let t0 = Mtime_clock.counter () in
      let r = f () in
      let dt =
        Mtime_clock.count t0 |> Mtime.Span.to_uint64_ns |> Int64.to_int
      in
      let (_ : int) = Atomic.fetch_and_add sum dt in
      r

    let flush_ns = Atomic.make 0

    let write_ns = Atomic.make 0

    let tick_ns = Atomic.make 0

    let handle exporter encoder {file; ch; _} =
      let t = Pbrt.Encoder.create () in
      let rec loop () =
        match Event.(ch |> receive |> sync) with
        | None ->
            ()
        | Some [] ->
            measure flush_ns @@ fun () -> flush file ; (loop [@tailcall]) ()
        | Some msg ->
            let () =
              measure write_ns @@ fun () ->
              encoder (exporter msg) t ;
              Pbrt.Encoder.write_chunks (Out_channel.output file) t ;
              Pbrt.Encoder.clear t
            in
            (loop [@tailcall]) ()
      in
      let finally () = Out_channel.close_noerr file in
      Fun.protect ~finally loop

    let metrics_cb name t () =
      let open Opentelemetry in
      [
        Metrics.sum ~name:(name ^ " overhead")
          ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
          ~unit_:"ns"
          [Metrics.int (Atomic.get t)]
      ]

    let make exporter encoder filename =
      let file = Out_channel.open_bin (filename ^ ".otel") in
      let t = {file; ch= Event.new_channel (); send_ns= Atomic.make 0} in
      let thread = Thread.create (handle exporter encoder) t in
      (thread, t)

    let trace =
      let open Opentelemetry_proto.Trace_service in
      make
        (fun resource_spans ->
          make_export_trace_service_request ~resource_spans ()
        )
        encode_pb_export_trace_service_request (filename ^ ".trace")

    let metrics =
      let open Opentelemetry_proto.Metrics_service in
      make
        (fun resource_metrics ->
          make_export_metrics_service_request ~resource_metrics ()
        )
        encode_pb_export_metrics_service_request (filename ^ ".metrics")

    let logs =
      let open Opentelemetry_proto.Logs_service in
      make
        (fun resource_logs -> make_export_logs_service_request ~resource_logs ())
        encode_pb_export_logs_service_request (filename ^ ".logs")

    let registered = Atomic.make false

    let register_metrics () =
      if not (Atomic.exchange registered true) then begin
        Opentelemetry.Metrics_callbacks.register
          (metrics_cb "trace" (snd trace).send_ns) ;
        Opentelemetry.Metrics_callbacks.register
          (metrics_cb "logs" (snd logs).send_ns) ;
        Opentelemetry.Metrics_callbacks.register
          (metrics_cb "metrics" (snd metrics).send_ns) ;
        Opentelemetry.Metrics_callbacks.register
          (metrics_cb "file buffer write" write_ns) ;
        Opentelemetry.Metrics_callbacks.register
          (metrics_cb "file flush" write_ns) ;
        Opentelemetry.Metrics_callbacks.register (metrics_cb "tick" tick_ns)
      end

    let send (_, t) msg =
      measure t.send_ns @@ fun () -> Event.(send t.ch msg |> sync)

    open Opentelemetry.Collector

    let send_trace = {send= (fun msg ~ret -> send trace (Some msg) ; ret ())}

    let send_metrics =
      {send= (fun msg ~ret -> send metrics (Some msg) ; ret ())}

    let send_logs = {send= (fun msg ~ret -> send logs (Some msg) ; ret ())}

    let signal_emit_gc_metrics = ignore

    let on_tick = Atomic.make (Opentelemetry.AList.make ())

    let invoke f = f ()

    let tick () =
      register_metrics () ;
      measure tick_ns @@ fun () ->
      on_tick |> Atomic.get |> Opentelemetry.AList.get |> List.iter invoke ;
      send trace (Some []) ;
      send logs (Some []) ;
      send metrics (Some [])

    let set_on_tick_callbacks = Atomic.set on_tick

    let cleanup () =
      send trace None ;
      send logs None ;
      send metrics None ;
      Thread.join (fst trace) ;
      Thread.join (fst metrics) ;
      Thread.join (fst logs)
  end
)

let create_backend ~filename () : (module Opentelemetry.Collector.BACKEND) =
  let (module B) = create_backend' ~filename () in
  (module B)

let with_setup ~filename ?enable () f =
  let (module B) = create_backend' ~filename () in
  Opentelemetry.Collector.with_setup_debug_backend ?enable (module B) ()
  @@ fun () ->
  (* can only be called when backend is registered, otherwise the on_tick
     callback gets registered to the wrong place *)
  B.register_metrics () ; f ()
