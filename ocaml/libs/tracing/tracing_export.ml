(*
 * Copyright (C) 2024 Cloud Software Group
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

module D = Debug.Make (struct let name = "tracing_export" end)

module Delay = Xapi_stdext_threads.Threadext.Delay
open D
open Tracing

let ( let@ ) f x = f x

let export_interval = ref 30.

let set_export_interval t = export_interval := t

let host_id = ref "localhost"

let set_host_id id = host_id := id

let service_name = ref "unknown"

let set_service_name name = service_name := name

let get_service_name () = !service_name

module Content = struct
  module Json = struct
    module ZipkinV2 = struct
      (* Module that helps export spans under Zipkin protocol, version 2. *)
      module ZipkinSpan = struct
        type zipkinEndpoint = {serviceName: string} [@@deriving rpcty]

        type annotation = {timestamp: int; value: string} [@@deriving rpcty]

        type t = {
            id: string
          ; traceId: string
          ; parentId: string option
          ; name: string
          ; timestamp: int
          ; duration: int
          ; kind: string option
          ; localEndpoint: zipkinEndpoint
          ; annotations: annotation list
          ; tags: (string * string) list
        }
        [@@deriving rpcty]

        type t_list = t list [@@deriving rpcty]

        let kind_to_zipkin_kind = function
          | SpanKind.Internal ->
              None
          | k ->
              Some k

        let json_of_t_list s =
          Rpcmarshal.marshal t_list.Rpc.Types.ty s |> Jsonrpc.to_string
      end

      let zipkin_span_of_span (s : Span.t) : ZipkinSpan.t =
        let serviceName = get_service_name () in
        let annotations =
          s
          |> Span.get_events
          |> List.map (fun event : ZipkinSpan.annotation ->
                 let timestamp =
                   int_of_float (event.SpanEvent.time *. 1000000.)
                 in
                 let value = event.SpanEvent.name in
                 {timestamp; value}
             )
        in
        let tags =
          let span_context = Span.get_context s in
          let trace_context =
            SpanContext.context_of_span_context span_context
          in
          let baggage =
            TraceContext.baggage_of trace_context |> Option.value ~default:[]
          in
          Span.get_attributes s @ baggage
        in
        {
          id=
            s
            |> Span.get_context
            |> SpanContext.span_id_of_span_context
            |> Span_id.to_string
        ; traceId=
            s
            |> Span.get_context
            |> SpanContext.trace_id_of_span_context
            |> Trace_id.to_string
        ; parentId=
            s
            |> Span.get_parent
            |> Option.map (fun x ->
                   x
                   |> Span.get_context
                   |> SpanContext.span_id_of_span_context
                   |> Span_id.to_string
               )
        ; name= s |> Span.get_name
        ; timestamp= int_of_float (Span.get_begin_time s *. 1000000.)
        ; duration=
            Option.value (Span.get_end_time s)
              ~default:(Unix.gettimeofday () *. 1000000.)
            -. Span.get_begin_time s
            |> ( *. ) 1000000.
            |> int_of_float
        ; kind=
            s
            |> Span.get_span_kind
            |> ZipkinSpan.kind_to_zipkin_kind
            |> Option.map SpanKind.to_string
        ; localEndpoint= {serviceName}
        ; annotations
        ; tags
        }

      let content_of (spans : Span.t list) =
        List.map zipkin_span_of_span spans |> ZipkinSpan.json_of_t_list
    end
  end
end

module Destination = struct
  module File = struct
    let trace_log_dir = ref "/var/log/dt/zipkinv2/json"

    let max_file_size = ref (1 lsl 20)

    let compress_tracing_files = ref true

    let set_trace_log_dir dir = trace_log_dir := dir

    let get_trace_log_dir () = !trace_log_dir

    let set_max_file_size size = max_file_size := size

    let set_compress_tracing_files enabled = compress_tracing_files := enabled

    let file_name = ref None

    let lock = Mutex.create ()

    let make_file_name () =
      let date = Ptime_clock.now () |> Ptime.to_rfc3339 ~frac_s:6 in
      let ( // ) = Filename.concat in
      let name =
        !trace_log_dir
        // String.concat "-" [get_service_name (); !host_id; date]
        ^ ".ndjson"
      in
      file_name := Some name ;
      name

    let with_fd file_name =
      Xapi_stdext_unix.Unixext.with_file file_name
        [O_WRONLY; O_CREAT; O_APPEND]
        0o700

    let write fd str =
      let content = str ^ "\n" in
      ignore @@ Unix.write_substring fd content 0 (String.length content)

    let export json =
      try
        let file_name =
          match !file_name with None -> make_file_name () | Some x -> x
        in
        Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname file_name) 0o700 ;
        let@ fd = file_name |> with_fd in
        write fd json ;
        if (Unix.fstat fd).st_size >= !max_file_size then (
          debug "Tracing: Rotating file %s > %d" file_name !max_file_size ;
          if !compress_tracing_files then
            Zstd.Fast.compress_file Zstd.Fast.compress ~file_path:file_name
              ~file_ext:"zst" ;
          ignore @@ make_file_name ()
        ) ;
        Ok ()
      with e -> Error e

    let with_stream f =
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () -> f export)
  end

  module Http = struct
    module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
    module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

    let export ~url json =
      try
        let body = json in
        let content_headers =
          [
            ("Content-Type", "application/json")
          ; ("Content-Length", string_of_int (String.length body))
          ]
        in
        let host =
          match (Uri.host url, Uri.port url) with
          | None, _ ->
              None
          | Some host, None ->
              Some host
          | Some host, Some port ->
              Some (Printf.sprintf "%s:%d" host port)
        in
        let host_headers =
          Option.fold ~none:[] ~some:(fun h -> [("Host", h)]) host
        in
        let headers =
          List.concat [content_headers; host_headers] |> Cohttp.Header.of_list
        in

        Open_uri.with_open_uri url (fun fd ->
            let request =
              Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers url
            in
            (* `with_open_uri` already closes the `fd`. And therefore
               according to the documentation of `in_channel_of_descr` and
               `out_channel_of_descr` we should not close the channels on top of
               `fd`. *)
            let ic = Unix.in_channel_of_descr fd in
            let oc = Unix.out_channel_of_descr fd in
            Request.write
              (fun writer -> Request.write_body writer body)
              request oc ;
            (* We flush instead of closing the sending stream as nginx responds to a TCP
               half-shutdown with a full shutdown of both directions of the HTTP request *)
            flush oc ;
            match try Response.read ic with _ -> `Eof with
            | `Eof ->
                Ok ()
            | `Invalid x ->
                Error (Failure ("invalid read: " ^ x))
            | `Ok response
              when Cohttp.Code.(response.status |> code_of_status |> is_error)
              ->
                Error (Failure (Cohttp.Code.string_of_status response.status))
            | `Ok _ ->
                Ok ()
        )
      with e -> Error e
  end

  let export_to_endpoint parent traces endpoint =
    debug "Tracing: About to export" ;
    try
      File.with_stream (fun file_export ->
          let export, name =
            match endpoint with
            | Url url ->
                (Http.export ~url, "Tracing.Http.export")
            | Bugtool ->
                (file_export, "Tracing.File.export")
          in
          let all_spans, count = traces in
          let attributes =
            [
              ("export.span.count", all_spans |> List.length |> string_of_int)
            ; ("export.endpoint", endpoint_to_string endpoint)
            ; ( "xs.tracing.spans_table.count"
              , Spans.span_count () |> string_of_int
              )
            ; ("xs.tracing.finished_spans_table.count", string_of_int count)
            ]
          in
          let@ _ =
            with_tracing ~trace_context:TraceContext.empty ~parent ~attributes
              ~name
          in
          all_spans
          |> Content.Json.ZipkinV2.content_of
          |> export
          |> Result.iter_error raise
      )
    with exn ->
      debug "Tracing: unable to export span : %s" (Printexc.to_string exn)

  let flush_spans () =
    let ((_span_list, span_count) as span_info) = Spans.since () in
    let attributes = [("export.traces.count", string_of_int span_count)] in
    let@ parent =
      with_tracing ~trace_context:TraceContext.empty ~parent:None ~attributes
        ~name:"Tracing.flush_spans"
    in
    TracerProvider.get_tracer_providers ()
    |> List.filter TracerProvider.get_enabled
    |> List.concat_map TracerProvider.get_endpoints
    |> List.iter (export_to_endpoint parent span_info)

  let delay = Delay.make ()

  (* Note this signal will flush the spans and terminate the exporter thread *)
  let signal () = Delay.signal delay

  let create_exporter () =
    enable_span_garbage_collector () ;
    Thread.create
      (fun () ->
        let signaled = ref false in
        while not !signaled do
          debug "Tracing: Waiting %d seconds before exporting spans"
            (int_of_float !export_interval) ;
          if not (Delay.wait delay !export_interval) then (
            debug "Tracing: we are signaled, export spans now and exit" ;
            signaled := true
          ) ;
          flush_spans ()
        done
      )
      ()

  let exporter = ref None

  let lock = Mutex.create ()

  let main () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match !exporter with
        | None ->
            let tid = create_exporter () in
            exporter := Some tid ;
            tid
        | Some tid ->
            tid
    )
end

let flush_and_exit = Destination.signal

let main = Destination.main
