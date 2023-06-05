(*
 * Copyright (C) 2023 Cloud Software Group
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
module D = Debug.Make (struct let name = "tracing" end)

open D

type endpoint = Bugtool | Url of Uri.t

let attribute_key_regex =
  Re.Posix.compile_pat "^[a-z0-9][a-z0-9._]{0,253}[a-z0-9]$"

let validate_attribute (key, value) =
  Re.execp attribute_key_regex key && String.length value <= 4095

module SpanKind = struct
  type t = Server | Consumer | Client | Producer | Internal [@@deriving rpcty]

  let to_string = function
    | Server ->
        "SERVER"
    | Consumer ->
        "CONSUMER"
    | Client ->
        "CLIENT"
    | Producer ->
        "PRODUCER"
    | Internal ->
        "INTERNAL"
end

let endpoint_of_string = function
  | "bugtool" ->
      Bugtool
  | url ->
      Url (Uri.of_string url)

let ok_none = Ok None

module Status = struct
  type status_code = Unset | Ok | Error [@@deriving rpcty]

  type t = {status_code: status_code; description: string option}
end

module Attributes = struct
  include Map.Make (String)

  let of_list list = List.to_seq list |> of_seq

  let to_assoc_list attr = to_seq attr |> List.of_seq
end

module SpanEvent = struct
  type t = {name: string; time: float; attributes: string Attributes.t}
end

module SpanContext = struct
  type t = {
      trace_id: bytes
    ; span_id: bytes
    ; trace_flags: char
    ; tracestate: (string * string) list
    ; is_remote: bool
  }

  let bytes_to_hex b =
    Bytes.fold_right (fun c acc -> String.make 1 c ^ acc) b ""

  let hex_to_bytes h = String.to_seq h |> Bytes.of_seq

  let is_valid t = t.span_id <> Bytes.empty || t.trace_id <> Bytes.empty

  let get_span_id t = bytes_to_hex t.span_id

  let get_trace_id t = bytes_to_hex t.trace_id

  let generate_id n =
    Bytes.map
      (fun _ -> Random.int32 (Int32.of_int 255) |> Int32.to_int |> Char.chr)
      (Bytes.create n)

  let rec create sampled parent =
    let span_id = generate_id 8 in
    match parent with
    | None ->
        let trace_id = generate_id 16 in
        let trace_flags = if sampled then '\x01' else '\x00' in
        let t =
          {trace_id; span_id; trace_flags; tracestate= []; is_remote= false}
        in
        if is_valid t then t else create sampled None
    | Some parent ->
        let trace_id = parent.trace_id in
        let trace_flags = parent.trace_flags in
        let tracestate = parent.tracestate in
        let is_remote = false in
        let t = {trace_id; span_id; trace_flags; tracestate; is_remote} in
        if is_valid t then t else create sampled (Some parent)

  let encode_flags f =
    Hex.of_char f |> fun (a, b) -> String.make 1 a ^ String.make 1 b

  let decode_flags f =
    try "0x" ^ f |> int_of_string |> char_of_int with _ -> '\x00'

  let to_traceparent t =
    Printf.sprintf "00-%s-%s-%s" (get_trace_id t) (get_span_id t)
      (encode_flags t.trace_flags)

  let to_tracestate t =
    match
      t.tracestate |> List.map (fun (k, v) -> k ^ "=" ^ v) |> String.concat ","
    with
    | "" ->
        None
    | tracestate ->
        Some tracestate

  let tracestate_get key t = List.assoc_opt key t.tracestate

  let tracestate_delete key t =
    let tracestate = List.filter (fun (k, _) -> k <> key) t.tracestate in
    {t with tracestate}

  let tracestate_replace key value t =
    let tracestate =
      (key, value) :: List.filter (fun (k, _) -> k <> key) t.tracestate
    in
    {t with tracestate}

  let of_traceparent ?tracestate traceparent =
    let tracestate =
      Option.fold ~none:[]
        ~some:(fun s ->
          String.split_on_char ',' s
          |> List.filter_map (fun kv ->
                 match String.split_on_char '=' kv with
                 | [k; v] ->
                     Some (k, v)
                 | _ ->
                     None
             )
        )
        tracestate
    in
    let elements = String.split_on_char '-' traceparent in
    match elements with
    | ["00"; trace_id; span_id; flags] ->
        let trace_flags = decode_flags flags in
        let is_remote = true in
        let trace_id = hex_to_bytes trace_id in
        let span_id = hex_to_bytes span_id in
        Some {trace_id; span_id; trace_flags; tracestate; is_remote}
    | _ ->
        None
end

module SpanLink = struct
  type t = {context: SpanContext.t; attributes: (string * string) list}
end

module Span = struct
  type t = {
      context: SpanContext.t
    ; span_kind: SpanKind.t
    ; status: Status.t
    ; parent: SpanContext.t option
    ; name: string
    ; begin_time: float
    ; end_time: float option
    ; links: SpanLink.t list
    ; events: SpanEvent.t list
    ; attributes: string Attributes.t
  }

  let compare span1 span2 =
    SpanContext.(
      String.compare
        (to_traceparent span1.context)
        (to_traceparent span2.context)
    )

  let get_context t = t.context

  let start ?(sampled = true) ?(attributes = Attributes.empty) ~name ~parent
      ~span_kind () =
    let parent = Option.map (fun s -> s.context) parent in
    Option.iter
      (fun parent ->
        if not (SpanContext.is_valid parent) then
          failwith "Tracing: parent span has invalid context"
      )
      parent ;
    let context = SpanContext.create sampled parent in
    (* Using gettimeofday over Mtime as it is better for sharing timestamps between the systems *)
    let begin_time = Unix.gettimeofday () in
    let end_time = None in
    let status : Status.t = {status_code= Status.Unset; description= None} in
    let links = [] in
    let events = [] in
    {
      context
    ; span_kind
    ; status
    ; parent
    ; name
    ; begin_time
    ; end_time
    ; links
    ; events
    ; attributes
    }

  let get_tag t tag = Attributes.find tag t.attributes

  let finish ?(attributes = Attributes.empty) ~span () =
    let attributes =
      Attributes.union (fun _k a _b -> Some a) attributes span.attributes
    in
    {span with end_time= Some (Unix.gettimeofday ()); attributes}

  let set_span_kind span span_kind = {span with span_kind}

  let add_link span context attributes =
    let link : SpanLink.t = {context; attributes} in
    {span with links= link :: span.links}

  let add_event span name attributes =
    let attributes = Attributes.of_list attributes in
    let event : SpanEvent.t = {name; time= Unix.gettimeofday (); attributes} in
    {span with events= event :: span.events}

  let set_error span exn_t =
    match exn_t with
    | exn, stacktrace -> (
        let msg = Printexc.to_string exn in
        let exn_type = Printexc.exn_slot_name exn in
        let description =
          Some
            (Printf.sprintf "Error: %s Type: %s Backtrace: %s" msg exn_type
               stacktrace
            )
        in
        let status_code = Status.Error in
        let exn_attributes =
          [
            ("exception.message", msg)
          ; ("exception.stacktrace", stacktrace)
          ; ("exception.type", exn_type)
          ; ("error", "true")
          ]
        in
        match span.status.status_code with
        | Unset ->
            let attributes =
              Attributes.union
                (fun _k a _b -> Some a)
                span.attributes
                (Attributes.of_list exn_attributes)
            in
            {span with status= {status_code; description}; attributes}
        | _ ->
            span
      )

  let set_ok span =
    let description = None in
    let status_code = Status.Ok in
    match span.status.status_code with
    | Unset ->
        {span with status= {status_code; description}}
    | _ ->
        span
end

module Spans = struct
  let lock = Mutex.create ()

  let spans = Hashtbl.create 100

  let max_spans = ref 1000

  let set_max_spans x = max_spans := x

  let max_traces = ref 1000

  let set_max_traces x = max_traces := x

  let finished_spans = Hashtbl.create 100

  let span_hashtbl_is_empty () = Hashtbl.length spans = 0

  let finished_span_hashtbl_is_empty () = Hashtbl.length finished_spans = 0

  let add_to_spans ~(span : Span.t) =
    let key = span.context.trace_id in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt spans key with
        | None ->
            if Hashtbl.length spans < !max_traces then
              Hashtbl.add spans key [span]
        | Some span_list ->
            if List.length span_list < !max_spans then
              Hashtbl.replace spans key (span :: span_list)
    )

  let remove_from_spans span =
    let key = span.Span.context.trace_id in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt spans key with
        | None ->
            debug "Span does not exist or already finished" ;
            None
        | Some span_list ->
            ( match
                List.filter (fun x -> x.Span.context <> span.context) span_list
              with
            | [] ->
                Hashtbl.remove spans key
            | filtered_list ->
                Hashtbl.replace spans key filtered_list
            ) ;
            Some span
    )

  let add_to_finished span =
    let key = span.Span.context.trace_id in
    let flags = span.Span.context.trace_flags in
    if Char.code flags mod 2 <> 1 then
      ()
    else
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
          match Hashtbl.find_opt finished_spans key with
          | None ->
              if Hashtbl.length finished_spans < !max_traces then
                Hashtbl.add finished_spans key [span]
          | Some span_list ->
              if List.length span_list < !max_spans then
                Hashtbl.replace finished_spans key (span :: span_list)
      )

  let mark_finished span = Option.iter add_to_finished (remove_from_spans span)

  let span_is_finished x =
    match x with
    | None ->
        false
    | Some (span : Span.t) -> (
      match Hashtbl.find_opt finished_spans span.context.trace_id with
      | None ->
          false
      | Some span_list ->
          List.exists (fun x -> x = span) span_list
    )

  (** since copies the existing finished spans and then clears the existing spans as to only export them once  *)
  let since () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let finished_traces = Hashtbl.create 100 in
        Hashtbl.filter_map_inplace
          (fun trace_id spans ->
            match spans with
            | root :: _ when root.Span.parent = None ->
                Hashtbl.add finished_traces trace_id spans ;
                None
            | _ ->
                Some spans
          )
          finished_spans ;
        finished_traces
    )

  let dump () = Hashtbl.(copy spans, Hashtbl.copy finished_spans)

  module GC = struct
    let lock = Mutex.create ()

    let span_timeout = ref 86400.

    let span_timeout_thread = ref None

    let gc_inactive_spans () =
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
          Hashtbl.filter_map_inplace
            (fun _ spanlist ->
              let filtered =
                List.filter_map
                  (fun span ->
                    let elapsed =
                      Unix.gettimeofday () -. span.Span.begin_time
                    in
                    if elapsed > !span_timeout *. 1000000. then (
                      debug "Tracing: Span %s timed out, forcibly finishing now"
                        (SpanContext.get_span_id span.Span.context) ;
                      let span =
                        Span.finish ~span
                          ~attributes:
                            (Attributes.singleton "gc_inactive_span_timeout"
                               (string_of_float elapsed)
                            )
                          ()
                      in
                      add_to_finished span ; None
                    ) else
                      Some span
                  )
                  spanlist
              in
              match filtered with [] -> None | spans -> Some spans
            )
            spans
      )

    let initialise_thread ~timeout =
      span_timeout := timeout ;
      span_timeout_thread :=
        Some
          (Thread.create
             (fun () ->
               while true do
                 debug "Tracing: Span garbage collector" ;
                 Thread.delay !span_timeout ;
                 gc_inactive_spans ()
               done
             )
             ()
          )
  end
end

module TracerProvider = struct
  type t = {
      name_label: string
    ; attributes: string Attributes.t
    ; endpoints: endpoint list
    ; enabled: bool
  }

  let get_name_label t = t.name_label

  let get_attributes t = Attributes.to_assoc_list t.attributes

  let get_endpoints t = t.endpoints

  let get_enabled t = t.enabled
end

module Tracer = struct
  type t = {name: string; provider: TracerProvider.t}

  let create ~name ~provider = {name; provider}

  let no_op =
    let provider : TracerProvider.t =
      {
        name_label= ""
      ; attributes= Attributes.empty
      ; endpoints= []
      ; enabled= false
      }
    in
    {name= ""; provider}

  let span_of_span_context context name : Span.t =
    {
      context
    ; status= {status_code= Status.Unset; description= None}
    ; name
    ; parent= None
    ; span_kind= SpanKind.Client (* This will be the span of the client call*)
    ; begin_time= Unix.gettimeofday ()
    ; end_time= None
    ; links= []
    ; events= []
    ; attributes= Attributes.empty
    }

  let start ~tracer:t ?(span_kind = SpanKind.Internal) ~name ~parent () :
      (Span.t option, exn) result =
    (* Do not start span if the TracerProvider is diabled*)
    if not t.provider.enabled then
      ok_none
    else
      let attributes = t.provider.attributes in
      let span = Span.start ~attributes ~name ~parent ~span_kind () in
      Spans.add_to_spans ~span ; Ok (Some span)

  let finish ?error span =
    Ok
      (Option.map
         (fun span ->
           let span =
             match error with
             | Some exn_t ->
                 Span.set_error span exn_t
             | None ->
                 Span.set_ok span
           in
           let span = Span.finish ~span () in
           Spans.mark_finished span ; span
         )
         span
      )

  let span_is_finished x = Spans.span_is_finished x

  let span_hashtbl_is_empty () = Spans.span_hashtbl_is_empty ()

  let finished_span_hashtbl_is_empty () =
    Spans.finished_span_hashtbl_is_empty ()
end

let lock = Mutex.create ()

let tracer_providers = Hashtbl.create 100

let set ?enabled ?attributes ?endpoints ~uuid () =
  Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
      let provider =
        match Hashtbl.find_opt tracer_providers uuid with
        | Some (provider : TracerProvider.t) ->
            let enabled = Option.value ~default:provider.enabled enabled in
            let attributes : string Attributes.t =
              Option.fold ~none:provider.attributes ~some:Attributes.of_list
                attributes
            in
            let endpoints =
              Option.fold ~none:provider.endpoints
                ~some:(List.map endpoint_of_string)
                endpoints
            in
            {provider with enabled; attributes; endpoints}
        | None ->
            failwith
              (Printf.sprintf "The TracerProvider : %s does not exist" uuid)
      in
      Hashtbl.replace tracer_providers uuid provider
  )

let create ~enabled ~attributes ~endpoints ~name_label ~uuid =
  let endpoints = List.map endpoint_of_string endpoints in
  let attributes = Attributes.of_list attributes in
  let provider : TracerProvider.t =
    {name_label; attributes; endpoints; enabled}
  in
  Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
      match Hashtbl.find_opt tracer_providers uuid with
      | None ->
          Hashtbl.add tracer_providers uuid provider
      | Some _ ->
          failwith "Tracing : TracerProvider already exists"
  )

let destroy ~uuid =
  Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
      Hashtbl.remove tracer_providers uuid
  )

let get_tracer_providers () =
  Hashtbl.fold (fun _ provider acc -> provider :: acc) tracer_providers []

let get_tracer ~name =
  let providers =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.fold (fun _k v acc -> v :: acc) tracer_providers []
    )
  in
  match
    List.find_opt (fun provider -> provider.TracerProvider.enabled) providers
  with
  | Some provider ->
      Tracer.create ~name ~provider
  | None ->
      warn "No provider found" ; Tracer.no_op

let enable_span_garbage_collector ?(timeout = 86400.) () =
  Spans.GC.initialise_thread ~timeout

module Export = struct
  let export_interval = ref 30.

  let set_export_interval t = export_interval := t

  let host_id = ref "localhost"

  let set_host_id id = host_id := id

  let service_name = ref None

  let set_service_name name = service_name := Some name

  let get_service_name () =
    match !service_name with
    | None ->
        warn "service name not yet set!" ;
        "unknown"
    | Some name ->
        name

  module Content = struct
    module Json = struct
      module Zipkinv2 = struct
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
            List.map
              (fun event : ZipkinSpan.annotation ->
                let timestamp =
                  int_of_float (event.SpanEvent.time *. 1000000.)
                in
                let value = event.SpanEvent.name in
                {timestamp; value}
              )
              s.events
          in
          {
            id= SpanContext.get_span_id s.Span.context
          ; traceId= SpanContext.get_trace_id s.Span.context
          ; parentId= Option.map SpanContext.get_span_id s.parent
          ; name= s.name
          ; timestamp= int_of_float (s.begin_time *. 1000000.)
          ; duration=
              Option.value s.end_time ~default:(Unix.gettimeofday () *. 1000000.)
              -. s.begin_time
              |> ( *. ) 1000000.
              |> int_of_float
          ; kind=
              Option.map SpanKind.to_string
                (ZipkinSpan.kind_to_zipkin_kind s.span_kind)
          ; localEndpoint= {serviceName}
          ; annotations
          ; tags=
              Attributes.fold (fun k v tags -> (k, v) :: tags) s.attributes []
          }

        let content_of (spans : Span.t list) =
          List.map zipkin_span_of_span spans |> ZipkinSpan.json_of_t_list
      end
    end
  end

  module Destination = struct
    module File = struct
      let trace_log_dir = ref "/var/log/dt/zipkinv2/json"

      let set_trace_log_dir dir = trace_log_dir := dir

      let export ~trace_id ~span_json ~path : (string, exn) result =
        try
          let date = Ptime_clock.now () |> Ptime.to_rfc3339 ~frac_s:6 in
          let file =
            path
            ^ String.concat "-" [trace_id; "xapi"; !host_id; date]
            ^ ".json"
          in
          Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname file) 0o700 ;
          Xapi_stdext_unix.Unixext.write_string_to_file file span_json ;
          Ok ""
        with e -> Error e
    end

    module Http = struct
      module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
      module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

      let export ~span_json ~url : (string, exn) result =
        try
          let body = span_json in
          let headers =
            Cohttp.Header.of_list
              [
                ("accepts", "application/json")
              ; ("content-type", "application/json")
              ; ("content-length", string_of_int (String.length body))
              ]
          in
          Open_uri.with_open_uri url (fun fd ->
              let request =
                Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers url
              in
              let ic = Unix.in_channel_of_descr fd in
              let oc = Unix.out_channel_of_descr fd in
              Request.write
                (fun writer -> Request.write_body writer body)
                request oc ;
              Unix.shutdown fd Unix.SHUTDOWN_SEND ;
              match try Response.read ic with _ -> `Eof with
              | `Eof ->
                  Ok ""
              | `Invalid x ->
                  Error (Failure ("invalid read: " ^ x))
              | `Ok response ->
                  let body = Buffer.create 128 in
                  let reader = Response.make_body_reader response ic in
                  let rec loop () =
                    match Response.read_body_chunk reader with
                    | Cohttp.Transfer.Chunk x ->
                        Buffer.add_string body x ; loop ()
                    | Cohttp.Transfer.Final_chunk x ->
                        Buffer.add_string body x
                    | Cohttp.Transfer.Done ->
                        ()
                  in
                  loop () ;
                  Ok (Buffer.contents body)
          )
        with e -> Error e
    end

    let export_to_endpoint span_list endpoint =
      try
        debug "Tracing: About to export" ;
        Hashtbl.iter
          (fun trace_id span_list ->
            let zipkin_spans = Content.Json.Zipkinv2.content_of span_list in
            match
              match endpoint with
              | Url url ->
                  Http.export ~span_json:zipkin_spans ~url
              | Bugtool ->
                  let trace_id = SpanContext.bytes_to_hex trace_id in
                  File.export ~trace_id ~span_json:zipkin_spans
                    ~path:!File.trace_log_dir
            with
            | Ok _ ->
                ()
            | Error e ->
                raise e
          )
          span_list
      with e -> debug "Tracing: ERROR %s" (Printexc.to_string e)

    let flush_spans () =
      let span_list = Spans.since () in
      get_tracer_providers ()
      |> List.filter (fun x -> x.TracerProvider.enabled)
      |> List.concat_map (fun x -> TracerProvider.get_endpoints x)
      |> List.iter (export_to_endpoint span_list)

    let main () =
      enable_span_garbage_collector () ;
      Thread.create
        (fun () ->
          while true do
            debug "Tracing: Waiting %d seconds before exporting spans"
              (int_of_float !export_interval) ;
            Thread.delay !export_interval ;
            flush_spans ()
          done
        )
        ()
  end
end

let main = Export.Destination.main
