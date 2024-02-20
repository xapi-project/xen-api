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

module Delay = Xapi_stdext_threads.Threadext.Delay
open D

module W3CBaggage = struct
  module Key = struct
    let is_valid_key str =
      let is_tchar = function
        | '0' .. '9'
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '!'
        | '#'
        | '$'
        | '%'
        | '&'
        | '\''
        | '*'
        | '+'
        | '-'
        | '.'
        | '^'
        | '_'
        | '`'
        | '|'
        | '~' ->
            true
        | _ ->
            false
      in
      String.for_all (fun c -> is_tchar c) str
  end

  module Value = struct
    type t = string

    let make str =
      let char_needs_encoding = function
        (* Encode anything that isn't in basic US-ASCII or is a Control, whitespace, DQUOTE , ; or \ *)
        | '\000' .. '\032' | '"' | ',' | ';' | '\\' | '\127' .. '\255' ->
            true
        | _ ->
            false
      in
      if String.exists (fun c -> char_needs_encoding c) str then
        let encode_char x =
          if char_needs_encoding x then
            Printf.sprintf "%%%02X" (Char.code x)
          else
            String.make 1 x
        in
        String.to_seq str
        |> Seq.map encode_char
        |> List.of_seq
        |> String.concat ""
      else
        str

    let to_string value : t = value
  end
end

type endpoint = Bugtool | Url of Uri.t

let attribute_key_regex =
  Re.Posix.compile_pat "^[a-z0-9][a-z0-9._]{0,253}[a-z0-9]$"

let validate_attribute (key, value) =
  String.length value <= 4095
  && Re.execp attribute_key_regex key
  && W3CBaggage.Key.is_valid_key key

let observe = ref true

let set_observe mode = observe := mode

let ( let@ ) f x = f x

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

let bugtool_name = "bugtool"

let endpoint_of_string = function
  | str when str = bugtool_name ->
      Bugtool
  | url ->
      Url (Uri.of_string url)

let endpoint_to_string = function
  | Bugtool ->
      bugtool_name
  | Url url ->
      Uri.to_string url

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
  type t = {trace_id: string; span_id: string} [@@deriving rpcty]

  let to_traceparent t = Printf.sprintf "00-%s-%s-00" t.trace_id t.span_id

  let of_traceparent traceparent =
    let elements = String.split_on_char '-' traceparent in
    match elements with
    | ["00"; trace_id; span_id; _] ->
        Some {trace_id; span_id}
    | _ ->
        None

  let trace_id_of_span_context t = t.trace_id
end

module SpanLink = struct
  type t = {context: SpanContext.t; attributes: (string * string) list}
end

module Span = struct
  type t = {
      context: SpanContext.t
    ; span_kind: SpanKind.t
    ; status: Status.t
    ; parent: t option
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

  let generate_id n = String.init n (fun _ -> "0123456789abcdef".[Random.int 16])

  let start ?(attributes = Attributes.empty) ~name ~parent ~span_kind () =
    let trace_id =
      match parent with
      | None ->
          generate_id 32
      | Some span_parent ->
          span_parent.context.trace_id
    in
    let span_id = generate_id 16 in
    let context : SpanContext.t = {trace_id; span_id} in
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
  module SpanTbl = struct
    let lock = Mutex.create ()

    type t = (string, Span.t list) Hashtbl.t

    let max_spans = ref 1000

    let max_traces = ref 1000

    let set_max_spans x = max_spans := x

    let set_max_traces x = max_traces := x

    let create init = Hashtbl.create init

    let is_empty spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      Hashtbl.length spantbl = 0

    module SpanMap = Map.Make (String)

    type frequencies = (string * int) list [@@deriving yojson]

    let depth = 10

    let make_freq_tbl spantbl =
      let freq_tbl = Hashtbl.create 100 in
      Hashtbl.iter
        (fun _ spans ->
          List.iter
            (fun (span : Span.t) ->
              let span_count =
                span.name
                |> Hashtbl.find_opt freq_tbl
                |> Option.value ~default:0
              in
              Hashtbl.replace freq_tbl span.name (span_count + 1)
            )
            spans
        )
        spantbl ;
      freq_tbl

    let take_most k v acc =
      let last_min = SpanMap.min_binding_opt acc in
      match last_min with
      | None ->
          SpanMap.add k v acc
      | Some last_min ->
          if SpanMap.cardinal acc < depth then
            SpanMap.add k v acc
          else if v <= snd last_min then
            acc
          else
            acc |> SpanMap.remove (fst last_min) |> SpanMap.add k v

    let most_frequent freq_tbl = Hashtbl.fold take_most freq_tbl SpanMap.empty

    let spans_with_names spantbl span_names =
      let spans_copy = Hashtbl.copy spantbl in
      Hashtbl.filter_map_inplace
        (fun _ v ->
          Some
            (List.filter
               (fun (span : Span.t) ->
                 List.exists (String.equal span.name) span_names
               )
               v
            )
        )
        spans_copy ;
      spans_copy
      |> Hashtbl.to_seq_values
      |> Seq.map List.to_seq
      |> Seq.concat
      |> List.of_seq

    let add_helper ?(trace_bucket = []) spantbl span key =
      let full () =
        match trace_bucket with
        | [] ->
            Hashtbl.length spantbl >= !max_traces
        | _ ->
            List.length trace_bucket >= !max_spans
      in
      Hashtbl.replace spantbl key (span :: trace_bucket) ;
      if full () then
        let _ =
          debug "%s exceeded max traces when adding to finished span table"
            __FUNCTION__
        in
        let freq_tbl = make_freq_tbl spantbl in
        freq_tbl
        |> most_frequent
        |> SpanMap.to_seq
        |> Seq.map fst
        |> List.of_seq
        |> spans_with_names spantbl
        |> Option.some
      else
        None

    let add spantbl span =
      let key = span.Span.context.trace_id in
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      add_helper ?trace_bucket:(Hashtbl.find_opt spantbl key) spantbl span key

    let mem spantbl (span : Span.t) =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      match Hashtbl.find_opt spantbl span.context.trace_id with
      | None ->
          false
      | Some span_list ->
          List.exists (fun x -> x = span) span_list

    let remove spantbl span =
      let key = span.Span.context.trace_id in
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      match Hashtbl.find_opt spantbl key with
      | None ->
          debug "%s span does not exist" __FUNCTION__ ;
          None
      | Some span_list ->
          ( match
              List.filter (fun x -> x.Span.context <> span.context) span_list
            with
          | [] ->
              Hashtbl.remove spantbl key
          | filtered_list ->
              Hashtbl.replace spantbl key filtered_list
          ) ;
          Some span

    let clear spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      Hashtbl.clear spantbl

    let since spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      let cleared = Hashtbl.copy spantbl in
      Hashtbl.clear spantbl ; cleared

    let copy spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      Hashtbl.copy spantbl

    let stats spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      let freq_tbl = make_freq_tbl spantbl in
      let most_frequent_list =
        freq_tbl |> most_frequent |> SpanMap.to_seq |> List.of_seq
      in
      debug "Most frequent %i span names: %s" depth
        (most_frequent_list |> frequencies_to_yojson |> Yojson.Safe.to_string) ;
      most_frequent_list

    let count_traces spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      Hashtbl.length spantbl

    let traces spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      spantbl |> Hashtbl.to_seq_keys |> List.of_seq

    let to_list spantbl =
      Xapi_stdext_threads.Threadext.Mutex.execute lock @@ fun _ ->
      Hashtbl.fold (fun _ spans acc -> spans @ acc) spantbl []
  end

  let ongoing_spans = SpanTbl.create 100

  let finished_spans = SpanTbl.create 100

  let mark_finished span =
    span
    |> SpanTbl.remove ongoing_spans
    |> Option.iter (fun span -> span |> SpanTbl.add finished_spans |> ignore)

  let get_oldest_half (spans : Span.t list) =
    let sorted_spans =
      List.sort
        (fun (s1 : Span.t) (s2 : Span.t) -> compare s1.begin_time s2.begin_time)
        spans
    in
    let length = List.length sorted_spans in
    Xapi_stdext_std.Listext.List.take ((length + 1) / 2) sorted_spans

  let finish_oldest_half (spans : Span.t list) =
    let attributes =
      Attributes.singleton "xs.tracing.finished.reason" "gc.full"
    in
    spans
    |> get_oldest_half
    |> List.iter (fun span ->
           let span = Span.finish ~attributes ~span () in
           debug "Mark span: %s as finished" span.name ;
           mark_finished span
       )

  let add_to_ongoing span =
    match SpanTbl.add ongoing_spans span with
    | None ->
        ()
    | Some spans ->
        finish_oldest_half spans

  let span_is_finished span =
    Option.fold ~none:false ~some:(SpanTbl.mem finished_spans) span

  let dump () = SpanTbl.(copy ongoing_spans, copy finished_spans)

  module GC = struct
    let lock = Mutex.create ()

    let span_timeout = ref 86400.

    let span_timeout_thread = ref None

    let gc_inactive_spans () =
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
          ongoing_spans
          |> SpanTbl.to_list
          |> List.filter_map (fun span ->
                 let elapsed = Unix.gettimeofday () -. span.Span.begin_time in
                 if elapsed > !span_timeout *. 1000000. then (
                   debug "Tracing: Span %s timed out, forcibly finishing now"
                     span.Span.context.span_id ;
                   let _ =
                     Span.finish ~span
                       ~attributes:
                         (Attributes.singleton "gc_inactive_span_timeout"
                            (string_of_float elapsed)
                         )
                       ()
                   in
                   Some span
                 ) else
                   None
             )
          |> List.iter mark_finished
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

  let start ~tracer:t ?(attributes = []) ?(span_kind = SpanKind.Internal) ~name
      ~parent () : (Span.t option, exn) result =
    (* Do not start span if the TracerProvider is diabled*)
    if not t.provider.enabled then
      ok_none
    else
      let attributes = Attributes.of_list attributes in
      let attributes =
        Attributes.union
          (fun _k a _b -> Some a)
          attributes t.provider.attributes
      in
      let span = Span.start ~attributes ~name ~parent ~span_kind () in
      Spans.add_to_ongoing span ; Ok (Some span)

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

  let span_hashtbl_is_empty () = Spans.(SpanTbl.is_empty ongoing_spans)

  let finished_span_hashtbl_is_empty () = Spans.(SpanTbl.is_empty finished_spans)
end

let lock = Mutex.create ()

let tracer_providers = Hashtbl.create 100

let get_tracer_providers () =
  Hashtbl.fold (fun _ provider acc -> provider :: acc) tracer_providers []

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
  ) ;
  if
    List.for_all
      (fun provider -> not provider.TracerProvider.enabled)
      (get_tracer_providers ())
  then (
    Spans.(
      SpanTbl.clear ongoing_spans ;
      SpanTbl.clear finished_spans
    )
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
          (* CP-45469: It is ok not to have an exception here since it is unlikely that the
             user has caused the issue, so no need to propagate back. It is also
             handy to not change the control flow since calls like cluster_pool_resync
             might not be aware that a TracerProvider has already been created.*)
          error "Tracing : TracerProvider %s already exists" name_label
  )

let destroy ~uuid =
  Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
      Hashtbl.remove tracer_providers uuid
  )

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
      warn "No provider found for tracing %s" name ;
      Tracer.no_op

let enable_span_garbage_collector ?(timeout = 86400.) () =
  Spans.GC.initialise_thread ~timeout

let with_tracing ?(attributes = []) ?(parent = None) ~name f =
  if not !observe then
    f None
  else
    let tracer = get_tracer ~name in
    match Tracer.start ~tracer ~attributes ~name ~parent () with
    | Ok span -> (
      try
        let result = f span in
        ignore @@ Tracer.finish span ;
        result
      with exn ->
        let backtrace = Printexc.get_backtrace () in
        let error = (exn, backtrace) in
        ignore @@ Tracer.finish span ~error ;
        raise exn
    )
    | Error e ->
        warn "Failed to start tracing: %s" (Printexc.to_string e) ;
        f None

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
            id= s.context.span_id
          ; traceId= s.context.trace_id
          ; parentId= Option.map (fun x -> x.Span.context.span_id) s.parent
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

      let max_file_size = ref (1 lsl 20)

      let compress_tracing_files = ref true

      let set_trace_log_dir dir = trace_log_dir := dir

      let set_max_file_size size = max_file_size := size

      let set_compress_tracing_files enabled = compress_tracing_files := enabled

      let file_name = ref None

      let lock = Mutex.create ()

      let new_file_name () =
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
            match !file_name with None -> new_file_name () | Some x -> x
          in
          Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname file_name) 0o700 ;
          let@ fd = file_name |> with_fd in
          write fd json ;
          if (Unix.fstat fd).st_size >= !max_file_size then (
            debug "Tracing: Rotating file %s > %d" file_name !max_file_size ;
            if !compress_tracing_files then
              Zstd.Fast.compress_file Zstd.Fast.compress ~file_path:file_name
                ~file_ext:"zst" ;
            ignore @@ new_file_name ()
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
          let headers =
            Cohttp.Header.of_list
              ([
                 ("Content-Type", "application/json")
               ; ("Content-Length", string_of_int (String.length body))
               ]
              @
              match Uri.host url with
              | None ->
                  []
              | Some h ->
                  let port =
                    match Uri.port url with
                    | Some p ->
                        ":" ^ string_of_int p
                    | None ->
                        ""
                  in
                  [("Host", h ^ port)]
              )
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
                  loop () ; Ok ()
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
            let all_spans = Spans.SpanTbl.to_list traces in
            let attributes =
              [
                ("export.span.count", all_spans |> List.length |> string_of_int)
              ; ("export.endpoint", endpoint_to_string endpoint)
              ; ( "xs.tracing.spans_table.count"
                , Spans.ongoing_spans
                  |> Spans.SpanTbl.count_traces
                  |> string_of_int
                )
              ; ( "xs.tracing.finished_spans_table.count"
                , traces |> Spans.SpanTbl.count_traces |> string_of_int
                )
              ]
            in
            let@ _ = with_tracing ~parent ~attributes ~name in
            Content.Json.Zipkinv2.content_of all_spans
            |> export
            |> Result.iter_error raise
        )
      with exn ->
        debug "Tracing: unable to export span : %s" (Printexc.to_string exn)

    let flush_spans () =
      let span_list = Spans.(SpanTbl.since finished_spans) in
      let attributes =
        [
          ( "export.traces.count"
          , span_list |> Spans.SpanTbl.count_traces |> string_of_int
          )
        ]
      in
      let@ parent =
        with_tracing ~parent:None ~attributes ~name:"Tracing.flush_spans"
      in
      get_tracer_providers ()
      |> List.filter (fun x -> x.TracerProvider.enabled)
      |> List.concat_map (fun x -> TracerProvider.get_endpoints x)
      |> List.iter (export_to_endpoint parent span_list)

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
end

let flush_and_exit = Export.Destination.signal

let main = Export.Destination.main
