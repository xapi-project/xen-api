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

let fail fmt = Printf.ksprintf failwith fmt

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

let observe = Atomic.make false

let set_observe mode = Atomic.set observe mode

let get_observe () = Atomic.get observe

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

  let attr_of_originator = function
    | None ->
        []
    | Some originator ->
        [("xs.xapi.session.originator", originator)]
end

module SpanEvent = struct
  type t = {name: string; time: float; attributes: string Attributes.t}
end

module SpanContext = struct
  type t = {trace_id: string; span_id: string} [@@deriving rpcty]

  let to_traceparent t = Printf.sprintf "00-%s-%s-01" t.trace_id t.span_id

  let of_traceparent traceparent =
    let elements = String.split_on_char '-' traceparent in
    match elements with
    | ["00"; trace_id; span_id; _] ->
        Some {trace_id; span_id}
    | _ ->
        None

  let trace_id_of_span_context t = t.trace_id

  let span_id_of_span_context t = t.span_id
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

  let get_name span = span.name

  let get_parent span = span.parent

  let get_span_kind span = span.span_kind

  let get_begin_time span = span.begin_time

  let get_end_time span = span.end_time

  let get_events span = span.events

  let get_attributes span =
    Attributes.fold (fun k v tags -> (k, v) :: tags) span.attributes []

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

  let span_count () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.length spans
    )

  let max_spans = Atomic.make 1000

  let set_max_spans x = Atomic.set max_spans x

  let max_traces = Atomic.make 1000

  let set_max_traces x = Atomic.set max_traces x

  let finished_spans = Hashtbl.create 100

  let span_hashtbl_is_empty () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.length spans = 0
    )

  let finished_span_hashtbl_is_empty () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.length finished_spans = 0
    )

  let add_to_spans ~(span : Span.t) =
    let key = span.context.trace_id in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt spans key with
        | None ->
            if Hashtbl.length spans < Atomic.get max_traces then
              Hashtbl.add spans key [span]
            else
              debug "%s exceeded max traces when adding to span table"
                __FUNCTION__
        | Some span_list ->
            if List.length span_list < Atomic.get max_spans then
              Hashtbl.replace spans key (span :: span_list)
            else
              debug "%s exceeded max traces when adding to span table"
                __FUNCTION__
    )

  let remove_from_spans span =
    let key = span.Span.context.trace_id in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt spans key with
        | None ->
            debug "%s span does not exist or already finished" __FUNCTION__ ;
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
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt finished_spans key with
        | None ->
            if Hashtbl.length finished_spans < Atomic.get max_traces then
              Hashtbl.add finished_spans key [span]
            else
              debug "%s exceeded max traces when adding to finished span table"
                __FUNCTION__
        | Some span_list ->
            if List.length span_list < Atomic.get max_spans then
              Hashtbl.replace finished_spans key (span :: span_list)
            else
              debug "%s exceeded max traces when adding to finished span table"
                __FUNCTION__
    )

  let mark_finished span = Option.iter add_to_finished (remove_from_spans span)

  let span_is_finished x =
    match x with
    | None ->
        false
    | Some (span : Span.t) ->
        Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
            match Hashtbl.find_opt finished_spans span.context.trace_id with
            | None ->
                false
            | Some span_list ->
                List.mem span span_list
        )

  (** since copies the existing finished spans and then clears the existing spans as to only export them once  *)
  let since () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let copy = Hashtbl.copy finished_spans in
        Hashtbl.clear finished_spans ;
        copy
    )

  let dump () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.(copy spans, Hashtbl.copy finished_spans)
    )

  module GC = struct
    let lock = Mutex.create ()

    let span_timeout = Atomic.make 86400.
    (* one day in seconds *)

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
                    if elapsed > Atomic.get span_timeout *. 1000000. then (
                      debug "Tracing: Span %s timed out, forcibly finishing now"
                        span.Span.context.span_id ;
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
      Atomic.set span_timeout timeout ;
      span_timeout_thread :=
        Some
          (Thread.create
             (fun () ->
               while true do
                 debug "Tracing: Span garbage collector" ;
                 Thread.delay (Atomic.get span_timeout) ;
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

  let lock = Mutex.create ()

  let tracer_providers = Hashtbl.create 100

  let create ~enabled ~attributes ~endpoints ~name_label ~uuid =
    let provider : t =
      let endpoints = List.map endpoint_of_string endpoints in
      let attributes = Attributes.of_list attributes in
      {name_label; attributes; endpoints; enabled}
    in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        ( match Hashtbl.find_opt tracer_providers uuid with
        | None ->
            Hashtbl.add tracer_providers uuid provider
        | Some _ ->
            (* CP-45469: It is ok not to have an exception here since it is unlikely that the
               user has caused the issue, so no need to propagate back. It is also
               handy to not change the control flow since calls like cluster_pool_resync
               might not be aware that a TracerProvider has already been created.*)
            error "Tracing : TracerProvider %s already exists" name_label
        ) ;
        if enabled then set_observe true
    )

  let get_tracer_providers_unlocked () =
    Hashtbl.fold (fun _ provider acc -> provider :: acc) tracer_providers []

  let get_tracer_providers () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock
      get_tracer_providers_unlocked

  let set ?enabled ?attributes ?endpoints ~uuid () =
    let update_provider (provider : t) enabled attributes endpoints =
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
    in

    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let provider =
          match Hashtbl.find_opt tracer_providers uuid with
          | Some (provider : t) ->
              update_provider provider enabled attributes endpoints
          | None ->
              fail "The TracerProvider : %s does not exist" uuid
        in
        Hashtbl.replace tracer_providers uuid provider ;
        if
          List.for_all
            (fun provider -> not provider.enabled)
            (get_tracer_providers_unlocked ())
        then (
          set_observe false ;
          Xapi_stdext_threads.Threadext.Mutex.execute Spans.lock (fun () ->
              Hashtbl.clear Spans.spans ;
              Hashtbl.clear Spans.finished_spans
          )
        ) else
          set_observe true
    )

  let destroy ~uuid =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let _ = Hashtbl.remove tracer_providers uuid in
        if Hashtbl.length tracer_providers = 0 then set_observe false else ()
    )
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

  let get_tracer ~name =
    if Atomic.get observe then (
      let providers =
        Xapi_stdext_threads.Threadext.Mutex.execute TracerProvider.lock
          TracerProvider.get_tracer_providers_unlocked
      in

      match List.find_opt TracerProvider.get_enabled providers with
      | Some provider ->
          create ~name ~provider
      | None ->
          warn "No provider found for tracing %s" name ;
          no_op
    ) else
      no_op

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

let enable_span_garbage_collector ?(timeout = 86400.) () =
  Spans.GC.initialise_thread ~timeout

let with_tracing ?(attributes = []) ?(parent = None) ~name f =
  if Atomic.get observe then (
    let tracer = Tracer.get_tracer ~name in
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
  ) else
    f None

let with_child_trace ?attributes parent ~name f =
  match parent with
  | None ->
      f None
  | Some _ as parent ->
      with_tracing ?attributes ~parent ~name f

module EnvHelpers = struct
  let traceparent_key = "TRACEPARENT"

  let of_traceparent traceparent =
    match traceparent with
    | None ->
        []
    | Some traceparent ->
        [String.concat "=" [traceparent_key; traceparent]]

  let to_traceparent env =
    let env_opt =
      List.find_opt (String.starts_with ~prefix:traceparent_key) env
    in
    Option.bind env_opt (fun key_value ->
        match String.split_on_char '=' key_value with
        | [key; traceparent] when String.equal key traceparent_key ->
            Some traceparent
        | _ ->
            None
    )

  let of_span span =
    match span with
    | None ->
        []
    | Some span ->
        Some (span |> Span.get_context |> SpanContext.to_traceparent)
        |> of_traceparent
end
