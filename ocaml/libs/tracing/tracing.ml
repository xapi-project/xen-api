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

let failures = Atomic.make 0

let not_throttled () =
  let old = Atomic.fetch_and_add failures 1 in
  old < 2

let reset_throttled () = Atomic.set failures 0

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

module SpanKind = struct
  type t = Server | Consumer | Client | Producer | Internal

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
  type status_code = Unset | Ok | Error

  type t = {status_code: status_code; _description: string option}
end

module Attributes = struct
  include Map.Make (String)

  let merge_element map (key, value) = add key value map

  let merge_into into list = List.fold_left merge_element into list

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

module Trace_id : sig
  type t

  val make : unit -> t

  val compare : t -> t -> int

  val of_string : string -> t

  val to_string : t -> string
end = struct
  type t = int64 * int64

  let make () = (Random.bits64 (), Random.bits64 ())

  let of_string s =
    try Scanf.sscanf s "%016Lx%016Lx" (fun a b -> (a, b))
    with e ->
      D.debug "Failed to parse trace id %s: %s" s (Printexc.to_string e) ;
      (* don't cause XAPI to fail *)
      (0L, 0L)

  let to_string (a, b) = Printf.sprintf "%016Lx%016Lx" a b

  let compare (a1, a2) (b1, b2) =
    match Int64.compare a1 b1 with 0 -> Int64.compare a2 b2 | n -> n
end

module Span_id : sig
  type t

  val make : unit -> t

  val compare : t -> t -> int

  val of_string : string -> t

  val to_string : t -> string
end = struct
  type t = int64

  let make = Random.bits64

  let of_string s =
    try Scanf.sscanf s "%Lx" Fun.id
    with e ->
      D.debug "Failed to parse span id %s: %s" s (Printexc.to_string e) ;
      (* don't cause XAPI to fail *)
      0L

  let to_string = Printf.sprintf "%016Lx"

  let compare = Int64.compare
end

(* The context of a trace that can be propagated across service boundaries. *)
module TraceContext = struct
  type traceparent = string [@@deriving yojson]

  type baggage = (string * string) list [@@deriving yojson]

  type t = {traceparent: traceparent option; baggage: baggage option}
  [@@deriving yojson]

  let empty = {traceparent= None; baggage= None}

  let with_traceparent traceparent ctx = {ctx with traceparent}

  let with_baggage baggage ctx = {ctx with baggage}

  let traceparent_of ctx = ctx.traceparent

  let baggage_of ctx = ctx.baggage

  let to_json_string t = Yojson.Safe.to_string (to_yojson t)

  let of_json_string s = of_yojson (Yojson.Safe.from_string s)
end

module SpanContext = struct
  type t = {
      trace_id: Trace_id.t
    ; span_id: Span_id.t
    ; trace_context: TraceContext.t
  }

  let context trace_id span_id =
    {trace_id; span_id; trace_context= TraceContext.empty}

  let to_traceparent t =
    let tid = Trace_id.to_string t.trace_id in
    let sid = Span_id.to_string t.span_id in
    Printf.sprintf "00-%s-%s-01" tid sid

  let of_traceparent traceparent =
    let elements = String.split_on_char '-' traceparent in
    match elements with
    | ["00"; trace_id; span_id; _] ->
        Some
          {
            trace_id= Trace_id.of_string trace_id
          ; span_id= Span_id.of_string span_id
          ; trace_context= TraceContext.empty
          }
    | _ ->
        None

  let trace_id_of_span_context t = t.trace_id

  let span_id_of_span_context t = t.span_id

  let context_of_span_context t = t.trace_context

  let with_trace_context trace_context t = {t with trace_context}

  let of_trace_context trace_context =
    let traceparent = TraceContext.traceparent_of trace_context in
    let span_context = Option.(join (map of_traceparent traceparent)) in
    Option.map (with_trace_context trace_context) span_context
end

module SpanLink = struct
  type t = {_context: SpanContext.t; _attributes: (string * string) list}
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

  let get_trace_context t = t.context |> SpanContext.context_of_span_context

  let start ?(attributes = Attributes.empty)
      ?(trace_context : TraceContext.t option) ~name ~parent ~span_kind () =
    let trace_id, extra_context =
      match parent with
      | None ->
          (Trace_id.make (), TraceContext.empty)
      | Some span_parent ->
          (span_parent.context.trace_id, span_parent.context.trace_context)
    in
    let span_id = Span_id.make () in
    let context : SpanContext.t =
      {trace_id; span_id; trace_context= extra_context}
    in
    let context =
      (* If trace_context is provided to the call, override any inherited trace context. *)
      trace_context
      |> Option.fold ~none:context
           ~some:(Fun.flip SpanContext.with_trace_context context)
    in
    (* Using gettimeofday over Mtime as it is better for sharing timestamps between the systems *)
    let begin_time = Unix.gettimeofday () in
    let end_time = None in
    let status : Status.t = {status_code= Status.Unset; _description= None} in
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
    let link : SpanLink.t = {_context= context; _attributes= attributes} in
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
        let stacktrace = Printexc.raw_backtrace_to_string stacktrace in
        let _description =
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
            {span with status= {status_code; _description}; attributes}
        | _ ->
            span
      )

  let set_ok span =
    let _description = None in
    let status_code = Status.Ok in
    match span.status.status_code with
    | Unset ->
        {span with status= {status_code; _description}}
    | _ ->
        span

  let with_trace_context span trace_context =
    let span_context =
      span |> get_context |> SpanContext.with_trace_context trace_context
    in
    {span with context= span_context}
end

module TraceMap = Map.Make (Trace_id)
module SpanMap = Map.Make (Span_id)

module Spans = struct
  let spans = Atomic.make TraceMap.empty

  let rec update_spans f arg =
    let old = Atomic.get spans in
    let next = f old arg in
    if Atomic.compare_and_set spans old next then
      ()
    else (
      (* TODO: should use Kcas.update, or Saturn skip_lists for domains *)
      Thread.yield () ;
      (update_spans [@tailcall]) f arg
    )

  let span_count () = TraceMap.cardinal (Atomic.get spans)

  let max_spans = Atomic.make 2500

  let set_max_spans x = Atomic.set max_spans x

  let max_traces = Atomic.make 1000

  let set_max_traces x = Atomic.set max_traces x

  let finished_spans = Atomic.make ([], 0)

  let span_hashtbl_is_empty () = TraceMap.is_empty (Atomic.get spans)

  let finished_span_hashtbl_is_empty () = Atomic.get finished_spans |> snd = 0

  let add_to_spans_unlocked spans (span : Span.t) =
    let key = span.context.trace_id in
    match TraceMap.find_opt key spans with
    | None ->
        if TraceMap.cardinal spans < Atomic.get max_traces then
          TraceMap.add key (SpanMap.singleton span.context.span_id span) spans
        else (
          if not_throttled () then
            debug "%s exceeded max traces when adding to span table"
              __FUNCTION__ ;
          spans
        )
    | Some span_list ->
        if SpanMap.cardinal span_list < Atomic.get max_spans then
          TraceMap.add key
            (SpanMap.add span.context.span_id span span_list)
            spans
        else (
          if not_throttled () then
            debug "%s exceeded max traces when adding to span table"
              __FUNCTION__ ;
          spans
        )

  let add_to_spans ~span = update_spans add_to_spans_unlocked span

  let remove_from_spans_unlocked spans span =
    let key = span.Span.context.trace_id in
    match TraceMap.find_opt key spans with
    | None ->
        if not_throttled () then
          debug "%s span does not exist or already finished" __FUNCTION__ ;
        spans
    | Some span_list ->
        let span_list = SpanMap.remove span.Span.context.span_id span_list in
        if SpanMap.is_empty span_list then
          TraceMap.remove key spans
        else
          TraceMap.add key span_list spans

  let remove_from_spans span =
    update_spans remove_from_spans_unlocked span ;
    Some span

  let rec add_to_finished span =
    let ((spans, n) as old) = Atomic.get finished_spans in
    if n < Atomic.get max_spans then
      let next = (span :: spans, n + 1) in
      if Atomic.compare_and_set finished_spans old next then
        ()
      else (
        Thread.yield () ;
        (add_to_finished [@tailcall]) span
      )
    else if not_throttled () then
      debug "%s exceeded max traces when adding to finished span table"
        __FUNCTION__

  let mark_finished span = Option.iter add_to_finished (remove_from_spans span)

  let empty_finished = ([], 0)

  (** since copies the existing finished spans and then clears the existing spans as to only export them once  *)
  let since () =
    let copy = Atomic.exchange finished_spans empty_finished in
    reset_throttled () ; copy

  let dump () = (Atomic.get spans, Atomic.get finished_spans)

  module GC = struct
    let span_timeout = Atomic.make 86400.
    (* one day in seconds *)

    let span_timeout_thread = ref None

    let gc_inactive_spans_unlocked spans () =
      TraceMap.filter_map
        (fun _ spanlist ->
          let filtered =
            SpanMap.filter_map
              (fun _ span ->
                let elapsed = Unix.gettimeofday () -. span.Span.begin_time in
                if elapsed > Atomic.get span_timeout *. 1000000. then (
                  if not_throttled () then
                    debug "Tracing: Span %s timed out, forcibly finishing now"
                      (Span_id.to_string span.Span.context.span_id) ;
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
          if SpanMap.is_empty filtered then None else Some filtered
        )
        spans

    let gc_inactive_spans () = update_spans gc_inactive_spans_unlocked ()

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

  let no_op =
    {
      name_label= ""
    ; attributes= Attributes.empty
    ; endpoints= []
    ; enabled= false
    }

  let current = Atomic.make no_op

  let get_current () = Atomic.get current

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
        if enabled then Atomic.set current provider
    )

  let get_tracer_providers_unlocked () =
    Hashtbl.fold (fun _ provider acc -> provider :: acc) tracer_providers []

  let get_tracer_providers () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock
      get_tracer_providers_unlocked

  let update_providers_unlocked () =
    let providers = get_tracer_providers_unlocked () in
    match List.find_opt (fun provider -> provider.enabled) providers with
    | None ->
        Atomic.set current no_op ;
        Atomic.set Spans.spans TraceMap.empty ;
        Atomic.set Spans.finished_spans Spans.empty_finished
    | Some enabled ->
        Atomic.set current enabled

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
        update_providers_unlocked ()
    )

  let destroy ~uuid =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let _ = Hashtbl.remove tracer_providers uuid in
        update_providers_unlocked ()
    )
end

let get_observe () = TracerProvider.(get_current ()).enabled

module Tracer = struct
  type t = TracerProvider.t

  let get_tracer ~name:_ = TracerProvider.get_current ()

  let span_of_span_context context name : Span.t =
    {
      context
    ; status= {status_code= Status.Unset; _description= None}
    ; name
    ; parent= None
    ; span_kind= SpanKind.Client (* This will be the span of the client call*)
    ; begin_time= Unix.gettimeofday ()
    ; end_time= None
    ; links= []
    ; events= []
    ; attributes= Attributes.empty
    }

  let start ~tracer:t ?(attributes = []) ?trace_context
      ?(span_kind = SpanKind.Internal) ~name ~parent () :
      (Span.t option, exn) result =
    let open TracerProvider in
    (* Do not start span if the TracerProvider is disabled*)
    if not t.enabled then
      ok_none
    else
      let attributes = Attributes.merge_into t.attributes attributes in
      let span =
        Span.start ~attributes ?trace_context ~name ~parent ~span_kind ()
      in
      Spans.add_to_spans ~span ; Ok (Some span)

  let update_span_with_parent span (parent : Span.t option) =
    if (TracerProvider.get_current ()).enabled then
      match parent with
      | None ->
          Some span
      | Some parent ->
          span
          |> Spans.remove_from_spans
          |> Option.map (fun existing_span ->
                 let old_context = Span.get_context existing_span in
                 let new_context : SpanContext.t =
                   let trace_context = span.Span.context.trace_context in
                   SpanContext.context
                     (SpanContext.trace_id_of_span_context parent.context)
                     old_context.span_id
                   |> SpanContext.with_trace_context trace_context
                 in
                 let updated_span = {existing_span with parent= Some parent} in
                 let updated_span = {updated_span with context= new_context} in

                 let () = Spans.add_to_spans ~span:updated_span in
                 updated_span
             )
    else
      Some span

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

  let span_hashtbl_is_empty () = Spans.span_hashtbl_is_empty ()

  let finished_span_hashtbl_is_empty () =
    Spans.finished_span_hashtbl_is_empty ()
end

let enable_span_garbage_collector ?(timeout = 86400.) () =
  Spans.GC.initialise_thread ~timeout

let with_tracing ?(attributes = []) ?(parent = None) ?trace_context ~name f =
  let tracer = Tracer.get_tracer ~name in
  if tracer.enabled then (
    match Tracer.start ~tracer ?trace_context ~attributes ~name ~parent () with
    | Ok span -> (
      try
        let result = f span in
        ignore @@ Tracer.finish span ;
        result
      with exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        let error = (exn, backtrace) in
        ignore @@ Tracer.finish span ~error ;
        Printexc.raise_with_backtrace exn backtrace
    )
    | Error e ->
        warn "Failed to start tracing: %s" (Printexc.to_string e) ;
        f None
  ) else
    f None

let with_child_trace ?attributes ?trace_context parent ~name f =
  match parent with
  | None ->
      f None
  | Some _ as parent ->
      with_tracing ?attributes ?trace_context ~parent ~name f

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

module Propagator = struct
  module type S = sig
    type carrier

    val traceparent_of : carrier -> Span.t option

    val with_tracing :
         ?attributes:(string * string) list
      -> name:string
      -> carrier
      -> (carrier -> 'a)
      -> 'a
  end

  module type PropS = sig
    type carrier

    val inject_into : TraceContext.t -> carrier -> carrier

    val extract_from : carrier -> TraceContext.t

    val name_span : carrier -> string
  end

  module Make (P : PropS) : S with type carrier = P.carrier = struct
    type carrier = P.carrier

    let traceparent_of carrier =
      (* TODO: The extracted TraceContext must be propagated through the
         spans. Simple approach is to add it to the SpanContext, and then
         inherit it properly (substituting/creating only identity-related). *)
      let ( let* ) = Option.bind in
      let trace_context = P.extract_from carrier in
      let* parent = TraceContext.traceparent_of trace_context in
      let* span_context = SpanContext.of_traceparent parent in
      let span_context =
        SpanContext.with_trace_context trace_context span_context
      in
      let name = P.name_span carrier in
      Some (Tracer.span_of_span_context span_context name)

    let with_tracing ?attributes ~name carrier f =
      let trace_context = P.extract_from carrier in
      let parent = traceparent_of carrier in
      let continue_with_child = function
        | Some child ->
            (* Here, "traceparent" is terminology for the [version-trace_id-span_id-flags] structure.
               Therefore, the purpose of the code below is to decorate the request with the derived (child) span's ID.
               This function only gets called if parent is not None. *)
            let span_context = Span.get_context child in
            let traceparent = SpanContext.to_traceparent span_context in
            let trace_context' =
              TraceContext.with_traceparent (Some traceparent) trace_context
            in
            let carrier' = P.inject_into trace_context' carrier in
            f carrier'
        | _ ->
            f carrier
      in
      with_child_trace ?attributes ~trace_context parent ~name
        continue_with_child
  end
end
