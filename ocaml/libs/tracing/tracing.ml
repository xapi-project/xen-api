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
let url_file = "/etc/xapi-tracing-url"

let trace_log_dir = "/var/log/dt/zipkinv2/json"

module D = Debug.Make (struct let name = "tracing" end)

open D

type endpoint = Bugtool | Url of string [@@deriving rpcty]

let endpoint_of_string = function "bugtool" -> Bugtool | url -> Url url

module SpanContext = struct
  type t = {trace_id: string; span_id: string} [@@deriving rpcty]

  let to_traceparent t = Printf.sprintf "00-%s-%s-00" t.trace_id t.span_id

  let of_traceparent traceparent =
    let elements = String.split_on_char '-' traceparent in
    match elements with
    | ["00"; trace_id; span_id; "00"] ->
        Some {trace_id; span_id}
    | _ ->
        None
end

module Span = struct
  type t = {
      span_context: SpanContext.t
    ; span_parent: t option
    ; span_name: string
    ; span_begin_time: float
    ; mutable span_end_time: float option
    ; mutable tags: (string * string) list
  }
  [@@deriving rpcty]

  let get_span_context t = t.span_context

  let generate_id n = String.init n (fun _ -> "0123456789abcdef".[Random.int 16])

  let start ?(tags = []) ~name ~parent () =
    let trace_id =
      match parent with
      | None ->
          generate_id 32
      | Some span_parent ->
          span_parent.span_context.trace_id
    in
    let span_id = generate_id 16 in
    let span_context : SpanContext.t = {trace_id; span_id} in
    let span_parent = parent in
    let span_name = name in
    let span_begin_time = Unix.gettimeofday () in
    let span_end_time = None in
    {span_context; span_parent; span_name; span_begin_time; span_end_time; tags}

  let finish ?(tags = []) ~span () =
    span.span_end_time <- Some (Unix.gettimeofday ()) ;
    match (span.tags, tags) with
    | _, [] ->
        ()
    | [], new_tags ->
        span.tags <- new_tags
    | orig_tags, new_tags ->
        span.tags <- orig_tags @ new_tags
end

module Spans = struct
  let lock = Mutex.create ()

  let spans = Hashtbl.create 100

  let add_to_spans ~(span : Span.t) =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let key = span.span_context.trace_id in
        match Hashtbl.find_opt spans key with
        | None ->
            Hashtbl.add spans key [span]
        | Some span_list ->
            if List.length span_list < 1000 then
              Hashtbl.replace spans key (span :: span_list)
    )

  let since () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let copy = Hashtbl.copy spans in
        Hashtbl.clear spans ; copy
    )
end

type provider_config_t = {
    name_label: string
  ; tags: (string * string) list
  ; endpoints: endpoint list
  ; filters: string list
  ; processors: string list
  ; enabled: bool
}
[@@deriving rpcty]

module Tracer = struct
  type t = {name: string; provider: provider_config_t ref}

  let create ~name ~provider = {name; provider}

  let span_of_span_context span_context span_name : Span.t =
    {
      span_context
    ; span_name
    ; span_parent= None
    ; span_begin_time= Unix.gettimeofday ()
    ; span_end_time= None
    ; tags= []
    }

  let get_empty name =
    warn "Tracer of name %s was not found" name ;
    let provider =
      ref
        {
          name_label= ""
        ; tags= []
        ; endpoints= []
        ; filters= []
        ; processors= []
        ; enabled= false
        }
    in
    {name= ""; provider}

  let start ~tracer:t ~name ~parent : (Span.t option, exn) result =
    let provider = !(t.provider) in
    (* Do not start span if the TracerProvider is diabled*)
    if not provider.enabled then
      Ok None
    else
      let tags = provider.tags in
      let span = Span.start ~tags ~name ~parent () in
      Spans.add_to_spans ~span ; Ok (Some span)

  let finish x : (unit, exn) result =
    match x with None -> Ok () | Some span -> Span.finish ~span () ; Ok ()
end

module TracerProvider = struct
  type t = {tracers: Tracer.t list; config: provider_config_t}

  let find_tracer ~provider:t ~name =
    match
      List.filter (fun (tracer : Tracer.t) -> tracer.name = name) t.tracers
    with
    | [tracer] ->
        tracer
    | _ ->
        Tracer.get_empty name
end

module TracerProviders = struct
  let lock = Mutex.create ()

  let tracer_providers = Hashtbl.create 100

  let find_or_create_tracer ~provider ~name =
    match
      List.filter
        (fun tracer -> tracer.Tracer.name = name)
        provider.TracerProvider.tracers
    with
    | provider :: _ ->
        provider
    | _ ->
        Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
            let label = provider.TracerProvider.config.name_label in
            let tracer =
              Tracer.create ~name ~provider:(ref provider.TracerProvider.config)
            in
            let provider =
              {provider with tracers= tracer :: provider.TracerProvider.tracers}
            in
            Hashtbl.replace tracer_providers label provider ;
            tracer
        )

  let set_default ~tags ~endpoints ~processors ~filters =
    let endpoints = List.map endpoint_of_string endpoints in
    let default : TracerProvider.t =
      {
        tracers= []
      ; config=
          {
            name_label= "default"
          ; tags= ("provider", "default") :: tags
          ; endpoints
          ; filters
          ; processors
          ; enabled= true
          }
      }
    in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.replace tracer_providers "default" default
    )

  let get_default () =
    try
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
          let provider = Hashtbl.find tracer_providers "default" in
          Ok provider
      )
    with e -> Error e

  let get_default_tracer ~name =
    let default = get_default () in
    match default with
    | Ok provider ->
        find_or_create_tracer ~provider ~name
    | Error e ->
        warn "Error fetching default provider: %s " (Printexc.to_string e) ;
        find_or_create_tracer
          ~provider:
            {
              tracers= []
            ; config=
                {
                  name_label= ""
                ; tags= []
                ; endpoints= []
                ; filters= []
                ; processors= []
                ; enabled= false
                }
            }
          ~name
end

type blob = Span.t [@@deriving rpcty]

type t = blob option [@@deriving rpcty]

let t_to_string_opt =
  Option.map (fun s ->
      Rpcmarshal.marshal blob.Rpc.Types.ty s |> Jsonrpc.to_string
  )

let t_of_string x : t =
  x
  |> Jsonrpc.of_string
  |> Rpcmarshal.unmarshal blob.Rpc.Types.ty
  |> Result.to_option

let empty : t = None

let is_empty = function None -> true | Some _ -> false

module Export = struct
  module Content = struct
    module Json = struct
      module Zipkinv2 = struct
        module ZipkinSpan = struct
          type localEndpoint = {serviceName: string} [@@deriving rpcty]

          type t = {
              id: string
            ; traceId: string
            ; parentId: string option
            ; name: string
            ; timestamp: int
            ; duration: int
            ; kind: string
            ; localEndpoint: localEndpoint
            ; tags: (string * string) list
          }
          [@@deriving rpcty]

          type t_list = t list [@@deriving rpcty]

          let json_of_t_list s =
            Rpcmarshal.marshal t_list.Rpc.Types.ty s |> Jsonrpc.to_string
        end

        let zipkin_span_of_span (s : Span.t) : ZipkinSpan.t =
          let tags =
            List.assoc_opt "host" s.tags
            |> Option.fold ~none:s.tags ~some:(fun uuid ->
                   ("instance", uuid) :: s.tags
               )
          in
          {
            id= s.span_context.span_id
          ; traceId= s.span_context.trace_id
          ; parentId=
              Option.map (fun x -> x.Span.span_context.span_id) s.span_parent
          ; name= s.span_name
          ; timestamp= int_of_float (s.span_begin_time *. 1000000.)
          ; duration=
              Option.value s.span_end_time ~default:(Unix.gettimeofday ())
              -. s.span_begin_time
              |> ( *. ) 1000000.
              |> int_of_float
          ; kind= "SERVER"
          ; localEndpoint= {serviceName= "xapi"}
          ; tags
          }

        let content_of (spans : Span.t list) =
          List.map zipkin_span_of_span spans |> ZipkinSpan.json_of_t_list
      end
    end
  end

  module Destination = struct
    module File = struct
      let export ~trace_id ~span_json : (string, exn) result =
        try
          (* TODO: *)
          let host_id = "myhostid" in
          let timestamp = Unix.gettimeofday () |> string_of_float in
          let microsec = String.sub timestamp 6 (String.length timestamp - 6) in
          let unix_time = float_of_string timestamp |> Unix.localtime in
          let date =
            Printf.sprintf "%d%d%d-%d%d%d-%s" unix_time.tm_year unix_time.tm_mon
              unix_time.tm_mday unix_time.tm_hour unix_time.tm_min
              unix_time.tm_sec microsec
          in
          let file =
            String.concat "/" [trace_log_dir; trace_id; "xapi"; host_id; date]
            ^ ".json"
          in
          Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname file) 0o700 ;
          Xapi_stdext_unix.Unixext.write_string_to_file file span_json ;
          (* TODO: Ensure file is only written when trace is complete *)
          Ok ""
        with e -> Error e
    end

    module Http = struct
      module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
      module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

      let export ~span_json ~url : (string, exn) result =
        try
          let body = span_json in
          let uri = Uri.of_string url in
          let headers =
            Cohttp.Header.of_list
              [
                ("accepts", "application/json")
              ; ("content-type", "application/json")
              ; ("content-length", string_of_int (String.length body))
              ]
          in
          Open_uri.with_open_uri uri (fun fd ->
              let request =
                Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri
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

    let export_to_http_server () =
      debug "Tracing: About to export to http server" ;
      let url =
        Xapi_stdext_unix.Unixext.string_of_file url_file |> String.trim
      in
      let span_list = Spans.since () in
      try
        Hashtbl.iter
          (fun _ span_list ->
            let zipkin_spans = Content.Json.Zipkinv2.content_of span_list in
            match Http.export ~span_json:zipkin_spans ~url with
            | Ok _ ->
                ()
            | Error e ->
                raise e
          )
          span_list ;
        Ok ()
      with e -> Error e

    let _export_to_logs () =
      debug "Tracing: About to export to dom0 logs" ;
      let span_list = Spans.since () in
      try
        Hashtbl.iter
          (fun trace_id span_list ->
            let zipkin_spans = Content.Json.Zipkinv2.content_of span_list in
            match File.export ~trace_id ~span_json:zipkin_spans with
            | Ok _ ->
                ()
            | Error e ->
                raise e
          )
          span_list ;
        Ok ()
      with e -> Error e

    let _ =
      Thread.create
        (fun () ->
          while true do
            debug "Tracing: Waiting 30s before exporting spans" ;
            Thread.delay 30. ;
            let export_span =
              Span.start ~name:"export_to_http_server" ~parent:None ()
            in
            match export_to_http_server () with
            | Ok () ->
                Span.finish ~span:export_span ()
            | Error e ->
                debug "Tracing: ERROR %s" (Printexc.to_string e) ;
                Span.finish ~span:export_span
                  ~tags:[("exception.message", Printexc.to_string e)]
                  ()
          done
        )
        ()
  end
end
