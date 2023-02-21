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
module Span = struct
  type t = {
      trace_id: string
    ; span_id: string
    ; span_parent: t option
    ; span_name: string
    ; span_begin_time: float
    ; mutable span_end_time: float option
    ; mutable tags: (string * string) list
  }
  [@@deriving rpcty]

  let generate_id () =
    String.init 16 (fun _ -> "0123456789abcdef".[Random.int 16])

  let start ?(tags = []) ~name ~parent () =
    let trace_id =
      match parent with
      | None ->
          generate_id ()
      | Some span_parent ->
          span_parent.trace_id
    in
    let span_id = generate_id () in
    let span_parent = parent in
    let span_name = name in
    let span_begin_time = Unix.gettimeofday () in
    let span_end_time = None in
    {
      trace_id
    ; span_id
    ; span_parent
    ; span_name
    ; span_begin_time
    ; span_end_time
    ; tags
    }

  let finish ?(tags = []) ~span () =
    span.span_end_time <- Some (Unix.gettimeofday ()) ;
    match (span.tags, tags) with
    | _, [] ->
        ()
    | [], new_tags ->
        span.tags <- new_tags
    | orig_tags, new_tags ->
        span.tags <- orig_tags @ new_tags

  let lock = Mutex.create ()

  let since spans =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let copy = Hashtbl.copy spans in
        Hashtbl.clear spans ; copy
    )
end

let spans = Hashtbl.create 100

let add_to_spans ~(span : Span.t) =
  Xapi_stdext_threads.Threadext.Mutex.execute Span.lock (fun () ->
      let key = span.trace_id in
      match Hashtbl.find_opt spans key with
      | None ->
          Hashtbl.add spans key [span]
      | Some span_list ->
          if List.length span_list < 1000 then
            Hashtbl.replace spans key (span :: span_list)
  )

type blob = Span.t [@@deriving rpcty]

type t = blob option [@@deriving rpcty]

let blob_json x : string =
  Rpcmarshal.marshal blob.Rpc.Types.ty x |> Jsonrpc.to_string

let json_of_t : t -> string option = Option.map blob_json

let string_of_t x : string = Option.value ~default:"(empty)" (json_of_t x)

let t_of_string x : t =
  x
  |> Jsonrpc.of_string
  |> Rpcmarshal.unmarshal blob.Rpc.Types.ty
  |> Result.to_option

let empty : t = None

let null = function None -> true | Some _ -> false

let start ~name ~parent : (t, exn) result =
  let span = Span.start ~name ~parent () in
  add_to_spans ~span ; Ok (Some span)

let finish x : (unit, exn) result =
  match x with None -> Ok () | Some span -> Span.finish ~span () ; Ok ()

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

          let json_of_t s =
            Rpcmarshal.marshal t.Rpc.Types.ty s |> Jsonrpc.to_string
        end

        let zipkin_span_of_span : Span.t -> ZipkinSpan.t =
         fun s ->
          {
            id= s.span_id
          ; traceId= s.trace_id
          ; parentId= Option.map (fun x -> x.Span.span_id) s.span_parent
          ; name= s.span_name
          ; timestamp= int_of_float (s.span_begin_time *. 1000000.)
          ; duration=
              (Option.value s.span_end_time ~default:(Unix.gettimeofday () *. 1000000.)
              -. (s.span_begin_time *. 1000000.)) |> int_of_float
          ; kind= "SERVER"
          ; localEndpoint= {serviceName= "xapi"}
          ; tags= s.tags
          }

        let content_of (span : Span.t) =
          ZipkinSpan.json_of_t (zipkin_span_of_span span)
      end
    end
  end
end
