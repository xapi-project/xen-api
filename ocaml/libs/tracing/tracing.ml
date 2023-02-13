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
module SpanContext = struct
  type t = {trace_id: string; span_id: string} [@@deriving rpcty]
end

module Span = struct
  type t = {
      context: SpanContext.t
    ; parent: t option
    ; name: string
    ; begin_time: float
    ; end_time: float option
    ; tags: (string * string) list
  }
  [@@deriving rpcty]

  let generate_id n = String.init n (fun _ -> "0123456789abcdef".[Random.int 16])

  let start ?(tags = []) ~name ~parent () =
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
    {context; parent; name; begin_time; end_time; tags}

  let finish ?(tags = []) ~span () =
    {span with end_time= Some (Unix.gettimeofday ()); tags= span.tags @ tags}

  let to_string s = Rpcmarshal.marshal t.Rpc.Types.ty s |> Jsonrpc.to_string

  let of_string s =
    Jsonrpc.of_string s
    |> Rpcmarshal.unmarshal t.Rpc.Types.ty
    |> Result.to_option
end

module Spans = struct
  let lock = Mutex.create ()

  let spans = Hashtbl.create 100

  let add_to_spans ~(span : Span.t) =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let key = span.context.trace_id in
        match Hashtbl.find_opt spans key with
        | None ->
            Hashtbl.add spans key [span]
        | Some span_list ->
            if List.length span_list < 1000 then
              Hashtbl.replace spans key (span :: span_list)
    )
end

module Tracer = struct
  let start ~name ~parent : (Span.t option, exn) result =
    let span = Span.start ~name ~parent () in
    Spans.add_to_spans ~span ; Ok (Some span)

  let finish span = Ok (Option.map (fun span -> Span.finish ~span ()) span)
end
