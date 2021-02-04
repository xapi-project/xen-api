(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt

type message_id = string * int64

module CompactMessage : sig
  type t

  val of_message : Message_switch_core.Protocol.Message.t -> t

  val to_message : t -> Message_switch_core.Protocol.Message.t

  val truncate_at : int ref
end = struct
  open Message_switch_core.Protocol

  let truncate_at = ref 0

  type kind = Message.kind

  type payload = Raw of string | Rpc of Rpc.t

  type t = {payload: payload; kind: Message.kind}

  let to_message t =
    {
      Message.payload=
        (match t.payload with Rpc rpc -> Jsonrpc.to_string rpc | Raw s -> s)
    ; kind= t.kind
    }

  let truncated = "…"

  let truncate s =
    if String.length s < !truncate_at then
      s
    else
      String.sub s 0 !truncate_at ^ truncated

  let rec truncate_rpc = function
    | (Rpc.Int _ | Int32 _ | Bool _ | Float _ | DateTime _ | Null) as orig ->
        orig
    | Rpc.Enum l ->
        Rpc.Enum (List.rev_map truncate_rpc l |> List.rev)
    | Rpc.Base64 s ->
        Rpc.Base64 (truncate s)
    | Rpc.Dict d ->
        Rpc.Dict (List.rev_map truncate_rpc_kv d |> List.rev)
    | Rpc.String s ->
        Rpc.String (truncate s)

  and truncate_rpc_kv (k, v) = (truncate k, truncate_rpc v)

  let of_message m =
    {
      payload=
        ( try Rpc (m.Message.payload |> Jsonrpc.of_string |> truncate_rpc)
          with _ -> Raw (truncate m.Message.payload)
        )
    ; kind= m.kind
    }
end

module CompactEvent : sig
  type t

  val of_event : Message_switch_core.Protocol.Event.t -> t

  val to_event : t -> Message_switch_core.Protocol.Event.t
end = struct
  open Message_switch_core.Protocol

  type message = Message of message_id * CompactMessage.t | Ack of message_id

  let to_message = function
    | Message (id, cm) ->
        Event.Message (id, CompactMessage.to_message cm)
    | Ack id ->
        Ack id

  let of_message = function
    | Event.Message (id, m) ->
        Message (id, CompactMessage.of_message m)
    | Ack id ->
        Ack id

  type t = {
      time: float
    ; input: string option
    ; queue: string
    ; output: string option
    ; message: message
    ; processing_time: int64 option
  }

  let to_event (t : t) =
    {
      Event.time= t.time
    ; input= t.input
    ; queue= t.queue
    ; output= t.output
    ; message= to_message t.message
    ; processing_time= t.processing_time
    }

  let of_event t =
    {
      time= t.Event.time
    ; input= t.input
    ; queue= t.queue
    ; output= t.output
    ; message= of_message t.message
    ; processing_time= t.processing_time
    }
end

type t = (int64 * CompactEvent.t) option array

type config = {trace_entries: int; trace_truncate: int}

let term =
  let open Cmdliner in
  let config trace_entries trace_truncate = {trace_entries; trace_truncate} in
  let trace_entries =
    let doc = "Maximum number of trace entries buffer (for message-cli tail)" in
    Arg.(value & opt int 16 & info ["trace-entries"] ~doc)
  in
  let trace_truncate =
    let doc =
      "Maximum size of strings in trace buffer prior to truncation (for \
       message-cli tail)"
    in
    Arg.(value & opt int 256 & info ["trace-truncate"] ~doc)
  in
  Term.(pure config $ trace_entries $ trace_truncate)

let init config =
  CompactMessage.truncate_at := config.trace_truncate ;
  Array.make config.trace_entries None

let c = Lwt_condition.create ()

let next_id = ref 0L

let add buffer event =
  let size = Array.length buffer in
  let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
  buffer.(next_slot) <- Some (!next_id, CompactEvent.of_event event) ;
  next_id := Int64.succ !next_id ;
  Lwt_condition.broadcast c ()

(* fold [f] over buffered items in chronological order *)
let fold buffer f acc =
  let size = Array.length buffer in
  let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
  let rec range start finish acc =
    if start > finish then
      acc
    else
      range (start + 1) finish (f buffer.(start) acc)
  in
  range 0 (next_slot - 1) (range next_slot (size - 1) acc)

let get buffer from timeout :
    (int64 * Message_switch_core.Protocol.Event.t) list Lwt.t =
  let sleep = Lwt_unix.sleep timeout in
  let rec wait_for_data () =
    if !next_id <= from then
      Lwt_condition.wait ?mutex:(Some Switch_main_helper.m) c >>= wait_for_data
    else
      return ()
  in
  (* Wait until some data is available ie. when next_id > from (or timeout) *)
  Lwt.pick [sleep; wait_for_data ()] >>= fun () ->
  (* start from next_slot, looking for non-None entries which
     	   are > from *)
  let reversed_results =
    fold buffer
      (fun x acc ->
        match x with
        | None ->
            acc
        | Some (id, _) when id < from ->
            acc
        | Some (id, x) ->
            (id, CompactEvent.to_event x) :: acc)
      []
  in
  return (List.rev reversed_results)
