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
open Cohttp
open Logging
open Clock

module StringSet = Set.Make(struct type t = string let compare = String.compare end)

module StringStringRelation = Relation.Make(struct type t = string let compare = compare end)(String)

module Connections = struct
  let t = ref (StringStringRelation.empty)

  let get_session conn_id =
    (* Nothing currently stops you registering multiple sessions per connection *)
    let sessions = StringStringRelation.get_bs conn_id !t in
    if sessions = StringStringRelation.B_Set.empty
    then None
    else Some(StringStringRelation.B_Set.choose sessions)

  let get_origin conn_id = match get_session conn_id with
    | None -> Protocol.Anonymous conn_id
    | Some x -> Protocol.Name x

  let add conn_id session =
    debug "+ connection %s" conn_id;
    t := StringStringRelation.add conn_id session !t

  let remove conn_id =
    debug "- connection %s" conn_id;
    t := StringStringRelation.remove_a conn_id !t

  let is_session_active session =
    StringStringRelation.get_as session !t <> StringStringRelation.A_Set.empty

end

let next_transfer_expected : (string, int64) Hashtbl.t = Hashtbl.create 128
let get_next_transfer_expected name =
  if Hashtbl.mem next_transfer_expected name
  then Some (Hashtbl.find next_transfer_expected name)
  else None
let record_transfer time name =
  Hashtbl.replace next_transfer_expected name time

let snapshot queues =
  let open Protocol.Diagnostics in
  let queues_of =
    List.fold_left (fun acc (n, q)->
        let queue_contents = Q.contents q in
        let next_transfer_expected = get_next_transfer_expected n in
        (n, { queue_contents; next_transfer_expected }) :: acc
      ) [] in
  let all_queues = List.map (fun n -> n, Q.Directory.find queues n)
    (Q.Directory.list queues "") in
  let transient_queues, permanent_queues =
    List.partition (fun (_, q) -> Q.get_owner q <> None) all_queues in
  let current_time = ns () in
  { start_time = 0L; current_time;
    permanent_queues = queues_of permanent_queues;
    transient_queues = queues_of transient_queues }

open Protocol
let process_request conn_id queues session request = match session, request with
  (* Only allow Login, Get, Trace and Diagnostic messages if there is no session *)
  | _, In.Login session ->
    (* associate conn_id with 'session' *)
    Connections.add conn_id session;
    return (queues, Out.Login)
  | _, In.Diagnostics ->
    return (queues, Out.Diagnostics (snapshot queues))
  | _, In.Trace(from, timeout) ->
    lwt events = Trace.get from timeout in
    return (queues, Out.Trace {Out.events = events})
  | _, In.Get path ->
    let path = if path = [] || path = [ "" ] then [ "index.html" ] else path in
    lwt ic = Lwt_io.open_file ~mode:Lwt_io.input (String.concat "/" ("www" :: path)) in
    lwt txt = Lwt_stream.to_string (Lwt_io.read_chars ic) in
    lwt () = Lwt_io.close ic in
    return (queues, Out.Get txt)
  | None, _ ->
    return (queues, Out.Not_logged_in)
  | Some session, In.List prefix ->
    return (queues, Out.List (Q.Directory.list queues prefix))
  | Some session, In.CreatePersistent name ->
    let queues = Q.Directory.add queues name in
    return (queues, Out.Create name)
  | Some session, In.CreateTransient name ->
    let queues = Q.Directory.add queues ~owner:session name in
    return (queues, Out.Create name)
  | Some session, In.Destroy name ->
    let queues = Q.Directory.remove queues name in
    return (queues, Out.Destroy)
  | Some session, In.Ack (name, id) ->
    Trace.add (Event.({time = Unix.gettimeofday (); input = Some session; queue = name; output = None; message = Ack (name, id); processing_time = None }));
    Q.ack queues (name, id)
    >>= fun queues ->
    return (queues, Out.Ack)
  | Some session, In.Transfer { In.from = from; timeout = timeout; queues = names } ->
    let time = Int64.add (ns ()) (Int64.of_float (timeout *. 1e9)) in
    List.iter (record_transfer time) names;
    let from = match from with None -> -1L | Some x -> Int64.of_string x in
    let messages = Q.transfer queues from names in

    let next = match messages with
      | [] -> from
      | x :: xs -> List.fold_left max (snd (fst x)) (List.map (fun x -> snd (fst x)) xs) in
    let transfer = {
      Out.messages = messages;
      next = Int64.to_string next
    } in
    List.iter
      (fun (id, m) ->
         let name = Q.queue_of_id id in
         let processing_time = match m.Message.kind with
           | Message.Request _ -> None
           | Message.Response id' -> begin match Q.entry queues id' with
               | Some request_entry ->
                 Some (Int64.sub (ns ()) request_entry.Entry.time)
               | None ->
                 None
             end in
         Trace.add (Event.({time = Unix.gettimeofday(); input = None; queue = name; output = Some session; message = Message (id, m); processing_time }))
      ) transfer.Out.messages;
    return (queues, Out.Transfer transfer)
  | Some session, In.Send (name, data) ->
    let origin = Connections.get_origin conn_id in
    begin match_lwt Q.send queues origin name data with
      | None -> return (queues, Out.Send None)
      | Some (queues, id) ->
        Trace.add (Event.({time = Unix.gettimeofday (); input = Some session; queue = name; output = None; message = Message (id, data); processing_time = None }));
        return (queues, Out.Send (Some id))
    end
  | Some session, In.Shutdown ->
    info "Received shutdown command";
    let (_: unit Lwt.t) =
      Lwt_unix.sleep 1.
      >>= fun () ->
      exit 0 in
    return (queues, Out.Shutdown)
