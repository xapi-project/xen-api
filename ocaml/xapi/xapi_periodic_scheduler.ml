(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module D = Debug.Make (struct let name = "backgroundscheduler" end)

open D
module Delay = Xapi_stdext_threads.Threadext.Delay

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

type func_ty = OneShot | Periodic of float

type t = {func: unit -> unit; ty: func_ty; name: string}

let delay = Delay.make ()

let (queue : t Ipq.t) = Ipq.create 50

let lock = Mutex.create ()

module Clock = struct
  (** time span of s seconds *)
  let span s = Mtime.Span.of_uint64_ns (Int64.of_float (s *. 1e9))

  let add_span clock secs =
    match Mtime.add_span clock (span secs) with
    | Some t ->
        t
    | None ->
        raise
          Api_errors.(Server_error (internal_error, ["clock overflow"; __LOC__]))
end

let add_to_queue ?(signal = true) name ty start newfunc =
  with_lock lock (fun () ->
      let ( ++ ) = Clock.add_span in
      Ipq.add queue
        {
          Ipq.ev= {func= newfunc; ty; name}
        ; Ipq.time= Mtime_clock.now () ++ start
        }
  ) ;
  if signal then Delay.signal delay

let remove_from_queue name =
  let index = Ipq.find_p queue (fun {name= n; _} -> name = n) in
  if index > -1 then
    Ipq.remove queue index

let wait_next sleep =
  try ignore (Delay.wait delay sleep)
  with e ->
    let detailed_msg =
      match e with
      | Unix.Unix_error (code, _, _) ->
          Unix.error_message code
      | _ ->
          "unknown error"
    in
    error
      "Could not schedule interruptable delay (%s). Falling back to normal \
       delay. New events may be missed."
      detailed_msg ;
    Thread.delay sleep

let loop () =
  debug "Periodic scheduler started" ;
  try
    while true do
      let empty = with_lock lock (fun () -> Ipq.is_empty queue) in
      if empty then
        wait_next 10.0
      (* Doesn't happen often - the queue isn't usually empty *)
      else
        let next = with_lock lock (fun () -> Ipq.maximum queue) in
        let now = Mtime_clock.now () in
        if next.Ipq.time < now then (
          let todo =
            (with_lock lock (fun () -> Ipq.pop_maximum queue)).Ipq.ev
          in
          (try todo.func () with _ -> ()) ;
          match todo.ty with
          | OneShot ->
              ()
          | Periodic timer ->
              add_to_queue ~signal:false todo.name todo.ty timer todo.func
        ) else (* Sleep until next event. *)
          let sleep =
            Mtime.(span next.Ipq.time now)
            |> Mtime.Span.add (Clock.span 0.001)
            |> Scheduler.span_to_s
          in
          wait_next sleep
    done
  with _ ->
    error
      "Periodic scheduler died! Xapi will no longer function well and should \
       be restarted."
