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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
module Delay = Xapi_stdext_threads.Threadext.Delay

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

type func_ty = OneShot | Periodic of float

type t = {func: unit -> unit; ty: func_ty; name: string}

let delay = Delay.make ()

let queue_default = {func= (fun () -> ()); ty= OneShot; name= ""}

let (queue : t Ipq.t) = Ipq.create 50 queue_default

let lock = Mutex.create ()

let add_to_queue_span ?(signal = true) name ty start_span newfunc =
  with_lock lock (fun () ->
      let ( ++ ) = Mtime.Span.add in
      Ipq.add queue
        {
          Ipq.ev= {func= newfunc; ty; name}
        ; Ipq.time= Mtime_clock.elapsed () ++ start_span
        }
  ) ;
  if signal then Delay.signal delay

let add_to_queue ?signal name ty start newfunc =
  let start_span =
    Clock.Timer.s_to_span start |> Option.value ~default:Mtime.Span.max_span
  in
  add_to_queue_span ?signal name ty start_span newfunc

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
  debug "%s started" __MODULE__ ;
  try
    while true do
      let empty = with_lock lock (fun () -> Ipq.is_empty queue) in
      if empty then
        wait_next 10.0
      (* Doesn't happen often - the queue isn't usually empty *)
      else
        let next = with_lock lock (fun () -> Ipq.maximum queue) in
        let now = Mtime_clock.elapsed () in
        if Mtime.Span.is_shorter next.Ipq.time ~than:now then (
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
            Mtime.(Span.abs_diff next.Ipq.time now)
            |> Mtime.Span.(add ms)
            |> Clock.Timer.span_to_s
          in
          wait_next sleep
    done
  with _ ->
    error
      "Scheduler thread died! This daemon will no longer function well and \
       should be restarted."
