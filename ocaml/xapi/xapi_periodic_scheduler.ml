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

module D = Debug.Make(struct let name="backgroundscheduler" end)
open D

open Stdext.Threadext

type func_ty = OneShot | Periodic of float

type t = {
  func : unit -> unit;
  ty : func_ty;
  name : string;
}

let delay = Delay.make ()

let (queue : (t Ipq.t)) = Ipq.create 50
let lock = Mutex.create ()

module Clock : sig
  (** Oclock represents time in nanoseconds in a 64-bit integer. We
   * can't make this abstract because module Ipq relies
   * on it (for now). Note that the monotonic clock is only good for
   * relative time measurements and the values returned by two different
   * clocks like Oclock.monotonic and Oclock.realtime are unrelated. The
   * underlying mechanism is clock_gettime(2).
   *)
  type t = Int64.t
  val gettime:  unit -> t
  val add:      t -> float -> t
  val sub:      t -> t -> t
  val to_sec:   t -> float
  val (++):     t -> float -> t
end = struct
  type t             = Int64.t
  let clock          = Oclock.monotonic
  let nano           = 1_000_000_000L (* 1E9 *)
  let nano'          = Int64.to_float nano
  let gettime ()     = Oclock.gettime clock
  let add time delay = Int64.(add time (mul (of_float delay) nano))
  let sub t1 t2      = Int64.(sub t1 t2)
  let to_sec delay   = (Int64.to_float delay) /.  nano'
  let (++)           = add
end

let add_to_queue ?(signal=true) name ty start newfunc =
  Mutex.execute lock (fun () ->
      Ipq.(add queue
        { ev={func=newfunc; ty; name}
        ; time=Clock.(gettime () ++ start)
        }));
  if signal then Delay.signal delay

let remove_from_queue name =
  let index = Ipq.find_p queue (fun {name=n} -> name = n) in
  if index > -1 then begin
    Ipq.remove queue index
  end

let loop () =
  debug "Periodic scheduler started";
  try
    while true do
      let empty = Mutex.execute lock (fun () -> Ipq.is_empty queue) in
      if empty
      then
        (Thread.delay 10.0) (* Doesn't happen often - the queue isn't usually empty *)
      else
        begin
          let next = Mutex.execute lock (fun () -> Ipq.maximum queue) in
          let now = Clock.gettime () in
          if next.Ipq.time < now then begin
            let todo = (Mutex.execute lock (fun () -> Ipq.pop_maximum queue)).Ipq.ev in
            (try todo.func () with _ -> ());
            match todo.ty with
            | OneShot -> ()
            | Periodic timer -> add_to_queue ~signal:false todo.name todo.ty timer todo.func
          end else begin
            (* Sleep until next event. *)
            let sleep = Clock.(sub next.Ipq.time now ++ 0.001) in
            try
              ignore(Delay.wait delay (Clock.to_sec sleep))
            with e ->
              let detailed_msg =
                match e with
                | Unix.Unix_error (code, _, _) -> Unix.error_message code
                | _ -> "unknown error"
              in
              error "Could not schedule interruptable delay (%s). Falling back to normal delay. New events may be missed." detailed_msg;
              Thread.delay (Clock.to_sec sleep)
          end
        end
    done
  with _ ->
    error "Periodic scheduler died! Xapi will no longer function well and should be restarted."


