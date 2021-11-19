(*
 * Copyright (C) Citrix Systems Inc.
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

open Xapi_stdext_threads

module D = Debug.Make (struct let name = "scheduler" end)

open D

module PipeDelay = struct
  (* Concrete type is the ends of a pipe *)
  type t = {
      (* A pipe is used to wake up a thread blocked in wait: *)
      pipe_out: Unix.file_descr
    ; pipe_in: Unix.file_descr
  }

  let make () =
    let pipe_out, pipe_in = Unix.pipe () in
    {pipe_out; pipe_in}

  let wait (x : t) (seconds : float) =
    let timeout = if seconds < 0.0 then 0.0 else seconds in
    if Thread.wait_timed_read x.pipe_out timeout then
      (* flush the single byte from the pipe *)
      let (_ : int) = Unix.read x.pipe_out (Bytes.create 1) 0 1 in
      (* return false if we were woken *)
      false
    else
      (* return true if we waited the full length of time, false if we were woken *)
      true

  let signal (x : t) =
    let (_ : int) = Unix.write x.pipe_in (Bytes.of_string "X") 0 1 in
    ()
end

type handle = Mtime.span * int

type handle_compat = int64 * int [@@deriving rpc]

let rpc_of_handle (s, id) = rpc_of_handle_compat (Mtime.Span.to_uint64_ns s, id)

let handle_of_rpc rpc =
  let i64, id = handle_compat_of_rpc rpc in
  (Mtime.Span.of_uint64_ns i64, id)

module HandleMap = Map.Make (struct
  type t = handle

  let compare (x1, id1) (x2, id2) =
    let c = Mtime.Span.compare x1 x2 in
    if c = 0 then
      id2 - id1
    else
      c
end)

type item = {id: int; name: string; fn: unit -> unit}

type t = {
    mutable schedule: item HandleMap.t
  ; delay: PipeDelay.t
  ; mutable next_id: int
  ; m: Mutex.t
}

type time = Delta of int

(*type t = int64 * int [@@deriving rpc]*)

let time_of_span span = span |> Mtime.Span.to_s |> ceil |> Int64.of_float

let mtime_sub time now = Mtime.Span.abs_diff time now |> time_of_span

let now () = Mtime_clock.elapsed ()

module Dump = struct
  type u = {time: int64; thing: string} [@@deriving rpc]

  type dump = u list [@@deriving rpc]

  let make s =
    let now = now () in
    Threadext.Mutex.execute s.m (fun () ->
        HandleMap.fold
          (fun (time, _) i acc ->
            {time= mtime_sub time now; thing= i.name} :: acc
            )
          s.schedule []
    )
end

let mtime_add x t =
  let dt = Mtime.(x *. Mtime.s_to_ns |> Int64.of_float |> Span.of_uint64_ns) in
  Mtime.Span.add dt t

let one_shot_f s dt (name : string) f =
  let time = mtime_add dt (now ()) in
  Threadext.Mutex.execute s.m (fun () ->
      let id = s.next_id in
      s.next_id <- s.next_id + 1 ;
      let item = {id; name; fn= f} in
      let handle = (time, id) in
      s.schedule <- HandleMap.add handle item s.schedule ;
      PipeDelay.signal s.delay ;
      handle
  )

let one_shot s (Delta x) name f = one_shot_f s (float x) name f

let cancel s handle =
  Threadext.Mutex.execute s.m (fun () ->
      s.schedule <- HandleMap.remove handle s.schedule
  )

let process_expired s =
  let t = now () in
  let expired =
    Threadext.Mutex.execute s.m (fun () ->
        let expired, eq, unexpired = HandleMap.split (t, max_int) s.schedule in
        assert (eq = None) ;
        s.schedule <- unexpired ;
        expired |> HandleMap.to_seq |> Seq.map snd
    )
  in
  (* This might take a while *)
  Seq.iter
    (fun i ->
      try i.fn ()
      with e ->
        debug "Scheduler ignoring exception: %s\n%!" (Printexc.to_string e)
      )
    expired ;
  expired () <> Seq.Nil

(* true if work was done *)

let rec main_loop s =
  while process_expired s do
    ()
  done ;
  let sleep_until =
    Threadext.Mutex.execute s.m (fun () ->
        try HandleMap.min_binding s.schedule |> fst |> fst
        with Not_found -> mtime_add 3600. (now ())
    )
  in
  let this = now () in
  let seconds =
    if Mtime.Span.compare sleep_until this > 0 then
      (* be careful that this is absolute difference,
         it is never negative! *)
      Mtime.Span.(abs_diff sleep_until this |> to_s)
    else
      0.
  in
  let (_ : bool) = PipeDelay.wait s.delay seconds in
  main_loop s

let make_scheduler () =
  let s =
    {
      schedule= HandleMap.empty
    ; delay= PipeDelay.make ()
    ; next_id= 0
    ; m= Mutex.create ()
    }
  in
  let (_ : Thread.t) = Thread.create main_loop s in
  s

let make = make_scheduler

module Delay = struct
  type state = Signalled | Timedout

  let s = make_scheduler ()

  type t = {c: Condition.t; m: Mutex.t; mutable state: state option}

  let make () = {c= Condition.create (); m= Mutex.create (); state= None}

  let wait t seconds =
    Threadext.Mutex.execute t.m (fun () ->
        let handle =
          one_shot_f s seconds "Delay.wait" (fun () ->
              if t.state = None then
                t.state <- Some Timedout ;
              Condition.broadcast t.c
          )
        in
        let rec loop () =
          match t.state with
          | Some Timedout ->
              (* return true if we waited the full length of time *)
              true
          | Some Signalled ->
              (* return false if we were woken, or pre-signalled *)
              false
          | None ->
              (* initial wait or spurious wakeup *)
              Condition.wait t.c t.m ; loop ()
        in
        let result = loop () in
        cancel s handle ;
        t.state <- None ;
        result
    )

  let signal t =
    Threadext.Mutex.execute t.m (fun () ->
        t.state <- Some Signalled ;
        Condition.broadcast t.c
    )
end
