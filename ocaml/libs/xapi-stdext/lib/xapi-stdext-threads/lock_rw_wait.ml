(*
 * Copyright (C) 2026 Cloud Software Group
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

open Xapi_stdext_unix.Lock_rw

type blocked = {by: (int * string) list; until: Clock.Timer.t}

type t = blocked Atomic.t

let create () =
  Atomic.make {by= []; until= Clock.Timer.start ~duration:Mtime.Span.zero}

(* A holder that outlasted a whole wait is likely to keep the lock: later
   waits fail at once rather than each wait for it in turn. Every waiter of
   one lock must apply the same values, so they live here, with the waits *)
let blocked_by_ttl = Mtime.Span.(20 * s)

(* 6 samples over 2 s: a healthy holder waits in the kernel for less than
   that, a wedged driver call for minutes (measured for the host GPU lock:
   about a second). A flock has no holding thread, so any thread counts *)
let stuck_samples = 6

let stuck_interval = Mtime.Span.(400 * ms)

type stuck = {process: int * string; threads: int list}

(* An Error asks Threadext.retry for the next sample; a window cut short by
   [stop] finds none stuck *)
let stuck ~stop blockers first =
  let in_d =
    List.concat_map (fun (b : blocker) ->
        List.map (fun d -> (b.process, d)) b.d_switches
    )
  in
  let samples = ref 0 in
  let stay = ref (in_d first) in
  let look () =
    ( if !samples > 0 then
        let now = in_d (blockers ()) in
        stay := List.filter (fun x -> List.mem x now) !stay
    ) ;
    incr samples ;
    if !stay = [] || !samples = stuck_samples then
      Ok !stay
    else
      Error ()
  in
  let threads stay process =
    List.(map (fun (_, d) -> d.tid) (filter (fun (p, _) -> p = process) stay))
  in
  match Threadext.retry ~delay:stuck_interval ~stop look with
  | Ok stay ->
      List.sort_uniq compare (List.map fst stay)
      |> List.map (fun process -> {process; threads= threads stay process})
  | Error () ->
      []

(* Own holds take turns with the caller: they never make a wait fail fast *)
let others blockers =
  List.(
    map (fun (b : blocker) -> b.process) (filter (fun b -> not b.own) blockers)
  )

let waiter ?(on_stuck = ignore) t span =
  let timer = Clock.Timer.start ~duration:span in
  let stop () = Clock.Timer.has_expired timer in
  let wait ~retry ~blockers =
    let now = blockers () in
    let blocked = Atomic.get t in
    let try_once () =
      match retry () with true -> Ok () | false -> Error `Busy
    in
    let held_up = stuck ~stop blockers now in
    if held_up <> [] then (
      on_stuck held_up ; Error `Stuck
    ) else if
        List.exists (fun b -> List.mem b blocked.by) (others now)
        && not (Clock.Timer.has_expired blocked.until)
      then
      Error `Busy
    else if stop () then
      Error `Busy (* no time left: the caller's try was the only one *)
    else
      Threadext.retry
        ~delay:Mtime.Span.(10 * ms)
        ~max_delay:Mtime.Span.(50 * ms)
        ~stop try_once
      |> Result.map_error (fun busy ->
          let until = Clock.Timer.start ~duration:blocked_by_ttl in
          Atomic.set t {by= others (blockers ()); until} ;
          busy
      )
  in
  {stop; wait}
