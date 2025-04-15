(*
 * Copyright (C) Cloud Software Group
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

(* avoid allocating an extra option every time *)
let invalid_holder = -1

let last_lock_holder = Atomic.make invalid_holder

let me () = Thread.self () |> Thread.id

let lock_acquired () =
  (* these need to be very low overhead, so just keep track of the last lock holder,
     i.e. track only one high-priority lock at a time
  *)
  Atomic.set last_lock_holder (me ())

let lock_released () = Atomic.set last_lock_holder invalid_holder

let[@inline always] am_i_holding_locks () =
  let last = Atomic.get last_lock_holder in
  last <> invalid_holder && last = me ()

let yield_interval = Atomic.make 0

(* TODO: use bechamel.monotonic-clock instead, which has lower overhead,
   but not in the right place in xs-opam yet
*)
let last_yield = Atomic.make 0

let thread_last_yield = Atomic.make 0

let failures = Atomic.make 0

let[@inline always] with_time_counter_now time_counter f args =
  let now = Monotonic_clock.now () |> Int64.to_int in
  Atomic.set time_counter now ;
  f args

module Runtime = struct
  let epoch_count = Atomic.make 0

  let maybe_thread_yield ~global_slice_period =
    let open Xapi_stdext_threads.Threadext in
    let thread_ctx = ThreadRuntimeContext.get () in
    let tgroup = thread_ctx.tgroup |> Tgroup.group_of_description in
    match tgroup with
    | None ->
        ()
    | Some tgroup ->
        let current_epoch = Atomic.get epoch_count in
        ( if current_epoch <> thread_ctx.tepoch then
            (* thread remembers that it is about to run in a new epoch *)
            let () = thread_ctx.time_running <- 0 in
            thread_ctx.tepoch <- current_epoch
          else
            (* thread remembers how long it is  running  in the current epoch *)
            let time_running_since_last_yield =
              (Monotonic_clock.now () |> Int64.to_int)
              - Atomic.get thread_last_yield
            in
            let time_running =
              thread_ctx.time_running + time_running_since_last_yield
            in
            thread_ctx.time_running <- time_running
        ) ;

        let sleep_or_yield sleep_time (tgroup : Tgroup.t) =
          (*todo: do not sleep if this is the last thread in the tgroup(s) *)
          if tgroup.group_descr = Tgroup.Description.authenticated_root then
            with_time_counter_now thread_last_yield Thread.yield ()
          else
            with_time_counter_now thread_last_yield Unix.sleepf sleep_time
        in
        let is_to_sleep_or_yield delay_s =
          if delay_s > 0. then
            Tgroup.with_one_fewer_thread_in_tgroup tgroup
              (sleep_or_yield delay_s)
        in

        (* fair scheduling decision to check if thread time_running has exceeded
           tgroup-mandated ideal time per thread *)
        if thread_ctx.time_running > tgroup.time_ideal then
          let since_last_global_slice =
            (Monotonic_clock.now () |> Int64.to_int) - Atomic.get last_yield
          in
          let until_next_global_slice =
            if since_last_global_slice < global_slice_period then
              global_slice_period - since_last_global_slice
            else
              0
          in
          let thread_delay_s =
            (until_next_global_slice |> float_of_int) *. 1e-9
          in
          is_to_sleep_or_yield thread_delay_s

  let incr_epoch ~frequency =
    let epoch = epoch_count |> Atomic.get in
    if epoch mod frequency = 0 then
      Atomic.set epoch_count 1
    else
      Atomic.incr epoch_count

  let sched_global_slice ~global_slice_period =
    (*refresh the eopch counter roughly every 10s for timeslices of 10ms*)
    incr_epoch ~frequency:1024 ;

    (* goal is to recalculate thread.time_ideal for each thread: *)
    (* 1) fairness: each thread group get the same amount of time inside the slice  *)
    (* 2) control : each thread group time is then weighted by its tgroup_share     *)
    (* 3) delegate: later, asynchronously, each thread decides to maybe yield based
       on its thread group idea of ideal time per thread *)
    (* delegation via tgroups minimizes the number of synchronous global writes
       here from O(threads) to O(groups) *)
    let time_ideal_of_tgroups groups =
      Tgroup.(
        let group_share_total =
          groups |> List.fold_left (fun xs x -> x.tgroup_share + xs) 0
        in
        groups
        |> List.iter (fun g ->
               let group_share_ratio =
                 match group_share_total with
                 | 0 ->
                     0.
                 | gst ->
                     (g.tgroup_share |> float_of_int) /. (gst |> float_of_int)
               in
               let group_time_ns =
                 group_share_ratio *. (global_slice_period |> float_of_int)
               in
               let thread_time_ideal =
                 match g.thread_count |> Atomic.get with
                 | 0 ->
                     0.
                 | gnt ->
                     group_time_ns /. (gnt |> float_of_int)
               in
               g.time_ideal <- thread_time_ideal |> int_of_float
           )
      )
    in
    let tgroups_with_threads =
      List.fold_left
        (fun xs x ->
          if x.Tgroup.thread_count |> Atomic.get > 0 then
            x :: xs
          else
            xs
        )
        [] (Tgroup.tgroups ())
    in
    (* reserve cpu time only to tgroups that have threads to run at the moment *)
    tgroups_with_threads |> time_ideal_of_tgroups
end

let periodic_hook (_ : Gc.Memprof.allocation) =
  let () =
    try
      let yield_interval = Atomic.get yield_interval in
      if !Constants.tgroups_enabled && !Constants.runtime_sched then
        if not (am_i_holding_locks ()) then
          let elapsed =
            (Monotonic_clock.now () |> Int64.to_int) - Atomic.get last_yield
          in
          if elapsed > yield_interval then (
            let now = Monotonic_clock.now () |> Int64.to_int in
            Atomic.set last_yield now ;
            Atomic.set thread_last_yield now ;
            Runtime.sched_global_slice ~global_slice_period:yield_interval ;
            Thread.yield ()
          ) else
            Runtime.maybe_thread_yield ~global_slice_period:yield_interval
        else
          ()
      else if not (am_i_holding_locks ()) then
        let elapsed =
          (Monotonic_clock.now () |> Int64.to_int) - Atomic.get last_yield
        in
        if elapsed > yield_interval then
          with_time_counter_now last_yield Thread.yield ()
    with _ ->
      (* It is not safe to raise exceptions here, it'd require changing all code to be safe to asynchronous interrupts/exceptions,
         see https://guillaume.munch.name/software/ocaml/memprof-limits/index.html#isolation
         Because this is just a performance optimization, we fall back to safe behaviour: do nothing, and just keep track that we failed
      *)
      Atomic.incr failures
  in
  None

let periodic =
  Gc.Memprof.
    {null_tracker with alloc_minor= periodic_hook; alloc_major= periodic_hook}

let set ?(sampling_rate = 1e-4) interval =
  Atomic.set yield_interval (interval *. 1e9 |> int_of_float) ;
  Gc.Memprof.start ~sampling_rate ~callstack_size:0 periodic

let clear () =
  Gc.Memprof.stop () ;
  Atomic.set yield_interval 0
