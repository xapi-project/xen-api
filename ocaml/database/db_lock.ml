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

module type REENTRANT_LOCK = sig
  type t

  (** Timing statistics modified by each thread after the lock is
      initially acquired. *)
  type statistics = {
      mutable max_time: float
    ; mutable min_time: float
    ; mutable total_time: float
    ; mutable acquires: int
  }

  val create : unit -> t
  (** Creates an instance of a reentrant lock. *)

  val lock : t -> unit
  (** [lock l] acquires the lock [l]. If the calling thread already
      holds the lock, the implementation internally increases the number
      of "holds" the thread has on the lock. Each call to [lock] must
      have a corresponding call to [unlock] or else it is an error. *)

  val unlock : t -> unit
  (** [unlock l] releases a hold on the lock. If the hold count
      becomes 0, the lock is free to be acquired by other threads. It is
      an error to call this from a thread that does not hold the lock. *)

  val statistics : t -> statistics
  (** Returns a copy of the internal timing statistics maintained by
      the implementation. Calling this has the effect of temporarily
      acquiring the lock, as only the lock holder can read or modify the
      internal record. *)
end

(** A simple re-entrant lock (recursive mutex). *)
module ReentrantLock : REENTRANT_LOCK = struct
  type tid = int

  type statistics = {
      mutable max_time: float
    ; mutable min_time: float
    ; mutable total_time: float
    ; mutable acquires: int
  }

  type t = {
      holder: tid option Atomic.t (* The holder of the lock *)
    ; mutable holds: int (* How many holds the holder has on the lock *)
    ; lock: Mutex.t (* Barrier to signal waiting threads *)
    ; condition: Condition.t
          (* Waiting threads are signalled via this condition to reattempt to acquire the lock *)
    ; statistics: statistics (* Bookkeeping of time taken to acquire lock *)
  }

  let create_statistics () =
    {max_time= neg_infinity; min_time= infinity; total_time= 0.; acquires= 0}

  let create () =
    {
      holder= Atomic.make None
    ; holds= 0
    ; lock= Mutex.create ()
    ; condition= Condition.create ()
    ; statistics= create_statistics ()
    }

  let current_tid () = Thread.(self () |> id)

  let[@inline never] [@specialize never] lock_acquired () =
    Xapi_timeslice.Timeslice.lock_acquired ()

  let[@inline never] [@specialize never] lock_released () =
    Xapi_timeslice.Timeslice.lock_released ()

  let lock l =
    let me = current_tid () in
    match Atomic.get l.holder with
    | Some tid when tid = me ->
        l.holds <- l.holds + 1
    | _ ->
        let intended = Some me in
        let counter = Mtime_clock.counter () in
        Mutex.lock l.lock ;
        while not (Atomic.compare_and_set l.holder None intended) do
          Condition.wait l.condition l.lock
        done ;
        lock_acquired () ;
        let stats = l.statistics in
        let delta = Clock.Timer.span_to_s (Mtime_clock.count counter) in
        stats.total_time <- stats.total_time +. delta ;
        stats.min_time <- Float.min delta stats.min_time ;
        stats.max_time <- Float.max delta stats.max_time ;
        stats.acquires <- stats.acquires + 1 ;
        Mutex.unlock l.lock ;
        l.holds <- 1

  let unlock l =
    let me = current_tid () in
    match Atomic.get l.holder with
    | Some tid when tid = me ->
        l.holds <- l.holds - 1 ;
        if l.holds = 0 then (
          let () = Atomic.set l.holder None in
          Mutex.lock l.lock ;
          Condition.signal l.condition ;
          Mutex.unlock l.lock ;
          lock_released ()
        )
    | _ ->
        failwith
          (Printf.sprintf "%s: Calling thread does not hold the lock!"
             __MODULE__
          )

  let statistics l =
    lock l ;
    let stats =
      (* Force a deep copy of the mutable fields *)
      let ({acquires; _} as original) = l.statistics in
      {original with acquires}
    in
    unlock l ; stats
end

(* The top-level database lock that writers must acquire. *)
let db_lock = ReentrantLock.create ()

(* Global flush lock: all db flushes are performed holding this lock *)
(* When we want to prevent the database from being flushed for a period
   (e.g. when doing a host backup in the OEM product) then we acquire this lock *)
let global_flush_mutex = Mutex.create ()

let with_lock f =
  let open Xapi_stdext_pervasives.Pervasiveext in
  ReentrantLock.(
    lock db_lock ;
    finally f (fun () -> unlock db_lock)
  )

type report = {count: int; avg_time: float; min_time: float; max_time: float}

let report () =
  let ReentrantLock.{max_time; min_time; total_time; acquires} =
    ReentrantLock.statistics db_lock
  in
  {
    count= acquires
  ; avg_time= total_time /. float_of_int acquires
  ; min_time
  ; max_time
  }
