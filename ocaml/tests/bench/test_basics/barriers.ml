(*
  Copyright (C) Cloud Software Group
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation; version 2.1 only. with the special
  exception on linking described in file LICENSE.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
*)

open Bechamel

(** Barrier implementation for benchmarking. 
  This is different from the {!mod:Pingpong} test, where we wait for any thread to reply: here we wait for all threads to reply.
  There are various ways to implement barriers, and this tries to determine which one is faster when implemented in OCaml 4.x which has a master lock.
  (The results may be completely different for OCaml 5 when using Domains).
 *)
module type BARRIER = sig
  (** the type of barriers. *)
  type t

  val make : int -> t
  (** [make n] creates a barrier for N threads: it waits until all N threads reach the barrier before allowing any thread to proceed.
    This barrier is reusable and has 2 phases: wait until all threads reach phase 1, then each thread can do some work, and then we wait until all threads reach phase 2.
   *)

  val phase1 : t -> int -> unit
  (** [phase1 barrier i] is the barrier for the [i]th thread.
    It waits until all N other threads have reached it before allowing any of them to proceeed.
    Also called a turnstile.
   *)

  val phase2 : t -> int -> unit
  (** [phase2 barrier i] a turnstile like [phase1] that waits for all N threads to exit the section protected by the barrier. *)

  val wait : t -> unit
  (** [wait barrier] is [phase1] followed by [phase2]. *)

  val name : string
  (** [name] is the name of the implementation. Do not use __MODULE__ here: that'd be the name of the parent module, not the submodule. *)
end

(* See "The Little Book of Semaphores" 3.7 Reusable barrier, 3.7.7 Barrier objects.
    Instead of the mutex+count we use an atomic though.

   There are various ways to implement the synchronization primitive:
     * mutex + condvar
     * single semaphore (which internally uses mutex+condvar)
     * array of semaphores, one for each thread. Technically this is not a barrier (there is no guarantee we have seen all N), but suffices for benchmarking purposes
   (we know the main thread will release the other threads once it has started releasing some)

   Note: array of mutexes wouldn't be a valid implementation because mutexes need to be acquired and released from the same thread.

   The basic principles are similar: on the first turnstile the worker threads wait until it has received N wakeup signals.
   On the 2nd turnstile each worker thread as it reaches it will stop and wait for the main thread to acknowledge it has seen all threads.
*)

module BarrierCond = struct
  module Turnstile = struct
    type t = {m: Mutex.t; mutable state: bool; cond: Condition.t}

    let create state = {m= Mutex.create (); state; cond= Condition.create ()}

    let wait t =
      Mutex.lock t.m ;
      while not t.state do
        Condition.wait t.cond t.m
      done ;
      t.state <- false ;
      Mutex.unlock t.m

    let signal t =
      Mutex.lock t.m ;
      assert (not t.state) ;
      t.state <- true ;
      Condition.signal t.cond ;
      Mutex.unlock t.m
  end

  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Turnstile.t
    ; turnstile2: Turnstile.t
  }

  let name = "barrier(condvars)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Turnstile.create false
    ; turnstile2= Turnstile.create true
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Turnstile.wait t.turnstile2 ;
      Turnstile.signal t.turnstile
    ) ;
    Turnstile.wait t.turnstile ;
    Turnstile.signal t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Turnstile.wait t.turnstile ;
      Turnstile.signal t.turnstile2
    ) ;
    Turnstile.wait t.turnstile2 ;
    Turnstile.signal t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

module BarrierPreloaded = struct
  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Semaphore.Counting.t
    ; turnstile2: Semaphore.Counting.t
  }

  let name = "barrier(semaphores,preloaded)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Semaphore.Counting.make 0
    ; turnstile2= Semaphore.Counting.make 0
    }

  let signal semaphore n =
    for _ = 1 to n do
      Semaphore.Counting.release semaphore
    done

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then
      signal t.turnstile t.n ;
    (* preload the counting semaphore with N wakeups *)
    Semaphore.Counting.acquire t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then
      signal t.turnstile2 t.n ;
    Semaphore.Counting.acquire t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

module BarrierBinary = struct
  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Semaphore.Binary.t
    ; turnstile2: Semaphore.Binary.t
  }

  let name = "barrier(semaphores,binary)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Semaphore.Binary.make false
    ; turnstile2= Semaphore.Binary.make true
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Semaphore.Binary.acquire t.turnstile2 ;
      Semaphore.Binary.release t.turnstile
    ) ;
    Semaphore.Binary.acquire t.turnstile ;
    Semaphore.Binary.release t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Semaphore.Binary.acquire t.turnstile ;
      Semaphore.Binary.release t.turnstile2
    ) ;
    Semaphore.Binary.acquire t.turnstile2 ;
    Semaphore.Binary.release t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

module BarrierCounting = struct
  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Semaphore.Counting.t
    ; turnstile2: Semaphore.Counting.t
  }

  let name = "barrier(semaphores,counting)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Semaphore.Counting.make 0
    ; turnstile2= Semaphore.Counting.make 1
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Semaphore.Counting.acquire t.turnstile2 ;
      Semaphore.Counting.release t.turnstile
    ) ;
    Semaphore.Counting.acquire t.turnstile ;
    Semaphore.Counting.release t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Semaphore.Counting.acquire t.turnstile ;
      Semaphore.Counting.release t.turnstile2
    ) ;
    Semaphore.Counting.acquire t.turnstile2 ;
    Semaphore.Counting.release t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

(* this relies on OCaml per-domain runtime lock allowing only 1 thread at a time to run,
    and therefore we can use Thread.yield to wait for a condition to be reached.

   This is very inefficient, just for comparison to see how bad: we have no control over which thread wakes:
   it could be one that will immediately yield again
*)
module BarrierYield = struct
  module Turnstile : sig
    type t

    val create : bool -> t

    val wait : t -> unit

    val signal : t -> unit
  end = struct
    type t = bool Atomic.t

    let create = Atomic.make

    let wait t =
      while not (Atomic.compare_and_set t true false) do
        Thread.yield ()
      done

    let signal t =
      let ok = Atomic.compare_and_set t false true in
      assert ok
  end

  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Turnstile.t
    ; turnstile2: Turnstile.t
  }

  let name = "barrier(atomic,yield)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Turnstile.create false
    ; turnstile2= Turnstile.create true
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Turnstile.wait t.turnstile2 ;
      Turnstile.signal t.turnstile
    ) ;
    Turnstile.wait t.turnstile ;
    Turnstile.signal t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Turnstile.wait t.turnstile ;
      Turnstile.signal t.turnstile2
    ) ;
    Turnstile.wait t.turnstile2 ;
    Turnstile.signal t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

module BarrierBinaryArray = struct
  module Turnstile = struct
    type t = {start: Semaphore.Binary.t; stop: Semaphore.Binary.t}

    let create _ =
      {start= Semaphore.Binary.make false; stop= Semaphore.Binary.make false}

    let wait_start t = Semaphore.Binary.acquire t.start

    let wake_start t = Semaphore.Binary.release t.start

    let wait_stop t = Semaphore.Binary.acquire t.stop

    let wake_stop t = Semaphore.Binary.release t.stop
  end

  type t = Turnstile.t Array.t

  let name = "barrier(semaphores,binary,array)"

  let make n = Array.init (n - 1) Turnstile.create

  let m = Mutex.create ()
  (*
  let k s =
    let id =  Thread.id (Thread.self ()) in
    Mutex.lock m;
    Format.eprintf "[%d]: %s\n" id s;
    flush stderr;
    Mutex.unlock m
    *)

  let phase1 t i =
    if i < Array.length t then
      (*
      Format.ksprintf k "%d/%d: wait_start@." i (Array.length t);*)
      Turnstile.wait_start t.(i)
    else
      (*
      Format.ksprintf k "%d: start@." i;*)

      (*
      Format.ksprintf k "n: wake_start@.";*)
      Array.iter Turnstile.wake_start t
  (*
      Format.ksprintf k "n: woke_start@.";*)

  let phase2 t i =
    if i < Array.length t then (*
      Format.ksprintf k "%d: wake_stop@." i;*)
      Turnstile.wake_stop t.(i)
    else
      (*
      Format.ksprintf k "%d: woke_stop@." i*)

      (*
      Format.ksprintf k "n: wait_stop@.";*)
      Array.iter Turnstile.wait_stop t (*
      Format.ksprintf k "n: stop@."*)

  let wait t =
    phase1 t (Array.length t) ;
    phase2 t (Array.length t)
end

module TestBarrier (B : BARRIER) = struct
  let make_barrier_threads n =
    let barrier = B.make (n + 1) in
    let stop = Atomic.make false in
    let t =
      Array.init n
      @@ Thread.create
      @@ fun i ->
      let local_stop = ref false in
      while not !local_stop do
        B.phase1 barrier i ;
        if Atomic.get stop then
          local_stop := true ;
        B.phase2 barrier i
      done
      (* Format.eprintf "[%d]: exit\n" (Thread.id (Thread.self ())); flush stderr*)
    in
    (* do one run in the allocation phase to ensure that the threads are all properly started and we don't measure thread spawn overhead *)
    B.wait barrier ;
    (barrier, stop, ref false, t)

  let free_barrier_threads (barrier, stop, freed, threads) =
    assert (not !freed) ;
    freed := true ;
    Atomic.set stop true ;
    B.phase1 barrier (Array.length threads) ;
    B.phase2 barrier (Array.length threads) ;
    Array.iter
      (fun t ->
        (*Format.eprintf "[0]: join %d\n" (Thread.id t);
          flush stderr;*)
        Thread.join t
        (*Format.eprintf "[0]: joined %d\n" (Thread.id t);
          flush stderr;*)
      )
      threads

  let test =
    Test.make_indexed_with_resource ~args:[1; 4; 8; 16] ~name:B.name
      Test.multiple ~allocate:make_barrier_threads ~free:free_barrier_threads
      (fun _ -> Staged.stage @@ fun (barrier, _, _, _) -> B.wait barrier
    )
end

let test_barrier (module B : BARRIER) =
  let module T = TestBarrier (B) in
  T.test
