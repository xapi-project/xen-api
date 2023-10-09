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

(** a module used to benchmark how long it takes to send and receive an event from the the main thread to a pool of worker threads. *)
module type S = sig
  (** a synchronization primitive *)
  type t

  val allocate : unit -> t
  (** [allocate ()] allocates a synchronization primitive.
    It will be used by threads other than the one that allocated it.
  *)

  val stop : t -> int -> unit
  (** [stop sync idx] stops the [idx]th use of the [sync] primitive. *)

  val free : t -> unit
  (** [free sync] releases [sync]. *)

  val worker : t -> int -> unit
  (** [worker sync idx] is the [idx]th worker thread using [sync] primitive.
    It should loop and receive events on [sync] and send back replies until told to stop.
   *)

  val run : t -> unit
  (** [run sync] is the main benchmark thread using [sync] primitive.
    It should send one event using [sync] and wait for a single reply.
    The benchmark framework will call this multiple times and measure how long it takes.
   *)

  val name : string
  (** [name] the name of this module. Don't use __MODULE__ here because that'll be the name of the entire file, not the submodule. *)
end

let test_pingpong ~args (module T : S) =
  let allocate n =
    let resource = T.allocate () in
    let threads = Array.init n @@ Thread.create @@ T.worker resource in
    (resource, threads, ref false)
  in

  let free (resource, threads, freed) =
    (* older versions of bechamel have a double free bug, detect it! *)
    assert (not !freed) ;
    freed := true ;
    Array.iteri (fun i _ -> T.stop resource i) threads ;
    Array.iter Thread.join threads ;
    T.free resource
  in

  let run (resource, _, _) = T.run resource in

  Test.make_indexed_with_resource ~name:T.name ~args Test.uniq ~allocate ~free
  @@ fun (_ : int) -> Staged.stage run

module TestEvent = struct
  type t = bool Event.channel * unit Event.channel

  let allocate () = (Event.new_channel (), Event.new_channel ())

  let stop (e1, _) (_ : int) = Event.(sync @@ send e1 false)

  let worker (e1, e2) (_ : int) =
    while Event.(sync @@ receive e1) do
      Event.(sync @@ send e2 ())
    done

  let run (e1, e2) =
    Event.(sync (send e1 true)) ;
    Event.(sync (receive e2))

  let free = ignore

  let name = "event"
end

module TestBinSem = struct
  type t = Semaphore.Binary.t * Semaphore.Binary.t * bool Atomic.t

  let name = "binary semaphore"

  let allocate () =
    (Semaphore.Binary.make false, Semaphore.Binary.make false, Atomic.make false)

  let stop (s1, s2, stop) (_ : int) =
    Atomic.set stop true ;
    Semaphore.Binary.release s1 ;
    Semaphore.Binary.acquire s2
  (* important to do this, because a binary semaphore can only be signaled once! *)

  let worker (s1, s2, stop) (_ : int) =
    while not (Atomic.get stop) do
      Semaphore.Binary.acquire s1 ;
      Semaphore.Binary.release s2
    done

  let run (s1, s2, _) =
    Semaphore.Binary.release s1 ;
    Semaphore.Binary.acquire s2

  let free = ignore
end

module TestCountSem = struct
  type t = Semaphore.Counting.t * Semaphore.Counting.t * bool Atomic.t

  let name = "counting semaphore"

  let allocate () =
    (Semaphore.Counting.make 0, Semaphore.Counting.make 0, Atomic.make false)

  let stop (s1, _, stop) (_ : int) =
    Atomic.set stop true ;
    Semaphore.Counting.release s1

  let worker (s1, s2, stop) (_ : int) =
    while not (Atomic.get stop) do
      Semaphore.Counting.acquire s1 ;
      Semaphore.Counting.release s2
    done

  let run (s1, s2, _) =
    Semaphore.Counting.release s1 ;
    Semaphore.Counting.acquire s2

  let free = ignore
end
