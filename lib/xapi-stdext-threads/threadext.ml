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

module Mutex = struct
  include Mutex

  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock;
    Xapi_stdext_pervasives.Pervasiveext.finally f (fun () -> Mutex.unlock lock)
end


module Alarm = struct

  type t =
    { token: Mutex.t ;
      mutable queue: (float * (unit -> unit)) list ;
      mutable notifier: (Unix.file_descr * Unix.file_descr) option ;
    }

  let create () =
    { token = Mutex.create () ;
      queue = [] ;
      notifier = None ;
    }

  let global_alarm = create ()

  let rec watch alarm =
    match alarm.notifier with
    | None -> assert false
    | Some (pipe_in, pipe_out) ->
      while Thread.wait_timed_read pipe_in 0. do
        ignore (Unix.read pipe_in (Bytes.create 1) 0 1)
      done;
      let next = Mutex.execute alarm.token
          (fun () ->
             let now = Unix.time () in
             let nqueue = List.filter
                 (fun (clock, callback) ->
                    (* Create helper thread in case callback could block us *)
                    clock > now || (let _ = Thread.create callback () in false))
                 alarm.queue in
             alarm.queue <- nqueue;
             match nqueue with
             | [] ->
               Unix.close pipe_out;
               Unix.close pipe_in;
               alarm.notifier <- None;
               None
             | (c, _) :: _ ->
               Some c) in
      match next with
      | None -> Thread.exit ()
      | Some c ->
        let now = Unix.time () in
        if c > now then ignore (Thread.wait_timed_read pipe_in (c -. now));
        watch alarm

  let register ?(alarm = global_alarm) time callback =
    Mutex.execute alarm.token
      (fun () ->
         let nqueue = (time, callback) :: alarm.queue in
         alarm.queue <- List.sort (fun x1 x2 -> compare (fst x1) (fst x2)) nqueue;
         match alarm.notifier with
         | Some (_, pipe_out) ->
           ignore (Unix.write pipe_out (Bytes.of_string "X") 0 1)
         | None ->
           let pipe_in, pipe_out = Unix.pipe () in
           alarm.notifier <- Some (pipe_in, pipe_out);
           ignore (Thread.create watch alarm))
end

module Thread = struct

  type t =
    | Running of Thread.t
    | Pending of pthread
  and pthread = float * int * Thread.t lazy_t

  type schedule = Now | Timeout of float | Indefinite

  type policy =
    | AlwaysRun
    | MaxCapacity of int * float option
    | WaitCondition of (unit -> schedule)

  let count = ref 0

  module PQueue = Set.Make(struct type t = pthread let compare = compare end)

  let running = ref 0

  let pqueue = ref PQueue.empty

  (* This info can be deduced from pqueue, but having a specific int val allow
     	   us to inspect it with lower cost and be lock free *)
  let pending = ref 0

  let running_threads () = !running

  let pending_threads () = !pending

  let scheduler_token = Mutex.create ()

  let policy = ref AlwaysRun

  (* Should be protected by scheduler_token *)
  let run_thread ((_, _, pt) as t) =
    (* Might have run by other scheduling policy *)
    if PQueue.mem t !pqueue then
      (pqueue := PQueue.remove t !pqueue; decr pending);
    if not (Lazy.is_val pt) then
      let _ = Lazy.force pt in
      incr running

  let fake_pivot = max_float, 0, lazy (Thread.create ignore ())
  let pivot = ref fake_pivot
  let pre_pivot = ref max_int

  (* Should be protected by scheduler_token, this could be triggered either
     	   because a thread finishes running and hence possibly provide an running
     	   slot, or the scheduling policy has been updated hence more oppotunities
     	   appear.  *)
  let rec run_pendings () =
    if not (PQueue.is_empty !pqueue) then
      let now = Unix.time() in
      let (c, _, _) as t = PQueue.min_elt !pqueue in
      (* Just in case policy has been changed *)
      let to_run = match !policy with
        | AlwaysRun -> true
        | MaxCapacity (max_threads, _) -> c <= now || !running < max_threads
        | WaitCondition f -> f () = Now in
      if to_run then (run_thread t; run_pendings ())
      else (* extra logic to avoid starvation or wrongly programmed deadlock *)
        let timeouts, exist, indefs = PQueue.split !pivot !pqueue in
        if not exist || (PQueue.cardinal timeouts >= !pre_pivot
                         && (run_thread !pivot; true)) then
          pivot :=
            if PQueue.is_empty indefs then fake_pivot
            else PQueue.min_elt indefs;
        pre_pivot := PQueue.cardinal timeouts

  let exit () =
    Mutex.execute scheduler_token
      (fun () -> decr running; run_pendings ());
    Thread.exit ()

  let set_policy p =
    Mutex.execute scheduler_token
      (fun () ->
         policy := p;
         run_pendings ())

  let create ?(schedule=Indefinite) f x =
    let finally = Xapi_stdext_pervasives.Pervasiveext.finally in
    let f' x =
      finally
        (fun () -> f x)
        exit in
    Mutex.execute scheduler_token
      (fun () ->
         run_pendings ();
         let timeout = match schedule with
           | Now -> 0.
           | Timeout t -> t
           | Indefinite -> max_float in
         let timeout =
           if timeout = 0. then 0. else
             match !policy with
             | AlwaysRun -> 0.
             | MaxCapacity (max_threads, max_wait_opt) ->
               if !running < max_threads && PQueue.is_empty !pqueue then 0.
               else begin match max_wait_opt with
                 | None -> timeout
                 | Some t -> min timeout t end
             | WaitCondition f -> match f () with
               | Now -> 0.
               | Timeout t -> min t timeout
               | Indefinite -> timeout in
         if timeout <= 0. then
           let t = Thread.create f' x in
           incr running;
           Running t
         else
           let deadline = 
             if timeout < max_float then timeout +. Unix.time()
             else max_float in
           let pt = lazy (Thread.create f' x) in
           incr count;
           if !count = max_int then count := 0;
           let t = (deadline, !count, pt) in
           pqueue := PQueue.add t !pqueue;
           incr pending;
           if deadline < max_float then
             Alarm.register deadline
               (fun () -> Mutex.execute scheduler_token
                   (fun () -> run_thread t));
           (* It's fine that a pended thread might get scheduled later on so
              					    that the information held in 't' becomes meaningless. This is
              					    comparable to the case that a Thread.t finishes running and its
              					    thread id still exits.
              					 *)
           Pending t)

  let self () =
    (* When we get here, the thread must be running *)
    Running (Thread.self ())

  let id = function
    | Running t -> Thread.id t
    | Pending (_, id, _) ->
      (* Pending thread have a negative id to avoid overlapping with running
         			     thread id *)
      -id

  let join = function
    | Running t -> Thread.join t
    | Pending ((_, _, pt) as t) ->
      if not (Lazy.is_val pt) then begin
        (* Give priority to those to be joined *)
        Mutex.execute scheduler_token (fun () -> run_thread t);
        assert (Lazy.is_val pt);
      end;
      Thread.join (Lazy.force pt)

  let kill = function
    | Running t ->
      (* Not implemented in stdlib *)
      Thread.kill t
    | Pending ((_, _, pt) as t) ->
      if Lazy.is_val pt then
        Thread.kill (Lazy.force pt)
      else
        Mutex.execute scheduler_token
          (fun () ->
             (* Just in case something happens before we grab the lock *)
             if Lazy.is_val pt then Thread.kill (Lazy.force pt)
             else (pqueue := PQueue.remove t !pqueue; decr pending))

  let delay = Thread.delay
  let exit = Thread.exit
  let wait_read = Thread.wait_read
  let wait_write = Thread.wait_write
  let wait_timed_read = Thread.wait_timed_read
  let wait_timed_write = Thread.wait_timed_write
  let wait_pid = Thread.wait_pid
  let select = Thread.select
  let yield = Thread.yield
  let sigmask = Thread.sigmask
  let wait_signal = Thread.wait_signal
end


(** create thread loops which periodically applies a function *)
module Thread_loop
  : functor (Tr : sig type t val delay : unit -> float end) ->
  sig
    val start : Tr.t -> (unit -> unit) -> unit
    val stop : Tr.t -> unit
    val update : Tr.t -> (unit -> unit) -> unit
  end
  = functor (Tr: sig type t val delay : unit -> float end) -> struct

    exception Done_loop
    let ref_table : ((Tr.t,(Mutex.t * Thread.t * bool ref)) Hashtbl.t) =
      Hashtbl.create 1

    (** Create a thread which periodically applies a function to the
        reference specified, and exits cleanly when removed *) 
    let start xref fn =
      let mut = Mutex.create () in
      let exit_var = ref false in
      (* create thread which periodically applies the function *)
      let tid = Thread.create (fun () ->
          try while true do
              Thread.delay (Tr.delay ());
              Mutex.execute mut (fun () ->
                  if !exit_var then
                    raise Done_loop;
                  let () = fn () in ()
                );
            done; with Done_loop -> ();
        ) () in
      (* create thread to manage the reference table and clean it up
         safely once the delay thread is removed *)
      let _ = Thread.create (fun () ->
          Hashtbl.add ref_table xref (mut,tid,exit_var);
          Thread.join tid;
          List.iter (fun (_,t,_) ->
              if tid = t then Hashtbl.remove ref_table xref
            ) (Hashtbl.find_all ref_table xref)
        ) () in ()

    (** Remove a reference from the thread table *)
    let stop xref =
      try let mut,_,exit_ref = Hashtbl.find ref_table xref in
        Mutex.execute mut (fun () -> exit_ref := true)
      with Not_found -> ()

    (** Replace a thread with another one *)
    let update xref fn =
      stop xref;
      start xref fn
  end

(** Parallel List.iter. Remembers all exceptions and returns an association list mapping input x to an exception.
    Applications of x which succeed will be missing from the returned list. *)
let thread_iter_all_exns f xs = 
  let exns = ref [] in
  let m = Mutex.create () in
  List.iter 
    Thread.join 
    (List.map 
       (fun x -> 
          Thread.create 
            (fun () ->  
               try
                 f x
               with e -> Mutex.execute m (fun () -> exns := (x, e) :: !exns)
            )
            ()
       ) xs);
  !exns

(** Parallel List.iter. Remembers one exception (at random) and throws it in the 
    error case. *)
let thread_iter f xs = match thread_iter_all_exns f xs with
  | [] -> ()
  | (_, e) :: _ -> raise e

module Delay = struct
  (* Concrete type is the ends of a pipe *)
  type t = { 
    (* A pipe is used to wake up a thread blocked in wait: *)
    mutable pipe_out: Unix.file_descr option;
    mutable pipe_in: Unix.file_descr option;
    (* Indicates that a signal arrived before a wait: *)
    mutable signalled: bool;
    m: Mutex.t
  }

  let make () = 
    { pipe_out = None;
      pipe_in = None;
      signalled = false;
      m = Mutex.create () }

  exception Pre_signalled

  let wait (x: t) (seconds: float) =
    let finally = Xapi_stdext_pervasives.Pervasiveext.finally in
    let to_close = ref [ ] in
    let close' fd = 
      if List.mem fd !to_close then Unix.close fd;
      to_close := List.filter (fun x -> fd <> x) !to_close in
    finally
      (fun () ->
         try
           let pipe_out = Mutex.execute x.m
               (fun () ->
                  if x.signalled then begin
                    x.signalled <- false;
                    raise Pre_signalled;
                  end;
                  let pipe_out, pipe_in = Unix.pipe () in
                  (* these will be unconditionally closed on exit *)
                  to_close := [ pipe_out; pipe_in ];
                  x.pipe_out <- Some pipe_out;
                  x.pipe_in <- Some pipe_in;
                  x.signalled <- false;
                  pipe_out) in
           let r, _, _ = Unix.select [ pipe_out ] [] [] seconds in
           (* flush the single byte from the pipe *)
           if r <> [] then ignore(Unix.read pipe_out (Bytes.create 1) 0 1);
           (* return true if we waited the full length of time, false if we were woken *)
           r = []
         with Pre_signalled -> false
      )
      (fun () -> 
         Mutex.execute x.m
           (fun () ->
              x.pipe_out <- None;
              x.pipe_in <- None;
              List.iter close' !to_close)
      )

  let signal (x: t) = 
    Mutex.execute x.m
      (fun () ->
         match x.pipe_in with
         | Some fd -> ignore(Unix.write fd (Bytes.of_string "X") 0 1)
         | None -> x.signalled <- true 	 (* If the wait hasn't happened yet then store up the signal *)
      )
end

let keep_alive () =
  while true do
    Thread.delay 20000.
  done
