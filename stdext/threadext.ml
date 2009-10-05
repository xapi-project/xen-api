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
    	let r = begin try f () with exn -> Mutex.unlock lock; raise exn end; in
    	Mutex.unlock lock;
    	r
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
    let to_close = ref [ ] in
    let close' fd = 
      if List.mem fd !to_close then Unix.close fd;
      to_close := List.filter (fun x -> fd <> x) !to_close in
    Pervasiveext.finally
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
	   if r <> [] then ignore(Unix.read pipe_out (String.create 1) 0 1);
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
	 | Some fd -> ignore(Unix.write fd "X" 0 1)
	 | None -> x.signalled <- true 	 (* If the wait hasn't happened yet then store up the signal *)
      )
end

let keep_alive () =
	while true do
		Thread.delay 20000.
	done
	
