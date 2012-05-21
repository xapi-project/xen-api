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

module D = Debug.Debugger(struct let name="backgroundscheduler" end)
open D

open Threadext 

type func_ty = OneShot | Periodic of float

type t = {
  func : unit -> unit;
  ty : func_ty;
  name : string;
}

let delay = Delay.make ()

let (queue : (t Ipq.t)) = Ipq.create 50
let lock = Mutex.create ()

let add_to_queue ?(signal=true) name ty start newfunc =
  Mutex.execute lock (fun () ->
    Ipq.add queue { Ipq.ev={ func=newfunc; ty=ty; name=name}; Ipq.time=((Unix.gettimeofday ()) +. start) });
  if signal then Delay.signal delay

let remove_from_queue name =
	let index = Ipq.find_p queue (fun {name=n} -> name = n) in
	if index > -1 then begin
		Ipq.remove queue index
	end
  
let loop () =
    debug "Periodic scheduler started";
    while true do
      try
	let empty = Mutex.execute lock (fun () -> Ipq.is_empty queue) in
	if empty 
	then 
	  (Thread.delay 10.0) (* Doesn't happen often - the queue isn't usually empty *)
	else
	  begin
	    let next = Mutex.execute lock (fun () -> Ipq.maximum queue) in
	    let now = Unix.gettimeofday () in
	    if next.Ipq.time < now then begin
	      let todo = (Mutex.execute lock (fun () -> Ipq.pop_maximum queue)).Ipq.ev in
	      (try todo.func () with _ -> ());
	      match todo.ty with 
		| OneShot -> ()
		| Periodic timer -> add_to_queue ~signal:false todo.name todo.ty timer todo.func
	    end else begin
	      (* Sleep until next event. *)
	      ignore(Delay.wait delay (next.Ipq.time -. now +. 0.001))
	    end
	  end
      with _ -> ()
    done


