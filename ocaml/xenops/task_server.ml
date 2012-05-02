(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(**
 * @group Xenops
 *)

open Threadext
open Pervasiveext
open Listext
open Fun

type stringpair = string * string

module type INTERFACE = sig
	val service_name : string

	exception Does_not_exist of stringpair
	exception Cancelled of string

	module Task : sig 
		type id = string
				
		type result =
			| Pending of float
			| Completed of float
			| Failed of Rpc.t
	end

	(* The following stuff comes from rpc-light.idl *)

	module Exception : sig
		type exnty
		val rpc_of_exnty : exnty -> Rpc.t
	end

	val exnty_of_exn : exn -> Exception.exnty
	val exn_of_exnty : Exception.exnty -> exn

	exception Internal_error of string
		
end


module Task = functor (Interface : INTERFACE) -> struct		

module D = Debug.Debugger(struct let name = Interface.service_name end)
open D

(* A task is associated with every running operation *)
type t = {
	id: string;                                    (* unique task id *)
	ctime: float;                                  (* created timestamp *)
	debug_info: string;                            (* token sent by client *)
	mutable result: Interface.Task.result;                   (* current completion state *)
	mutable subtasks: (string * Interface.Task.result) list; (* one level of "subtasks" *)
	f: t -> unit;                                  (* body of the function *)
	m: Mutex.t;                                    (* protects cancelling state: *)
    mutable cancelling: bool;                      (* set by cancel *)
	mutable cancel: (unit -> unit) list;           (* attempt to cancel [f] *)
}

module SMap = Map.Make(struct type t = string let compare = compare end)

(* Tasks are stored in an id -> t map *)
let tasks = ref SMap.empty
let m = Mutex.create ()
let c = Condition.create ()

(* [next_task_id ()] returns a fresh task id *)
let next_task_id =
	let counter = ref 0 in
	fun () ->
		let result = string_of_int !counter in
		incr counter;
		result

(* [add dbg f] creates a fresh [t], registers and returns it *)
let add dbg (f: t -> unit) =
	let t = {
		id = next_task_id ();
		ctime = Unix.gettimeofday ();
		debug_info = dbg;
		result = Interface.Task.Pending 0.;
		subtasks = [];
		f = f;
		m = Mutex.create ();
		cancelling = false;
		cancel = [];
	} in
	Mutex.execute m
		(fun () ->
			tasks := SMap.add t.id t !tasks
		);
	t

(* [run t] executes the task body, updating the fields of [t] *)
let run item =
	try
		let start = Unix.gettimeofday () in
		item.f item;
		let duration = Unix.gettimeofday () -. start in
		item.result <- Interface.Task.Completed duration;
	with
		| e ->
			let e = e |> Interface.exnty_of_exn |> Interface.Exception.rpc_of_exnty in
			debug "Caught exception while processing queue: %s" (e |> Jsonrpc.to_string);
			debug "%s" (Printexc.get_backtrace ());
			item.result <- Interface.Task.Failed e

let exists_locked id = SMap.mem id !tasks

let find_locked id =
	if not (exists_locked id) then raise (Interface.Does_not_exist("task", id));
	SMap.find id !tasks

let with_subtask t name f =
	let start = Unix.gettimeofday () in
	try
		t.subtasks <- (name, Interface.Task.Pending 0.) :: t.subtasks;
		let result = f () in
		t.subtasks <- List.replace_assoc name (Interface.Task.Completed (Unix.gettimeofday () -. start)) t.subtasks;
		result
	with e ->
		t.subtasks <- List.replace_assoc name (Interface.Task.Failed (Interface.Exception.rpc_of_exnty (Interface.exnty_of_exn (Interface.Internal_error (Printexc.to_string e))))) t.subtasks;
		raise e

let list () =
	Mutex.execute m
		(fun () ->
			SMap.bindings !tasks |> List.map snd
		)

(* Remove the task from the id -> task mapping. NB any active thread will still continue. *)
let destroy id =
	Mutex.execute m
		(fun () ->
			tasks := SMap.remove id !tasks
		)

let cancel id =
	let t = Mutex.execute m (fun () -> find_locked id) in
	let callbacks = Mutex.execute t.m
		(fun () ->
			t.cancelling <- true;
			t.cancel
		) in
	List.iter
		(fun f ->
			try
				f ()
			with e ->
				debug "Task.cancel %s: ignore exception %s" id (Printexc.to_string e)
		) callbacks

let raise_cancelled t = raise (Interface.Cancelled(t.id))

let check_cancelling t = if Mutex.execute t.m (fun () -> t.cancelling) then raise_cancelled t

let with_cancel t cancel_fn f =
	Mutex.execute t.m (fun () -> t.cancel <- cancel_fn :: t.cancel);
	finally
		(fun () ->
			check_cancelling t;
			f ()
		)
		(fun () -> Mutex.execute t.m (fun () -> t.cancel <- List.tl t.cancel))

end
