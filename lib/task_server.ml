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

open Xenops_utils

module D = Debug.Make(struct let name = "task_server" end)
open D

type stringpair = string * string

module type INTERFACE = sig
	val service_name : string

	exception Does_not_exist of stringpair
	exception Cancelled of string

	module Task : sig 
		type id = string

		type async_result

		val rpc_of_async_result : async_result -> Rpc.t
		val async_result_of_rpc : Rpc.t -> async_result

		type completion_t = {
			duration : float;
			result : async_result option
		}
				
		type state =
			| Pending of float
			| Completed of completion_t
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

module SMap = Map.Make(struct type t = string let compare = compare end)

(* Tasks are stored in an id -> t map *)

(* A task is associated with every running operation *)
type t = {
	id: string;                                    (* unique task id *)
	ctime: float;                                  (* created timestamp *)
	dbg: string;                                   (* token sent by client *)
	mutable state: Interface.Task.state;         (* current completion state *)
	mutable subtasks: (string * Interface.Task.state) list; (* one level of "subtasks" *)
	f: t -> Interface.Task.async_result option;    (* body of the function *)
	tm: Mutex.t;                                   (* protects cancelling state: *)
    mutable cancelling: bool;                      (* set by cancel *)
	mutable cancel: (unit -> unit) list;           (* attempt to cancel [f] *)
	mutable cancel_points_seen: int;               (* incremented every time we pass a cancellation point *)
	test_cancel_at: int option;                    (* index of the cancel point to trigger *)
}

type tasks = {
	tasks : t SMap.t ref;
	mutable test_cancel_trigger : (string * int) option;
	m : Mutex.t;
	c : Condition.t;
}

let empty () =
	let tasks = ref SMap.empty in
	let m = Mutex.create () in
	let c = Condition.create () in
	{ tasks; test_cancel_trigger = None; m; c }

(* [next_task_id ()] returns a fresh task id *)
let next_task_id =
	let counter = ref 0 in
	fun () ->
		let result = string_of_int !counter in
		incr counter;
		result

let set_cancel_trigger tasks dbg n =
	Mutex.execute tasks.m
		(fun () ->
			tasks.test_cancel_trigger <- Some (dbg, n)
		)

let clear_cancel_trigger tasks =
	Mutex.execute tasks.m
		(fun () ->
			tasks.test_cancel_trigger <- None
		)

(* [add dbg f] creates a fresh [t], registers and returns it *)
let add tasks dbg (f: t -> Interface.Task.async_result option) =
	let t = {
		id = next_task_id ();
		ctime = Unix.gettimeofday ();
		dbg = dbg;
		state = Interface.Task.Pending 0.;
		subtasks = [];
		f = f;
		tm = Mutex.create ();
		cancelling = false;
		cancel = [];
		cancel_points_seen = 0;
		test_cancel_at = match tasks.test_cancel_trigger with
			| Some (dbg', n) when dbg = dbg' ->
				clear_cancel_trigger tasks; (* one shot *)
				Some n
			| _ -> None
	} in
	Mutex.execute tasks.m
		(fun () ->
			tasks.tasks := SMap.add t.id t !(tasks.tasks)
		);
	t

(* [run t] executes the task body, updating the fields of [t] *)
let run item =
	try
		let start = Unix.gettimeofday () in
		let result = item.f item in
		let duration = Unix.gettimeofday () -. start in
		item.state <- Interface.Task.Completed { Interface.Task.duration; result };
		debug "Task %s completed; duration = %.0f" item.id duration
	with
		| e ->
			let e = e |> Interface.exnty_of_exn |> Interface.Exception.rpc_of_exnty in
			debug "Task %s failed; exception = %s" item.id (e |> Jsonrpc.to_string);
			debug "%s" (Printexc.get_backtrace ());
			item.state <- Interface.Task.Failed e

let exists_locked tasks id = SMap.mem id !(tasks.tasks)

let find_locked tasks id =
	if not (exists_locked tasks id) then raise (Interface.Does_not_exist("task", id));
	SMap.find id !(tasks.tasks)

let replace_assoc key new_value existing =
        (key, new_value) :: (List.filter (fun (k, _) -> k <> key) existing)

let with_subtask t name f =
	let start = Unix.gettimeofday () in
	try
		t.subtasks <- (name, Interface.Task.Pending 0.) :: t.subtasks;
		let result = f () in
		let duration = Unix.gettimeofday () -. start in
		t.subtasks <- replace_assoc name (Interface.Task.Completed {Interface.Task.duration; result=None}) t.subtasks;
		result
	with e ->
		t.subtasks <- replace_assoc name (Interface.Task.Failed (Interface.Exception.rpc_of_exnty (Interface.exnty_of_exn (Interface.Internal_error (Printexc.to_string e))))) t.subtasks;
		raise e

let list tasks =
	Mutex.execute tasks.m
		(fun () ->
			SMap.bindings !(tasks.tasks) |> List.map snd
		)

(* Remove the task from the id -> task mapping. NB any active thread will still continue. *)
let destroy tasks id =
	Mutex.execute tasks.m
		(fun () ->
			tasks.tasks := SMap.remove id !(tasks.tasks)
		)

let cancel tasks id =
	let t = Mutex.execute tasks.m (fun () -> find_locked tasks id) in
	let callbacks = Mutex.execute t.tm
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

let raise_cancelled t =
	info "Task %s has been cancelled: raising Cancelled exception" t.id;
	raise (Interface.Cancelled(t.id))

let check_cancelling t =
	Mutex.execute t.tm
		(fun () ->
			t.cancel_points_seen <- t.cancel_points_seen + 1;
			if t.cancelling then raise_cancelled t;
			Opt.iter (fun x -> if t.cancel_points_seen = x then begin
				info "Task %s has been triggered by the test-case (cancel_point = %d)" t.id t.cancel_points_seen;
				raise_cancelled t
			end) t.test_cancel_at
		)

let with_cancel t cancel_fn f =
	Mutex.execute t.tm (fun () -> t.cancel <- cancel_fn :: t.cancel);
	finally
		(fun () ->
			check_cancelling t;
			f ()
		)
		(fun () -> Mutex.execute t.tm (fun () -> t.cancel <- List.tl t.cancel))

end
