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
open Xapi_stdext_monadic
open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_threads.Threadext


module D = Debug.Make(struct let name = "task_server" end)
open D

type stringpair = string * string

module type INTERFACE = sig
  val service_name : string

  val does_not_exist : string * string -> exn
  val cancelled : string -> exn
  val marshal_exn : exn -> Rpc.t

  module Task : sig
    type id = string

    type async_result

    type completion_t = {
      duration : float;
      result : async_result option
    }

    type state =
      | Pending of float
      | Completed of completion_t
      | Failed of Rpc.t

    type t = {
      id: id;
      dbg: string;
      ctime: float;
      state: state;
      subtasks: (string * state) list;
      debug_info: (string * string) list;
      backtrace: string;
      cancellable: bool;
    }

  end

  (* The following stuff comes from rpc-light.idl *)

end


module Task = functor (Interface : INTERFACE) -> struct

  module SMap = Map.Make(struct type t = string let compare = compare end)

  (* Tasks are stored in an id -> t map *)
  type id = string

  (* A task is associated with every running operation *)
  type task_handle = {
    tasks : tasks;
    id: id;                                        (* unique task id *)
    ctime: float;                                  (* created timestamp *)
    dbg: string;                                   (* token sent by client *)
    mutable state: Interface.Task.state;           (* current completion state *)
    mutable subtasks: (string * Interface.Task.state) list; (* one level of "subtasks" *)
    f: task_handle -> Interface.Task.async_result option;    (* body of the function *)
    tm: Mutex.t;                                   (* protects cancelling state: *)
    mutable cancelling: bool;                      (* set by cancel *)
    mutable cancel: (unit -> unit) list;           (* attempt to cancel [f] *)
    mutable cancel_points_seen: int;               (* incremented every time we pass a cancellation point *)
    test_cancel_at: int option;                    (* index of the cancel point to trigger *)
    mutable backtrace: Backtrace.t;                (* on error, a backtrace *)
    mutable cancellable: bool;
  }

  and tasks = {
    task_map : task_handle SMap.t ref;
    mutable test_cancel_trigger : (string * int) option;
    m : Mutex.t;
    c : Condition.t;
  }

  let empty () =
    let task_map = ref SMap.empty in
    let m = Mutex.create () in
    let c = Condition.create () in
    { task_map; test_cancel_trigger = None; m; c }

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

  let id_of_handle task_handle = task_handle.id

  (* [add dbg f] creates a fresh [t], registers and returns it *)
  let add tasks dbg (f: task_handle -> Interface.Task.async_result option) =
    let t = {
      tasks = tasks;
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
      test_cancel_at = (match tasks.test_cancel_trigger with
          | Some (dbg', n) when dbg = dbg' ->
            clear_cancel_trigger tasks; (* one shot *)
            Some n
          | _ -> None);
      backtrace = Backtrace.empty;
      cancellable = true;
    } in
    Mutex.execute tasks.m
      (fun () ->
         tasks.task_map := SMap.add t.id t !(tasks.task_map)
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
      Backtrace.is_important e;
      error "Task %s failed; %s" item.id (Printexc.to_string e);
      item.backtrace <- Backtrace.remove e;
      let e = e |> Interface.marshal_exn in
      item.state <- Interface.Task.Failed e

  let find_locked tasks id =
    try
      SMap.find id !(tasks.task_map)
    with
    | _ -> raise (Interface.does_not_exist ("task", id))

  let to_interface_task t =
    {
      Interface.Task.id = t.id;
      dbg = t.dbg;
      ctime = t.ctime;
      state = t.state;
      subtasks = t.subtasks;
      debug_info = [
        "cancel_points_seen", string_of_int t.cancel_points_seen
      ];
      backtrace = Sexplib.Sexp.to_string (Backtrace.sexp_of_t t.backtrace);
      cancellable = t.cancellable;
    }

  let handle_of_id tasks id =
    Mutex.execute tasks.m (fun () -> find_locked tasks id)

  let get_state task =
    task.state

  let set_state task state =
    task.state <- state

  let get_dbg task_handle =
    task_handle.dbg

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
      Backtrace.is_important e;
      let e' = Interface.marshal_exn e in
      t.subtasks <- replace_assoc name (Interface.Task.Failed e') t.subtasks;
      raise e

  let list tasks =
    Mutex.execute tasks.m
      (fun () ->
         SMap.bindings !(tasks.task_map) |> List.map snd
      )

  (* Remove the task from the id -> task mapping. NB any active thread will still continue. *)
  let destroy task =
    let tasks = task.tasks in
    Mutex.execute tasks.m
      (fun () ->
         tasks.task_map := SMap.remove task.id !(tasks.task_map)
      )

  let cancel task =
    let callbacks = Mutex.execute task.tm
        (fun () ->
           if not task.cancellable then begin
             info "Task %s is not cancellable." task.id;
             []
           end
           else begin
             task.cancelling <- true;
             task.cancel
           end
        ) in
    List.iter
      (fun f ->
         try
           f ()
         with e ->
           debug "Task.cancel %s: ignore exception %s" task.id (Printexc.to_string e)
      ) callbacks

  let raise_cancelled task =
    info "Task %s has been cancelled: raising Cancelled exception" task.id;
    raise (Interface.cancelled task.id)

  let check_cancelling_locked task =
    task.cancel_points_seen <- task.cancel_points_seen + 1;
    if task.cancelling then raise_cancelled task;
    Opt.iter (fun x -> if task.cancel_points_seen = x then begin
        info "Task %s has been triggered by the test-case (cancel_point = %d)" task.id task.cancel_points_seen;
        raise_cancelled task
      end) task.test_cancel_at

  let check_cancelling t =
    Mutex.execute t.tm (fun () -> check_cancelling_locked t)

  let with_cancel t cancel_fn f =
    Mutex.execute t.tm (fun () ->
        try
          check_cancelling_locked t;
          t.cancel <- cancel_fn :: t.cancel
        with e ->
          (try cancel_fn () with e -> debug "Task.cancel %s: ignore exception %s" t.id (Printexc.to_string e));
          raise e);
    finally
      (fun () ->
         check_cancelling t;
         f ()
      )
      (fun () -> Mutex.execute t.tm (fun () -> t.cancel <- List.tl t.cancel))

  let prohibit_cancellation task =
    Mutex.execute task.tm (fun () ->
        (* If task is cancelling, just cancel it before setting it to not cancellable *)
        check_cancelling_locked task;
        task.cancellable <- false)
end
