(* Task_server: Helper for interfaces of a particular shape
   *
   * This module is a helper module for interfaces that implement a Task interface
   * with which asynchronous calls are implemented. In particular, it handles
   * subtasks, cancellation, reporting of backtraces and a way of injecting
   * failures at particular points known as 'cancellation triggers'
*)

type stringpair = string * string

module type INTERFACE =
sig
  val service_name : string
  val does_not_exist : stringpair -> exn
  val cancelled : string -> exn
  val marshal_exn : exn -> Rpc.t

  module Task :
  sig
    type id = string
    type async_result
    type completion_t = {
      duration : float;
      result : async_result option;
    }
    type state =
        Pending of float
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
end

module Task :
  functor (Interface : INTERFACE) ->
  sig

    (* An [id] is a marshallable reference to a task in a [tasks] type *)
    type id = string

    (* A [task_handle] is required for all operations on tasks *)
    type task_handle

    (* A [tasks] record contains a list of tasks *)
    type tasks

    (* Operations on [tasks] *)
    val empty : unit -> tasks
    val list : tasks -> task_handle list

    (* After running [set_cancel_trigger tasks dbg n], the next task added with
       matching [dbg] will be cancelled on the [n]th time it checks for cancellation *)
    val set_cancel_trigger : tasks -> string -> int -> unit
    val clear_cancel_trigger : tasks -> unit

    (* Convert between task_handle and id and Interface.Task.t *)
    val id_of_handle : task_handle -> id
    val handle_of_id : tasks -> id -> task_handle
    val to_interface_task : task_handle -> Interface.Task.t

    (* [add tasks dbg f] adds a new task with debug string [dbg] that will execute [f] when run *)
    val add : tasks -> string -> (task_handle -> Interface.Task.async_result option) -> task_handle
    val run : task_handle -> unit

    (* Query/Set the current state/dbg key of a task *)
    val get_state : task_handle -> Interface.Task.state
    val set_state : task_handle -> Interface.Task.state -> unit
    val get_dbg : task_handle -> string

    (* Given a task, record a specific subtask happening. Subtasks are just for
       labelling subsections of a task, and there is no real task hierarchy. *)
    val with_subtask : task_handle -> id -> (unit -> 'a) -> 'a

    (* Destroy removes the task from the [tasks] list *)
    val destroy : task_handle -> unit

    (* Cancel attempts to cancel a task. This is a cooperative thing,
       the task must check ifself whether it has been cancelled. However, any
       cancel function passed in via [with_cancel] will be executed after the
       task has been marked as cancelled *)
    val cancel : task_handle -> unit

    (* This will raise the appropriate exception to record that the
       currently executing task has been cancelled. Should be called within the
       context of the function [f] from the [add] function call *)
    val raise_cancelled : task_handle -> 'a

    (* Checks to see whether the task has been asked to cancel. Raises the
       Cancelled exception if it has, or if it has hit the cancellation point
       set *)
    val check_cancelling : task_handle -> unit

    (* Sets a cancellation function to be called if the task is cancelled *)
    val with_cancel : task_handle -> (unit -> unit) -> (unit -> 'a) -> 'a
    (* Set a task not cancellable *)
    val prohibit_cancellation: task_handle -> unit
  end
