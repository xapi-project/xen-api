module D : Debug.DEBUG

type stringpair = string * string

module type INTERFACE =
  sig
    val service_name : string
    exception Does_not_exist of stringpair
    exception Cancelled of string
    module Task :
      sig
        type id = string
        type async_result
        val rpc_of_async_result : async_result -> Rpc.t
        val async_result_of_rpc : Rpc.t -> async_result
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
        }
      end
    module Exception : sig type exnty val rpc_of_exnty : exnty -> Rpc.t end
    val exnty_of_exn : exn -> Exception.exnty
    val exn_of_exnty : Exception.exnty -> exn
    exception Internal_error of string
  end

module Task :
  functor (Interface : INTERFACE) ->
    sig

      type id = string

      type task_handle

      type tasks

      val empty : unit -> tasks

      val next_task_id : unit -> id

      val set_cancel_trigger : tasks -> string -> int -> unit

      val clear_cancel_trigger : tasks -> unit

      val id_of_handle : task_handle -> id

      val to_interface_task : task_handle -> Interface.Task.t

      val add :
        tasks -> string -> (task_handle -> Interface.Task.async_result option) -> task_handle

      val run : task_handle -> unit

      val find : tasks -> id -> task_handle

      val get_state : tasks -> id -> Interface.Task.state

      val with_subtask : task_handle -> id -> (unit -> 'a) -> 'a

      val list : tasks -> id list

      val destroy : tasks -> id -> unit

      val cancel : tasks -> id -> unit

      val raise_cancelled : task_handle -> 'a

      val check_cancelling : task_handle -> unit

      val with_cancel : task_handle -> (unit -> unit) -> (unit -> 'a) -> 'a
    end
