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
      end
    module Exception : sig type exnty val rpc_of_exnty : exnty -> Rpc.t end
    val exnty_of_exn : exn -> Exception.exnty
    val exn_of_exnty : Exception.exnty -> exn
    exception Internal_error of string
  end

module Task :
  functor (Interface : INTERFACE) ->
    sig

      type t = {
        id : string;
        ctime : float;
        dbg : string;
        mutable state : Interface.Task.state;
        mutable subtasks : (string * Interface.Task.state) list;
        f : t -> Interface.Task.async_result option;
        tm : Mutex.t;
        mutable cancelling : bool;
        mutable cancel : (unit -> unit) list;
        mutable cancel_points_seen : int;
        test_cancel_at : int option;
        mutable backtrace : Backtrace.t;
      }

      type tasks

      val empty : unit -> tasks

      val next_task_id : unit -> string

      val set_cancel_trigger : tasks -> string -> int -> unit

      val clear_cancel_trigger : tasks -> unit

      val add :
        tasks -> string -> (t -> Interface.Task.async_result option) -> t

      val run : t -> unit

      val get_state : tasks -> string -> Interface.Task.state

      val with_subtask : t -> string -> (unit -> 'a) -> 'a

      val list : tasks -> t list

      val destroy : tasks -> string -> unit

      val cancel : tasks -> string -> unit

      val raise_cancelled : t -> 'a

      val check_cancelling_locked : t -> unit

      val check_cancelling : t -> unit

      val with_cancel : t -> (unit -> unit) -> (unit -> 'a) -> 'a
    end
