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
      module SMap :
        sig
          type key = string
          type +'a t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val max_binding : 'a t -> key * 'a
          val choose : 'a t -> key * 'a
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        end
      type t = {
        id : string;
        ctime : float;
        dbg : string;
        mutable state : Interface.Task.state;
        mutable subtasks : (string * Interface.Task.state) list;
        f : t -> Interface.Task.async_result option;
        tm : Stdext.Threadext.Mutex.t;
        mutable cancelling : bool;
        mutable cancel : (unit -> unit) list;
        mutable cancel_points_seen : int;
        test_cancel_at : int option;
        mutable backtrace : Backtrace.t;
      }
      type tasks = {
        tasks : t SMap.t ref;
        mutable test_cancel_trigger : (string * int) option;
        m : Stdext.Threadext.Mutex.t;
        c : Condition.t;
      }
      val empty : unit -> tasks
      val next_task_id : unit -> string
      val set_cancel_trigger : tasks -> string -> int -> unit
      val clear_cancel_trigger : tasks -> unit
      val add :
        tasks -> string -> (t -> Interface.Task.async_result option) -> t
      val run : t -> unit
      val exists_locked : tasks -> SMap.key -> bool
      val find_locked : tasks -> SMap.key -> t
      val replace_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
      val with_subtask : t -> string -> (unit -> 'a) -> 'a
      val list : tasks -> t list
      val destroy : tasks -> SMap.key -> unit
      val cancel : tasks -> SMap.key -> unit
      val raise_cancelled : t -> 'a
      val check_cancelling_locked : t -> unit
      val check_cancelling : t -> unit
      val with_cancel : t -> (unit -> unit) -> (unit -> 'a) -> 'a
    end
