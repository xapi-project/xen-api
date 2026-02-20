(** A work item submitted to a worker pool *)
module type Work = sig
  (** work item*)
  type t

  val describe_work : t -> string
  (** [describe_work work] returns a short description of the operation
      for debugging purposes *)

  val dump_task : t -> Rpc.t
  (** [dump_task t] dumps information about the task to execute, other than
      the operation defined above *)

  val execute : t -> unit
  (** [execute item] gets called to run the work item.
      Exceptions raised by [execute] get logged and ignored.
      Calls to [execute] with the same tag are serialised.
  *)

  val finally : t -> unit
  (** [finally item] gets called when executing the work item has finished,
      regardless whether it raised an exception or not.
      Exceptions raised by [finally] get logged and ignored.
      Note that calls to [finally] are not serialised!
  *)

  val should_keep : t -> t list -> bool
  (** [should_keep current previous]
      Determines whether the current work item should be retained
      knowing that the [previous] items in the queue exist.
  *)
end

module type Dump = sig
  type t

  val typ_of : t Rpc.Types.typ

  val make : unit -> t
  (** Dump the global state of this module *)
end

module type S = sig
  (** work item *)
  type work

  module Redirector : sig
    (** A redirector queues items, and redirects their execution to a thread
        from the worker pool *)
    type t

    module Dump : Dump

    val default : t
    (** The default queue should be used for all items, except see below *)

    val parallel_queues : t
    (** We create another queue only for Parallel atoms so as to avoid a situation where
        Parallel atoms can not progress because all the workers available for the
        default queue are used up by other operations depending on further Parallel
        atoms, creating a deadlock.
    *)

    (* We create another queue only for Nested_parallel atoms for the same reason
     as parallel_queues. When a Nested_parallel atom is inside a Parallel atom,
     they are both using a worker whilst not doing any work, so they each need
     additional space to prevent a deadlock. *)
    val nested_parallel_queues : t

    (* We create another queue only for VM_receive_memory operations for the same reason again.
     Migration spawns 2 operations, send and receive, so if there is limited available worker space
     a deadlock can happen when VMs are migrating between hosts or on localhost migration
     as the receiver has no free workers to receive memory. *)
    val receive_memory_queues : t

    val push : t -> string -> work -> unit
    (** [push queue tag work] Pushes [wotk] at the end of [queue].
        Items with the same [tag] are serialised, but items with different tags
        can be executed in parallel if enough workers are available.
        [tag]s get scheduled in a round-robin fashion.
        You need to start some workers, otherwise none of the items get executed.
    *)

    val alias : tag:string -> alias:string -> unit

    val to_string : t -> string
  end

  module WorkerPool : sig
    module Dump : Dump

    val start : int -> unit
    (** [start n] Launches [n] additional worker threads *)

    val set_size : int -> unit
    (** [set_size n] sets the worker pool size to [n]. *)
  end
end

module Make : functor (W : Work) -> S with type work = W.t
