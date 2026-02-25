open Quicktest_trace
open Client.Client

(** A XAPI object *)
module type OBJECT = sig
  (** XAPI DB record type *)
  type t

  (** 'a {!type:Ref.t} *)
  type dbref

  val rpc_of_t : t -> Rpc.t
  (** [rpc_of_t t] converts a DB record to an Rpc type *)

  val string_of_ref : dbref -> string
  (** [string_of_ref dbref] converts a db reference to a string *)

  val dbref_of_rpc : Rpc.t -> dbref
  (** [dbref_of_rpc rpc] unmarshals a dbref from [rpc]. *)

  val get_record : (self:dbref -> t) Client.Client.api
  (** [get_record ~rpc ~session ~self] is the API call to retrieve the DB
      record, given a db reference [self] *)
end

module type TASK = sig
  type 'a t

  val v :
       string
    -> (Rpc.t -> 'a)
    -> (client -> Scope.t -> unit)
    -> (unit -> API.ref_task)
    -> 'a t
  (** [v client name of_rpc log_obj f] is the Async API call [f]
        that returns a Task.
        The Task result is converted using [of_rpc] on success.
        On failure [log_obj] is used.
    *)

  val task : _ t -> API.ref_task
  (** [task t] is the DB reference for the task *)

  val result : Client.Client.client -> 'a t -> ('a, exn) result
  (** [result t task] retrieves the task result, raises exception on
        failure, and calls [log ~self:task.dbref].
        Can be called multiple times, it caches the result.
        Also marks the tracing span as completed.
    *)

  val on_progress : _ t -> float -> unit
  (** [on_progress task progress] emits a [task.progress] metric, where [progress] should be between 0.0 and 1.0. *)
end

module type OBJECT_OUT = sig
  include OBJECT

  val log : Client.Client.client -> Scope.t -> self:dbref -> unit
  (** [log client scope ~self] retrieves the DB record for [self] and logs it. *)

  val with_call : Client.Client.client -> string -> dbref -> 'a api -> 'a
  (** [with_call client name dbref f] is the API call [f], and calls [log ~self] on failure. *)

  val call_set :
       Client.Client.client
    -> (self:dbref -> value:'a -> unit) api
    -> self:dbref
    -> value:'a
    -> unit
  (** [call_set t f ~self ~value] is the API call [f ~self ~value], calls [log ~self] on failure *)

  val call_get :
    Client.Client.client -> (self:dbref -> 'a) api -> self:dbref -> 'a
  (** [call_get t f ~self] is the API call [f ~self] *)

  module Task : sig
    include TASK

    val of_call :
      client -> string -> (Rpc.t -> 'a) -> dbref -> API.ref_task api -> 'a t
    (** [of_call client name of_rpc dbref f] is the Async API call [f] on
        the object [dbref] that creates a task [name], and the result is mapped using [of_rpc]. *)
  end

  val task :
       Client.Client.client
    -> string
    -> (Rpc.t -> 'a)
    -> dbref
    -> API.ref_task api
    -> 'a Task.t
  (** [task] is {!val:Task.of_call} *)
end
