(** [Async.Lock_file] is a wrapper that provides Async equivalents for
    {{!Core.Lock_file}[Core.Lock_file]}. *)

open! Core
open! Import

(** [create ?message path] tries to create a file at [path] containing the text [message],
    pid if none provided.  It returns true on success, false on failure.  Note: there is
    no way to release the lock or the fd created inside!  It will only be released when
    the process dies.*)
val create
  :  ?message        : string
  -> ?close_on_exec  : bool    (** default is [true] *)
  -> ?unlink_on_exit : bool    (** default is [false] *)
  -> string
  -> bool Deferred.t

(** [create_exn ?message path] is like [create] except that it throws an exception on
    failure instead of returning a boolean value. *)
val create_exn
  :  ?message        : string
  -> ?close_on_exec  : bool    (** default is [true] *)
  -> ?unlink_on_exit : bool    (** default is [false] *)
  -> string
  -> unit Deferred.t

(** [waiting_create path] repeatedly tries to lock [path], becoming determined when [path]
    is locked or raising when [abort] becomes determined.  Similar to
    {{!Core.Lock_file.blocking_create}[Core.Lock_file.blocking_create]}. *)
val waiting_create
  :  ?abort          : unit Deferred.t  (** default is [Deferred.never ()] *)
  -> ?message        : string
  -> ?close_on_exec  : bool             (** default is [true] *)
  -> ?unlink_on_exit : bool             (** default is [false] *)
  -> string
  -> unit Deferred.t

(** [is_locked path] returns true when the file at [path] exists and is locked, false
    otherwise. *)
val is_locked : string -> bool Deferred.t

(** [Nfs] has analogs of functions in {{!Core.Lock_file.Nfs}[Core.Lock_file.Nfs]}; see
    there for documentation.  In addition to adding [Deferred]'s, [blocking_create] was
    renamed [waiting_create] to avoid the impression that it blocks Async. *)
module Nfs : sig
  val create     : ?message : string -> string -> unit Deferred.Or_error.t
  val create_exn : ?message : string -> string -> unit Deferred.t

  val waiting_create
    :  ?abort:unit Deferred.t  (** default is [Deferred.never ()]. *)
    -> ?message : string
    -> string
    -> unit Deferred.t

  val unlock_exn     : string -> unit Deferred.t
  val unlock         : string -> unit Deferred.Or_error.t

  val critical_section
    :  ?message : string
    -> string
    -> abort:unit Deferred.t
    -> f : (unit -> 'a Deferred.t)
    -> 'a Deferred.t

  val get_hostname_and_pid : string -> (string * Pid.t) option Deferred.t
  val get_message          : string ->  string          option Deferred.t
end
