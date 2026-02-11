(*
 * Copyright (c) Cloud Software Group, Inc
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
open Types
open Client.Client

module Task : TASK

module Object : functor (O : OBJECT) ->
  OBJECT_OUT
    with type dbref = O.dbref
     and type t = O.t
     and type 'a Task.t = 'a Task.t

module VM : sig
  include
    OBJECT_OUT
      with type dbref = API.ref_VM
       and type t = API.vM_t
       and type 'a Task.t = 'a Task.t

  include module type of VM

  module Async : sig
    val clone : client -> vm:dbref -> new_name:string -> dbref Task.t

    val pool_migrate :
         Client.Client.client
      -> vm:dbref
      -> host:[< `host] Ref.t
      -> options:(string * string) list
      -> unit Task.t

    val hard_reboot : Client.Client.client -> vm:dbref -> unit Task.t

    val hard_shutdown : Client.Client.client -> vm:dbref -> unit Task.t

    val start_on :
         Client.Client.client
      -> vm:dbref
      -> host:[< `host] Ref.t
      -> start_paused:bool
      -> force:bool
      -> unit Task.t
  end
end

val batched_run_or_cancel :
     client
  -> string
  -> ?batch_size:int
  -> ?on_progress:(total:int -> int -> float -> unit)
  -> (client -> 'a Task.t) list
  -> ('a, exn) result list
(** [batched_run_or_cancel name ?batch_size t ?on_progress tasks] runs all [tasks], and waits for them to complete.
    If any task fails, all other pending tasks are canceled if possible, and
    then waits for all tasks to complete.
    [on_progress] is called with the overall progress of tasks, by default this
    shows a CLI progress bar.

    @returns the result of all tasks
*)

val wait_no_active_tasks :
     ?on_progress:(total:int -> int -> float -> unit)
  -> client
  -> host:API.ref_host
  -> unit
(** [wait_no_active_tasks t ~host] waits until [host] has 0 active tasks. *)
