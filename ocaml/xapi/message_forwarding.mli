(*
 * Copyright (C) 2026 Vates.
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

(** Forwarding of API calls to the host where they must run *)

val call_slave_with_session :
     (Context.t -> string -> API.ref_task option -> Rpc.call -> Rpc.response)
  -> Context.t
  -> API.ref_host
  -> API.ref_task option
  -> (rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> 'a)
  -> 'a
(** [call_slave_with_session remote_rpc_fn __context host task_opt f]
    calls [f] with an RPC function to [host] built by [remote_rpc_fn],
    and a pool session that is destroyed afterwards. *)

val do_op_on_common :
     local_fn:(__context:Context.t -> 'a)
  -> __context:Context.t
  -> host:API.ref_host
  -> remote_fn:'b
  -> (Context.t -> API.ref_host -> API.ref_task option -> 'b -> 'a)
  -> 'a
(** Run [local_fn] if [host] is the local host, otherwise call the last
    argument to run [remote_fn] on [host]. *)

val do_op_on_nolivecheck_no_retry :
     local_fn:(__context:Context.t -> 'a)
  -> __context:Context.t
  -> host:API.ref_host
  -> remote_fn:
       (rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> 'a)
  -> 'a
(** Run an operation on [host] with a pool session, without checking
    that [host] is live and without retrying. *)

val do_op_on_localsession_nolivecheck :
     local_fn:(__context:Context.t -> 'a)
  -> __context:Context.t
  -> host:API.ref_host
  -> remote_fn:
       (rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> 'a)
  -> 'a
(** Run an operation on [host] with a local session, without checking
    that [host] is live and without retrying. *)

(** Implementation of the API that forwards each call to the host where
    it must run, and runs it there with the given implementation. *)
module Forward : functor (_ : Custom_actions.CUSTOM_ACTIONS) ->
  Custom_actions.CUSTOM_ACTIONS

val register_callback_fns : unit -> unit
(** Register the callbacks used by the RPC clients to track stunnel
    processes and check the destination of stunnel connections. *)
