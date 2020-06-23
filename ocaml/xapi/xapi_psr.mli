(*
 * Copyright (C) Citrix Systems Inc.
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

(** psr = pool secret rotation
  * ps = pool secret = ptoken *)

type failure =
  | Failed_during_accept_new_pool_secret
  | Failed_during_send_new_pool_secret
  | Failed_during_cleanup

type 'a r = (unit, failure * 'a) result

(* entry point *)
val start : __context:Context.t -> unit

(* the rest is exposed for unit testing *)

module type Impl = sig
  (* real impl uses SecretString.t
     but tests use string for simplicity *)
  type pool_secret

  (* the old pool_secret will be used as proof that
     a host should accept an incoming pool_secret *)
  (* = (old_pool_secret, new_pool_secret) *)
  type pool_secrets = pool_secret * pool_secret

  type host

  (* checkpoints should be written to persistent
     storage in the event that a PSR fails. the
     next PSR will resume from the last checkpoint *)
  val save_checkpoint : string -> unit

  val retrieve_checkpoint : unit -> string option

  (* if a PSR fails, the next PSR will use the
      backed-up pool_secrets *)
  val backup : pool_secrets -> unit

  val retrieve : unit -> pool_secrets

  (* these tell_* are  executed on each host in the
     pool. if any call fails, the PSR fails *)
  val tell_accept_new_pool_secret : pool_secrets -> host -> unit

  val tell_send_new_pool_secret : pool_secrets -> host -> unit

  val tell_cleanup_old_pool_secret : host -> unit

  (* cleanup master _after_ the members *)
  val cleanup_master : unit -> unit
end

module Make : functor (Impl : Impl) -> sig
  open Impl

  (* we model a pool as a list of hosts.
     we accept pool_secrets as a parameter to avoid non-determinism *)
  val start : pool_secrets -> master:host -> members:host list -> host r
end
