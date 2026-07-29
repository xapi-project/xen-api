(*
 * Copyright (C) Cloud Software Group, Inc.
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

(** In-memory store of rate-limit token-bucket workers, keyed by the
    Rate_limit datamodel ref. Owns the worker lifecycle and exposes
    {!find_bucket} for Xapi_caller's request-dispatch path. *)

val find_bucket : API.ref_Rate_limit -> Rate_limit_lib.Rate_limit.t option
(** Look up the in-memory bucket worker for a Rate_limit row. *)

val set_caller_refresh_callback :
  (__context:Context.t -> API.ref_Caller -> unit) -> unit
(** Install the callback used to refresh in-memory state for a caller
    whenever its rate_limit DB field changes. Called once at startup by
    Xapi_caller. *)

(** {2 Datamodel-message implementations.}

    These are called by the generated dispatch layer via the
    Custom_actions / message_forwarding wiring. *)

val create :
     __context:Context.t
  -> name_label:string
  -> name_description:string
  -> burst_size:float
  -> fill_rate:float
  -> API.ref_Rate_limit

val destroy : __context:Context.t -> self:API.ref_Rate_limit -> unit

val add_caller :
     __context:Context.t
  -> self:API.ref_Rate_limit
  -> caller:API.ref_Caller
  -> unit

val remove_caller :
     __context:Context.t
  -> self:API.ref_Rate_limit
  -> caller:API.ref_Caller
  -> unit

val set_burst_size :
  __context:Context.t -> self:API.ref_Rate_limit -> value:float -> unit

val set_fill_rate :
  __context:Context.t -> self:API.ref_Rate_limit -> value:float -> unit

val register : __context:Context.t -> unit
(** Load all persisted Rate_limit rows and build their in-memory workers.
    Must run before Xapi_caller.register so that callers can attach to
    existing buckets at startup. *)
