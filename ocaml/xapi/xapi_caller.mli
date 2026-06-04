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

(** In-memory caller_table mirroring the Caller DB rows, plus the entry
    point used by [Server_helpers.do_dispatch] / [Xapi_http.add_handler] to
    apply per-caller rate limiting. *)

val default_token_cost : float

val get_token_cost : string -> float

val submit_sync :
     user_agent:string
  -> client_ip:string
  -> callback:(unit -> 'a)
  -> task_create:((Context.t -> unit) -> unit)
  -> float
  -> 'a

val submit_async :
     user_agent:string
  -> client_ip:string
  -> callback:(unit -> unit)
  -> task_create:((Context.t -> unit) -> unit)
  -> float
  -> unit

val create :
     __context:Context.t
  -> name_label:string
  -> name_description:string
  -> user_agent:string
  -> client_ip:string
  -> API.ref_Caller

val destroy : __context:Context.t -> self:API.ref_Caller -> unit

val add_group :
  __context:Context.t -> self:API.ref_Caller -> group:string -> unit

val remove_group :
  __context:Context.t -> self:API.ref_Caller -> group:string -> unit

val query_token_usage : __context:Context.t -> self:API.ref_Caller -> float

val query_call_count : __context:Context.t -> self:API.ref_Caller -> int64

val query_group_token_usage : __context:Context.t -> group:string -> float

val query_group_call_count : __context:Context.t -> group:string -> int64

val query_all_usage : __context:Context.t -> string list list

val register : __context:Context.t -> unit
(** Populate caller_table from persisted Caller rows and install
    [Xapi_rate_limit]'s caller-refresh callback. Must run after
    [Xapi_rate_limit.register] so that bucket lookups succeed. *)
