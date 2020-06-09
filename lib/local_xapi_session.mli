(*
 * Copyright (C) Citrix Inc
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

val with_session :
     ((Rpc.call -> Rpc.response Lwt.t) -> [`session] API.Ref.t -> 'a Lwt.t)
  -> 'a Lwt.t
(** [with_session f] logs in as the local superuser via xapi's local Unix
    domain socket, and takes care to close the session when [f] finishes. It
    keeps retrying the login requests up to
    {!Consts.wait_for_xapi_timeout_seconds} seconds. If it does not manage to
    log in before this timeout, it fails with an exception. It waits for
    {!Consts.wait_for_xapi_retry_delay_seconds} seconds between subsequent
    login attempts. *)
