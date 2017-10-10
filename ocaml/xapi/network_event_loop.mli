(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

(** This module implements an event loop that watches network objects, and
    updates the firewall rules for the NBD port in case of relevant
    changes by calling a dedicated script. The items in the "purpose"
    field of the network objects specify whether encrypted ("nbd" purpose)
    or unencrypted ("nbd_insecure" purpose) access to the NBD server is
    allowed on those networks.
    After each event on a network object in the database, we decide on
    which interfaces the NBD port should be enabled on this host. If this
    set of interfaces did not change, that is, last time we've updated the
    firewall with the same set of interfaces, then we do not call the script
    again. We wait for 5 seconds after each event has been processed, to
    rate-limit the event loop and its database queries.
    In case of failures other than EVENTS_LOST, we wait for an additional 5
    seconds and then reregister and continue the event loop. In case of
    EVENTS_LOST, we reregister and continue without waiting.
*)

val watch_networks_for_nbd_changes : unit -> unit

val _watch_networks_for_nbd_changes : Context.t -> update_firewall:(string list -> unit) -> wait_after_event_seconds:float -> wait_after_failure_seconds:float -> unit
(** This version of {!watch_networks_for_nbd_changes} is for unit testing
    purposes - it calls the [update_firewall] function, instead of invoking a
    script, and how many seconds it waits after failures is specified by
    [wait_after_failure_seconds]. *)
