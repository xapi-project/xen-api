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

(** Functions for implementing 'High Availability' (HA).
    @group High Availability (HA) *)

val ha_redo_log : Redo_log.redo_log
(** The redo log instance used for HA *)

(******************************************************************************)
(** {2 Interface with the low-level HA subsystem} *)

module Monitor :
  sig
    (** Control the background HA monitoring thread *)

    val plan_out_of_date : bool ref
    (** Used to explicitly signal that we should replan *)

    val stop : unit -> unit
  end

val ha_prevent_restarts_for : 'a -> int64 -> unit
(** Called by MTC in Orlando Update 1 to temporarily block the VM restart thread. *)

val on_server_restart : unit -> unit
(** Called when xapi restarts: server may be in emergency mode at this point.
    We need to inspect the local configuration and if HA is supposed to be armed
    we need to set everything up.
    Note that the master shouldn't be able to activate HA while we are offline
    since that would cause us to come up with a broken configuration (the
    enable-HA stage has the critical task of synchronising the HA configuration
    on all the hosts). So really we only want to notice if the Pool has had
    HA disabled while we were offline. *)

val on_database_engine_ready : unit -> unit
(** Called in the master xapi startup when the database is ready. We set all
    hosts (including this one) to disabled, then signal the monitor thread to look.
    It can then wait for slaves to turn up before trying to restart VMs. *)

(******************************************************************************)
(** {2 Internal API calls to configure individual hosts} *)

val ha_disable_failover_decisions : 'a -> 'b -> unit
(** Internal API call to prevent this node making an unsafe failover decision.
    This call is idempotent. *)

val ha_disarm_fencing : 'a -> 'b -> unit
(** Internal API call to disarm localhost. If the daemon is missing then we
    return success. Either fencing was previously disabled and the daemon has
    shutdown OR the daemon has died and this node will fence shortly...
*)

val ha_stop_daemon : 'a -> 'b -> unit
(** Internal API call to stop the HA daemon. This call is idempotent. *)

val emergency_ha_disable : 'a -> bool -> unit
(** Emergency-mode API call to disarm localhost *)

val ha_release_resources : Context.t -> 'a -> unit
(** Internal API call to release any HA resources after the system has been
    shutdown. This call is idempotent. Modified for CA-48539 to call
    vdi.deactivate before vdi.detach. *)

val ha_wait_for_shutdown_via_statefile : 'a -> 'b -> unit
(** Internal API call which blocks until this node's xHA daemon spots the
    invalid statefile and exits cleanly. If the daemon survives but the
    statefile access is lost then this function will return an exception and
    the no-statefile shutdown can be attempted.
*)

val preconfigure_host :
  Context.t ->
  [ `host ] API.Ref.t ->
  [ `VDI ] API.Ref.t list ->
  [ `VDI ] API.Ref.t ->
  string -> unit
(** Internal API call to preconfigure localhost *)

val join_liveset : 'a -> 'b Ref.t -> unit

val propose_new_master : __context:'a -> address:string -> manual:'b -> unit
(** First phase of a two-phase commit of a new master *)

val commit_new_master : __context:Context.t -> address:string -> unit
(** Second phase of a two-phase commit of a new master *)

val abort_new_master : __context:'a -> address:string -> unit

(******************************************************************************)
(** {2 External API calls} *)

(** {3 Pool.*_ha API calls} *)

val disable : Context.t -> unit

val enable :
  Context.t -> [ `SR ] API.Ref.t list -> (string * string) list -> unit

(** {3 Functions called by host.* API calls} *)

val before_clean_shutdown_or_reboot : __context:Context.t -> host:'a -> unit
(** Called before shutting down or rebooting a host
    (called by the host.shutdown, host.reboot API functions). *)

