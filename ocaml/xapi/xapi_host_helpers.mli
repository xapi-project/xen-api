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

(** Common code for dealing with Hosts.
 * @group Host Management
*)

val assert_operation_valid :
  __context:Context.t ->
  self:API.ref_host ->
  op:API.host_allowed_operations ->
  unit
(** [assert_operation_valid ~__context ~self ~op] checks the operation [op] is
    currently valid on [host]. There are various checks that are performed,
    for example:
    {ul
    {- Ony one 'provisioning-type' operation is allowed at once}
    {- Reboot / Shutdown cannot run concurrently}
    {- Shutdown and Reboot are only allowed if the host is disabled}
    }*)

val assert_host_disabled :
  __context:Context.t ->
  host:API.ref_host ->
  unit
(** [assert_host_disabled ~__context ~host] raises an API error
    host_not_disabled if the host is not disabled. *)

val update_allowed_operations :
  __context:Context.t ->
  self:API.ref_host ->
  unit
(** [update_allowed_operations ~__context ~self] updates the
    allowed_operations database field with all the operations that are
    currently allowed given the current state of the host. *)

val update_allowed_operations_all_hosts :
  __context:Context.t ->
  unit
(** [update_allowd_operations_all_hosts ~__context] runs
    [update_alloed_operations] for each host *)

val cancel_tasks :
  __context:Context.t ->
  self:API.ref_host ->
  all_tasks_in_db:API.ref_task list ->
  task_ids:string list -> unit
(** [cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids] is a helper
    utility for batch cancelling tasks associated with a previously dead host.
    See the file `cancel_tasks.ml` and the function [Helpers.cancel_tasks] for
    more context. *)

val mark_host_as_dead :
  __context:Context.t ->
  host:API.ref_host ->
  reason:string ->
  unit
(** [mark_host_as_dead ~__context ~host ~reason] is called on the master when a
    host is declaring it's going to be dead soon, via the [tickle_heartbeat]
    code. The host will be added to the Xapi_globs table of
    [hosts_which_are_shutting_down], the host_metrics live field will be set to
    false and any pre and post declare_dead scripts will be executed. *)

val consider_enabling_host : __context:Context.t -> unit
(** [consider_enabling_host ~__context] is called at the end of the xapi
    startup sequence. It will enable the host unless:
    {ul
    {- the user asked the host to be disabled and there was a problem}
    {- HA is enabled and one-or-more PBDs failed to plug}
    {- `disabled_until_next_reboot` is set in the local DB}}
*)

val consider_enabling_host_request : __context:Context.t -> unit
(** [consider_enabling_host_request ~__context] will ensure that
    [consider_enabling_host] is called soon. It will coalesce multiple requests
    that are made. *)

val user_requested_host_disable : bool ref
(** [user_requested_host_disable] : true if so. Persistent until xapi is
    restarted *)

val assert_startup_complete : unit -> unit
(** [assert_startup_complete ()] will raise `host_still_booting` if the startup
    sequence is not yet complete. *)

module Host_requires_reboot : sig
  val set : unit -> unit
  (** [set ()] is used to signal the host needs a reboot. This could be, for
      example, that the dom0 memory settings have changed and a reboot is
      required for them to take effect. *)

  val get : unit -> bool
  (** [get ()] returns [true] if the host needs to be rebooted *)
end

module Configuration : sig
  val set_initiator_name : string -> unit
  (** [set_initiator_name iqn] will write the iscsi initiator configuration to
      the file specified in Xapi_globs (usually /etc/iscsi/initiatorname.iscsi)
      *)

  val set_multipathing : bool -> unit
  (** [set_multipathing enabled] will touch the file specified in Xapi_globs
      (usually /var/run/nonpersistent/multipath_enabled) if [enabled] is true,
      otherwise it will remove the file. *)

  val sync_config_files : __context:Context.t -> unit
  (** [sync_config_files ~__context] ensures that the iscsi iqn and
      multipathing configuration files reflect the values of the corresponding
      fields in xapi's database. It should be called at startup on every host
      BEFORE the other_config watcher [start_watcher_thread] is started *)

  val watch_other_configs : __context:Context.t -> float -> (string * bool) -> (string * bool)
  (** [watch_other_configs ~__context timeout] returns a function that performs
      one iteration of watching Host.other_config. If an update occurs this
      will check whether the iscsi_iqn field in other-config is correctly
      reflected in the field Host.iscsi_iqn, and if not it will call
      Host.set_iscsi_iqn with the value specified in other-config. This is
      intended to be run on the master. The calls will not be made if the pool
      is in rolling upgrade mode, so when the pool exits rolling upgrade mode
      all hosts are checked. The returned function has type [token -> token],
      where [token] is a (string * bool). The initial value should be a tuple
      of the empty string and a boolean whose value is true if the pool is
      currently in rolling pool upgrade mode, and the returned value should be
      used for further invocations. This function is exposed only for unit
      testing, and should not be invoked directly.*)

  val start_watcher_thread : __context:Context.t -> unit
  (** [start_watcher_thread ~__context] will start a thread that watches the
      other-config field of all hosts and keeps the iscsi_iqn value in sync
      with the first-class field Host.iscsi_iqn. As with watch_other_configs,
      this function must only be run on the master. *)
end
