(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

type t
(** Represents the information known about a VM's PV drivers *)

val of_guest_metrics: Db_actions.vM_guest_metrics_t option -> t
(** [of_guest_metrics x] returns an type t representing the PV driver version information *)

val has_pv_drivers: t -> bool
(** [has_pv_drivers x] returns true if the guest is running some version of PV
    drivers. *)

val is_up_to_date: t -> bool
(** [is_up_to_date x] returns true if we know the PV driver version is the latest
    from the perspective of the host (not the Pool) *)

val is_ok_for_migrate: t -> bool
(** [is_ok_for_migrate x] returns true if this VM should be allowed to migrate
    (generally we want to allow this for rolling upgrade) *)

val make_error_opt: t -> API.ref_VM -> API.ref_VM_guest_metrics -> (string * string list) option
(** [make_error_opt x] returns None if the operation should not be 
    blocked because of the PV driver versions and Some(code, params) otherwise.
    Note that we are conservative and block everything (except migrate see [is_ok_for_migrate]) 
    when the PV drivers are not the most recent version. *)

val get_latest_tools_vsn: unit -> unit
(** [get_latest_tools_vsn ()] sets the global variable Xapi_globs.tools_version with
    the latest PV tools iso detected in the root filesystem. *)
