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

val make_error_opt: t -> API.ref_VM -> (string * string list) option
(** [make_error_opt x] returns None if has_pv_drivers,
    	and Some(code,params) otherwise. *)

val is_windows_and_orlando_or_newer: Db_actions.vM_guest_metrics_t -> bool
(** True if the pv driver version info is a Windows version and has a build
    	number (build number was new in Orlando). *)
