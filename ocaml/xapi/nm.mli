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
(** Helper module to plug and unplug PIFs
 * @group Networking
*)

val bring_pif_up :
  __context:Context.t -> ?management_interface:bool -> API.ref_PIF -> unit
(** Bring up and configure a PIF and related infrastructure on this host *)

val bring_pif_down : __context:Context.t -> ?force:bool -> API.ref_PIF -> unit
(** Bring down a PIF and related infrastructure on this host *)

val with_local_lock : (unit -> 'a) -> 'a
(** Execute a given function under the control of a mutex *)

val is_dom0_interface : API.pIF_t -> bool
(** [is_dom0_interface pif_r] returns true if pif_r is a network interface
    which has a dom0 endpoint *)

(* Internals *)
val maybe_update_master_pif_mac :
  __context:Context.t -> API.bond_t -> API.pIF_t -> API.ref_PIF -> API.pIF_t
