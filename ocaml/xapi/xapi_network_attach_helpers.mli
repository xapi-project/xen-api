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
(** Assertion helpers used when attaching a network
 * @group Networking
*)

(** Raises an exception if the network has VIFs in use on the host *)
val assert_network_has_no_vifs_in_use_on_me :
  __context:Context.t ->
  host:[ `host ] Ref.t -> network:[ `network ] Ref.t -> unit

(** Raises an exception when the [disallow_unplug] flag is set *)
val assert_pif_disallow_unplug_not_set :
  __context:Context.t -> [ `PIF ] Ref.t -> unit

(** Returns a list of PIFs on a given Network and on a given Host *)
val get_local_pifs :
  __context:Context.t ->
  network:[ `network ] Ref.t ->
  host:[ `host ] Ref.t ->
  [ `PIF ] Ref.t list

(** Raises an exception if the PIF is a bond slave *)
val assert_no_slave :
  __context:Context.t ->
  [ `PIF ] Ref.t ->
  unit

(** Raises an exception if any network cannot be seen *)
val assert_can_see_named_networks :
  __context:Context.t ->
  vm:[ `VM ]Ref.t -> host:[ `host ] Ref.t -> [ `network ] Ref.t list -> unit

(** Raises an exception if the network cannot be attached. *)
val assert_can_attach_network_on_host :
  __context:Context.t ->
  self:[ `network ] Ref.t ->
  host:[ `host ] Ref.t ->
  unit
