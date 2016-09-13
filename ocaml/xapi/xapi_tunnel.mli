(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
(** Module that defines API functions for tunnels
 * @group Networking
*)

(** Create a tunnel for a given transport PIF and network *)
val create :
  __context:Context.t ->
  transport_PIF:[ `PIF ] Ref.t ->
  network:[ `network ] Ref.t ->
  [ `tunnel ] Ref.t

(** Internal version of [create] without checks/exceptions and auto-plugging *)
val create_internal :
  __context:Context.t ->
  transport_PIF:[ `PIF ] Ref.t ->
  network:[ `network ] Ref.t ->
  host:[ `host ] Ref.t ->
  [ `tunnel ] Ref.t * [ `PIF ] Ref.t

(** Destroy a tunnel. Removes the tunnel object as well as the tunnel access PIF. *)
val destroy : __context:Context.t -> self:[ `tunnel ] Ref.t -> unit
