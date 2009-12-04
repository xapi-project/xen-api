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
(** Module that defines API functions for Network objects
 * @group XenAPI functions
 *)

(** Instantiate the bridge associated to this network on the localhost, and bring
   up the PIFs on the localhost that are on this network, provided it wouldn't 
   destroy existing Networks (e.g. slaves of a bond) in use by something (VIF or management interface).
   Note special-case handling of new management interfaces: we skip the 
   check for the existing management interface (essential otherwise switching
   from a bond slave to a bond master would fail) and we make sure to call
   {!Nm.bring_pif_up} with the [management_interface] argument so it can make sure
   the default gateway is set up correctly *)
val attach_internal :
  ?management_interface:bool ->
  __context:Context.t -> self:[ `network ] Ref.t -> unit -> unit

(** Remove the bridge associated to this network *)
val detach : string -> unit

(** Makes the network immediately available on a particular host (Network.attach is hidden from docs) *)
val attach :
  __context:Context.t -> network:[ `network ] Ref.t -> host:'a -> unit

val network_gc_func : unit -> unit

(** Internal fn used by slave to create new network records on master during pool join operation *)
val pool_introduce :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  other_config:(string * string) list -> bridge:string -> [ `network ] Ref.t

(** Attempt to create a bridge with a unique name *)
val create :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  other_config:(string * string) list ->
  tags:string list -> [ `network ] Ref.t

(** WARNING WARNING WARNING: called with the master dispatcher lock; do nothing but basic DB calls
    here without being really sure *)
val destroy : __context:Context.t -> self:[ `network ] Ref.t -> unit

(** Create a placeholder for a named binary blob of data that is associated with this pool *)
val create_new_blob :
  __context:Context.t ->
  network:[ `network ] Ref.t ->
  name:string -> mime_type:string -> [ `blob ] Ref.t
