(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

(** A host driver variant, referred to by a host driver *)
module Variant : sig
  val create :
       __context:Context.t
    -> name:string
    -> version:string
    -> driver:[`Host_driver] API.Ref.t
    -> hw_present:bool
    -> priority:float
    -> dev_status:string
    -> [`Driver_variant] Ref.t

  val destroy : __context:Context.t -> self:[`Driver_variant] API.Ref.t -> unit

  val select : __context:Context.t -> self:[`Driver_variant] API.Ref.t -> unit
end

val create :
     __context:Context.t
  -> host:[`host] API.Ref.t
  -> name:string
  -> friendly_name:string
  -> _type:string
  -> description:string
  -> info:string
  -> active_variant:[`Driver_variant] API.Ref.t
  -> selected_variant:[`Driver_variant] API.Ref.t
  -> [`Host_driver] Ref.t
(** A host driver *)

val destroy : __context:Context.t -> self:[`Host_driver] API.Ref.t -> unit

val select :
     __context:Context.t
  -> self:[`Host_driver] API.Ref.t
  -> variant:[`Driver_variant] API.Ref.t
  -> unit

val deselect : __context:Context.t -> self:[`Host_driver] API.Ref.t -> unit
(** This is just for completeness; don't see a use case right now *)

val scan : __context:Context.t -> host:[`host] API.Ref.t -> unit
(** scan and re-scan scan the [host] for drivers and update the xapi
    database accordingly. Previous entries are purged (this may change
    in the future *)

val rescan : __context:Context.t -> host:[`host] API.Ref.t -> unit
