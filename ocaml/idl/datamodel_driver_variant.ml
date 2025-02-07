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

open Datamodel_types
open Datamodel_common
open Datamodel_roles

let select =
  call ~name:"select" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "UNSUPPORTED Select this variant of a driver to become active after \
       reboot or immediately if currently no version is active"
    ~params:
      [
        ( Ref _driver_variant
        , "self"
        , "Driver variant to become active (after reboot)."
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let t =
  create_obj ~in_db:true ~in_oss_since:None ~persist:PersistEverything
    ~lifecycle:[] ~name:_driver_variant ~gen_constructor_destructor:false
    ~descr:"UNSUPPORTED. Variant of a host driver" ~gen_events:true
    ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      [
        uid _driver_variant ~lifecycle:[]
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "name"
          "Name identifying the driver variant within the driver"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:(Ref _host_driver) "driver"
          "Driver this variant is a part of"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "version"
          "Unique version of this driver variant"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:Bool "hardware_present"
          "True if the hardware for this variant is present on the host"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:Float "priority"
          "Priority; this needs an explanation how this is ordered"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "status"
          "Development and release status of this variant, like 'alpha'"
      ]
    ~messages:[select] ()
