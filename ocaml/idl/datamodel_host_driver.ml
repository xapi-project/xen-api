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

(** A Host_driver instance represents a driver on a host that is
    installed in multiple versions. At most one version is active; a
    different version can be selected to become active after reboot. If
    no version is active, selecting a version makes it the active
    version immediately. *)

let select =
  call ~name:"select" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "UNSUPPORTED. Select a variant of the driver to become active after \
       reboot or immediately if currently no version is active"
    ~params:
      [
        (Ref _host_driver, "self", "Driver to become active (after reboot).")
      ; ( Ref _driver_variant
        , "variant"
        , "Driver version to become active (after reboot)."
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let deselect =
  call ~name:"deselect" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "UNSUPPORTED. Deselect the currently active variant of this driver after \
       reboot. No action will be taken if no variant is currently active."
    ~params:
      [(Ref _host_driver, "self", "Driver to become inactive (after reboot).")]
    ~allowed_roles:_R_POOL_ADMIN ()

let rescan =
  call ~name:"rescan" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "UNSUPPORTED. Re-scan a host's drivers and update information about \
       them. This is mostly  for trouble shooting."
    ~params:[(Ref _host, "host", "Update driver information of this host.")]
    ~allowed_roles:_R_POOL_ADMIN ()

let t =
  create_obj ~in_db:true ~in_oss_since:None ~persist:PersistEverything
    ~lifecycle:[] ~name:_host_driver ~gen_constructor_destructor:false
    ~descr:"UNSUPPORTED. A multi-version driver on a host" ~gen_events:true
    ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      [
        uid _host_driver ~lifecycle:[]
      ; field ~lifecycle:[] ~qualifier:StaticRO ~ty:(Ref _host) "host"
          "Host where this driver is installed" ~default_value:(Some (VRef ""))
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "name"
          "Name identifying the driver uniquely"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "friendly_name"
          "Descriptive name, not used for identification"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:(Set (Ref _driver_variant))
          "variants" "Variants of this driver available for selection"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:(Ref _driver_variant)
          "active_variant"
          "Currently active variant of this driver, if any, or Null."
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:(Ref _driver_variant)
          "selected_variant"
          "Variant (if any) selected to become active after reboot. Or Null"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "type"
          "Device type this driver supports, like network or storage"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "description"
          "Description of the driver"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "info"
          "Information about the driver"
      ]
    ~messages:[select; deselect; rescan] ()
