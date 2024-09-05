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
      "Select a version of the driver to become active after reboot or \
       immediately if currently no version is active"
    ~params:
      [
        (Ref _host_driver, "self", "Driver to become active (after reboot).")
      ; (String, "version", "Driver version to become active (after reboot).")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let deselect =
  call ~name:"deselect" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "Deselect the currently active version of this driver after reboot. No \
       action will be taken if no version is currently active."
    ~params:
      [(Ref _host_driver, "self", "Driver to become inactive (after reboot).")]
    ~allowed_roles:_R_POOL_ADMIN ()

let t =
  create_obj ~in_db:true ~in_oss_since:None ~persist:PersistEverything
    ~lifecycle:[] ~name:_host_driver ~gen_constructor_destructor:false
    ~descr:"A multi-version driver on a host" ~gen_events:true ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      [
        uid _host_driver
      ; field ~lifecycle:[] ~qualifier:StaticRO ~ty:(Ref _host) "host"
          "Host where this driver is installed" ~default_value:(Some (VRef ""))
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "name"
          "Name identifying the driver uniquely"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:(Set String) "versions"
          "Versions available of this driver available for selection"
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "active_version"
          "Currently active version of this driver, if any. An empty string \
           means none is active."
      ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:String "selected_version"
          "Version (if any) selected to become active after reboot. An empty \
           string means none is selected."
      ]
    ~messages:[select; deselect] ()
