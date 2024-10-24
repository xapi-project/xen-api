(*
 * Copyright (c) Cloud Software Group, Inc.
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

open Datamodel_types
open Datamodel_common
open Datamodel_roles

let placement_policy =
  Enum
    ( "placement_policy"
    , [
        ("anti_affinity", "Anti-affinity placement policy")
      ; ("normal", "Default placement policy")
      ]
    )

let t =
  create_obj ~name:_vm_group ~descr:"A VM group" ~doccomments:[]
    ~gen_constructor_destructor:true ~gen_events:true ~in_db:true ~lifecycle:[]
    ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_VM_ADMIN ~messages:[]
    ~contents:
      [
        uid _vm_group
          ~lifecycle:
            [(Published, rel_rio, "Unique identifier/object reference")]
      ; namespace ~name:"name"
          ~contents:(names ~lifecycle:[(Published, rel_rio, "")] None RW)
          ()
      ; field ~qualifier:StaticRO ~lifecycle:[] ~ty:placement_policy "placement"
          ~default_value:(Some (VEnum "normal"))
          "The placement policy of the VM group"
      ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:(Set (Ref _vm)) "VMs"
          "The list of VMs associated with the group"
      ]
    ()
