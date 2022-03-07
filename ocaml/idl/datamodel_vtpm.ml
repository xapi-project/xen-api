(*
   Copyright (C) Citrix Systems Inc.

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

let t =
  create_obj ~in_db:true ~in_oss_since:oss_since_303 ~persist:PersistEverything
    ~lifecycle:[(Published, rel_rio, "Added VTPM stub")]
    ~gen_constructor_destructor:true ~name:_vtpm ~descr:"A virtual TPM device"
    ~gen_events:false ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN ~messages:[]
    ~contents:
      [
        uid _vtpm
      ; field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine"
      ; field ~qualifier:StaticRO ~ty:(Ref _vm) "backend"
          "the domain where the backend is located"
      ]
    ()
