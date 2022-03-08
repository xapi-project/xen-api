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

let get_contents =
  call ~name:"get_contents" ~in_product_since:"rel_next"
    ~doc:"Obtain the contents of the TPM" ~secret:true
    ~params:[(Ref _vtpm, "self", "The VTPM reference")]
    ~result:(String, "The contents") ~hide_from_docs:true
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let set_contents =
  call ~name:"set_contents" ~in_product_since:"rel_next"
    ~doc:"Introduce new contents for the TPM" ~secret:true
    ~params:
      [
        (Ref _vtpm, "self", "The VTPM reference")
      ; (String, "contents", "The new contents")
      ]
    ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let t =
  create_obj ~in_db:true ~in_oss_since:oss_since_303 ~persist:PersistEverything
    ~lifecycle:
      [
        (Published, rel_rio, "Added VTPM stub")
      ; (Extended, rel_next, "Added ability to manipulate contents")
      ; (Extended, rel_next, "Added VTPM profiles")
      ; (Changed, rel_next, "Removed backend field")
      ]
    ~gen_constructor_destructor:true ~name:_vtpm ~descr:"A virtual TPM device"
    ~gen_events:false ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      [
        uid _vtpm
      ; field ~qualifier:StaticRO ~ty:(Ref _vm) "VM"
          "The virtual machine the TPM is attached to"
      ; field ~qualifier:DynamicRO
          ~ty:(Map (String, String))
          ~lifecycle:[(Published, rel_next, "Added VTPM profiles")]
          "profile" "The security properties that define how the TPM is handled"
      ; field ~qualifier:DynamicRO ~ty:(Ref _secret) ~internal_only:true
          ~lifecycle:[(Published, rel_next, "Added VTPM contents")]
          "contents" "The contents of the TPM"
      ]
    ~messages:[get_contents; set_contents]
    ()
