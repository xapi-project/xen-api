(*
 * Copyright (C) 2022 Citrix Systems Inc.
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

let lifecycle = []

let gpg_key_type = Enum ("gpg_key_type", [("rpm_pubkey", "RPM GPG public key")])

let t =
  create_obj ~name:_gpg_key ~descr:"Description" ~doccomments:[]
    ~gen_constructor_destructor:false ~gen_events:true ~in_db:true ~lifecycle
    ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_READ_ONLY ~messages:[]
    ~contents:
      [
        uid _gpg_key ~lifecycle
      ; field ~qualifier:StaticRO ~lifecycle ~ty:String
          ~default_value:(Some (VString "")) "name"
          "The name of the GPG key file"
      ; field ~qualifier:StaticRO ~lifecycle ~ty:DateTime
          ~default_value:(Some (VDateTime Date.never)) "created"
          "Date on which the key was created"
      ; field ~qualifier:StaticRO ~lifecycle ~ty:String
          ~default_value:(Some (VString "")) "fingerprint"
          "The fingerprint of the GPG public key"
      ; field ~qualifier:DynamicRO ~lifecycle ~ty:Bool
          ~default_value:(Some (VBool false)) "uninstalled"
          "The key has been uninstalled or not"
      ; field ~qualifier:DynamicRO ~lifecycle ~ty:gpg_key_type
          ~default_value:(Some (VEnum "rpm_pubkey")) "type"
          "The type of the GPG key"
      ]
    ()
