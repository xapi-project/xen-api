(*
 * Copyright (C) 2023 Cloud Software Group
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

let t =
  create_obj ~name:_rate_limit ~descr:"Rate limiting policy for a XAPI client"
    ~doccomments:[] ~gen_constructor_destructor:true ~gen_events:true
    ~in_db:true ~lifecycle:[] ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      ([uid _rate_limit ~lifecycle]
      @ [
          field ~qualifier:StaticRO ~ty:String ~lifecycle "client_id"
            "An identifier for the rate limited client" ~ignore_foreign_key:true
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:Float ~lifecycle "burst_size"
            "Amount of tokens that can be consumed in one burst"
            ~ignore_foreign_key:true ~default_value:(Some (VFloat 0.))
        ; field ~qualifier:StaticRO ~ty:Float ~lifecycle "fill_rate"
            "Tokens added to token bucket per second" ~ignore_foreign_key:true
            ~default_value:(Some (VFloat 0.))
        ]
      )
    ~messages:[] ()
