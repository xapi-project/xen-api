(*
 * Copyright (C) Cloud Software Group
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

let create =
  call ~name:"create" ~doc:"Create a new rate limiter" ~in_oss_since:None
    ~lifecycle
    ~params:
      [
        (String, "name_label", "Human-readable label for the rate limiter")
      ; ( String
        , "name_description"
        , "Human-readable description for the rate limiter"
        )
      ; (Float, "burst_size", "Maximum tokens that the bucket can hold")
      ; (Float, "fill_rate", "Tokens added to the bucket per second")
      ]
    ~result:(Ref _rate_limit, "Reference to the newly created rate limiter")
    ~allowed_roles:_R_POOL_ADMIN ()

let destroy =
  call ~name:"destroy"
    ~doc:
      "Destroy the rate limiter. Also clears the rate_limit field of any \
       caller currently attached to it."
    ~in_oss_since:None ~lifecycle
    ~params:[(Ref _rate_limit, "self", "The rate limiter to destroy")]
    ~allowed_roles:_R_POOL_ADMIN ()

let add_caller =
  call ~name:"add_caller"
    ~doc:
      "Attach the given caller to this rate limiter. Replaces any rate limiter \
       previously attached to the caller."
    ~in_oss_since:None ~lifecycle
    ~params:
      [
        (Ref _rate_limit, "self", "The rate limiter")
      ; (Ref _caller, "caller", "The caller to attach")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let remove_caller =
  call ~name:"remove_caller"
    ~doc:"Detach the given caller from this rate limiter" ~in_oss_since:None
    ~lifecycle
    ~params:
      [
        (Ref _rate_limit, "self", "The rate limiter")
      ; (Ref _caller, "caller", "The caller to detach")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_burst_size =
  call ~name:"set_burst_size" ~doc:"Set the burst size of the rate limiter"
    ~in_oss_since:None ~lifecycle
    ~params:
      [
        (Ref _rate_limit, "self", "The rate limiter")
      ; (Float, "value", "The new burst size, must be positive")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_fill_rate =
  call ~name:"set_fill_rate" ~doc:"Set the fill rate of the rate limiter"
    ~in_oss_since:None ~lifecycle
    ~params:
      [
        (Ref _rate_limit, "self", "The rate limiter")
      ; (Float, "value", "The new fill rate (tokens/second), must be positive")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let t =
  create_obj ~name:_rate_limit
    ~descr:"A rate limiter associated with one or more callers" ~doccomments:[]
    ~gen_constructor_destructor:false ~gen_events:true ~in_db:true ~lifecycle
    ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      [
        uid _rate_limit ~lifecycle
      ; namespace ~name:"name" ~contents:(names None RW ~lifecycle) ()
      ; field ~qualifier:DynamicRO ~ty:(Set (Ref _caller)) ~lifecycle "callers"
          "The set of callers attached to this rate limiter"
      ; field ~qualifier:StaticRO ~ty:Float ~lifecycle "burst_size"
          "Maximum tokens that the bucket can hold" ~ignore_foreign_key:true
          ~default_value:(Some (VFloat 0.))
      ; field ~qualifier:StaticRO ~ty:Float ~lifecycle "fill_rate"
          "Tokens added to the bucket per second" ~ignore_foreign_key:true
          ~default_value:(Some (VFloat 0.))
      ]
    ~messages:
      [
        create; destroy; add_caller; remove_caller; set_burst_size; set_fill_rate
      ]
    ()
