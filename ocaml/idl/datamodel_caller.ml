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
  call ~name:"create" ~doc:"Create a new caller record" ~in_oss_since:None
    ~lifecycle
    ~params:
      [
        (String, "name_label", "Human-readable label for the caller")
      ; (String, "name_description", "Human-readable description for the caller")
      ; ( String
        , "user_agent"
        , "User agent matching pattern. Empty string is a full wildcard; a \
           trailing '*' makes the field a prefix pattern; otherwise the field \
           is matched exactly."
        )
      ; ( String
        , "client_ip"
        , "Client IP matching pattern. Same wildcard semantics as user_agent."
        )
      ]
    ~result:(Ref _caller, "Reference to the newly created caller")
    ~allowed_roles:_R_POOL_ADMIN ()

let destroy =
  call ~name:"destroy" ~doc:"Destroy the given caller record" ~in_oss_since:None
    ~lifecycle
    ~params:[(Ref _caller, "self", "The caller to destroy")]
    ~allowed_roles:_R_POOL_ADMIN ()

let add_group =
  call ~name:"add_group" ~doc:"Add a caller to a group" ~in_oss_since:None
    ~lifecycle
    ~params:
      [
        (Ref _caller, "self", "The caller to update")
      ; (String, "group", "Group the caller is being added to")
      ]
    ~allowed_roles:_R_POOL_OP ()

let remove_group =
  call ~name:"remove_group" ~doc:"Remove a caller from a group"
    ~in_oss_since:None ~lifecycle
    ~params:
      [
        (Ref _caller, "self", "The caller to update")
      ; (String, "group", "Group the caller is removed from")
      ]
    ~allowed_roles:_R_POOL_OP ()

let query_usage =
  call ~name:"query_usage"
    ~doc:
      "Return cumulative token and call count statistics for this caller, \
       taken from the in-memory counters since XAPI startup."
    ~in_oss_since:None ~lifecycle
    ~params:[(Ref _caller, "self", "The caller to query")]
    ~result:
      ( Map (String, String)
      , "Map with keys \"tokens\" (float) and \"calls\" (int)"
      )
    ~allowed_roles:_R_POOL_OP ()

let query_group_usage =
  call ~name:"query_group_usage"
    ~doc:
      "Return cumulative token and call count statistics summed across every \
       caller in the named group."
    ~in_oss_since:None ~lifecycle
    ~params:[(String, "group", "Caller group to aggregate over")]
    ~result:
      ( Map (String, String)
      , "Map with keys \"tokens\" (float) and \"calls\" (int)"
      )
    ~allowed_roles:_R_POOL_OP ()

let query_all_usage =
  call ~name:"query_all_usage"
    ~doc:
      "Return per-caller usage for every known caller, sorted by token use \
       descending."
    ~in_oss_since:None ~lifecycle ~params:[]
    ~result:
      ( Set (Set String)
      , "Rows of [uuid; name_label; tokens; calls], highest tokens first"
      )
    ~allowed_roles:_R_POOL_OP ()

let t =
  create_obj ~name:_caller ~descr:"XAPI caller description and rate limiting"
    ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
    ~in_db:true ~lifecycle ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      [
        uid _caller ~lifecycle
      ; namespace ~name:"name" ~contents:(names None RW ~lifecycle) ()
      ; field ~qualifier:StaticRO ~ty:String ~lifecycle "user_agent"
          "User agent matching pattern. Empty string is a full wildcard; a \
           trailing '*' makes the field a prefix pattern; otherwise the field \
           is matched exactly."
          ~ignore_foreign_key:true ~default_value:(Some (VString ""))
      ; field ~qualifier:StaticRO ~ty:String ~lifecycle "client_ip"
          "Client IP matching pattern. Same wildcard semantics as user_agent."
          ~ignore_foreign_key:true ~default_value:(Some (VString ""))
      ; field ~qualifier:DynamicRO ~ty:DateTime ~lifecycle "last_access"
          "Last time a call was received from this caller"
          ~ignore_foreign_key:true ~default_value:(Some (VDateTime Date.epoch))
      ; field ~qualifier:RW ~ty:(Set String) ~lifecycle "groups"
          "Groups to which this caller has been assigned"
          ~ignore_foreign_key:true ~default_value:(Some (VSet []))
      ; field ~qualifier:StaticRO ~ty:(Ref _rate_limit) ~lifecycle "rate_limit"
          "Rate limiter attached to this caller, if any. Populated via \
           Rate_limit.add_caller rather than set directly."
          ~default_value:(Some (VRef null_ref))
      ]
    ~messages:
      [
        create
      ; destroy
      ; add_group
      ; remove_group
      ; query_usage
      ; query_group_usage
      ; query_all_usage
      ]
    ()
