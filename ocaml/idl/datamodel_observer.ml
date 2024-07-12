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

let register =
  call ~name:"register" ~in_oss_since:None ~lifecycle:[] ~hide_from_docs:true
    ~doc:"Register a observer on a particular host"
    ~params:
      [
        (Ref _observer, "self", "The observer")
      ; (Ref _host, "host", "The host to be registered on")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let unregister =
  call ~name:"unregister" ~in_oss_since:None ~lifecycle:[] ~hide_from_docs:true
    ~doc:"Unegister a observer on a particular host"
    ~params:
      [
        (Ref _observer, "self", "The observer")
      ; (Ref _host, "host", "The host to be unregistered on")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_hosts =
  call ~name:"set_hosts" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Sets the hosts that the observer is to be registered on"
    ~params:
      [
        (Ref _observer, "self", "The observer")
      ; (Set (Ref _host), "value", "Hosts the observer is registered on")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_enabled =
  call ~name:"set_enabled" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "Enable / disable this observer which will stop the observer from \
       producing observability information"
    ~params:
      [
        (Ref _observer, "self", "The observer")
      ; ( Bool
        , "value"
        , "If the observer is to be enabled (true) or disabled (false)"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_attributes =
  call ~name:"set_attributes" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "Set the attributes of an observer. These are used to emit metadata by \
       the observer"
    ~params:
      [
        (Ref _observer, "self", "The observer")
      ; ( Map (String, String)
        , "value"
        , "The attributes that the observer emits as part of the data"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_endpoints =
  call ~name:"set_endpoints" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the file/HTTP endpoints the observer sends data to"
    ~params:
      [
        (Ref _observer, "self", "The observer")
      ; ( Set String
        , "value"
        , "The endpoints that the observer will export data to. A URL or the \
           string 'bugtool'. This can refer to an enpoint to the local file \
           system"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_components =
  call ~name:"set_components" ~in_oss_since:None ~lifecycle:[]
    ~doc:
      "Set the components on which the observer will broadcast to. i.e. xapi, \
       xenopsd, networkd, etc."
    ~params:
      [
        (Ref _observer, "self", "The observer")
      ; (Set String, "value", "The components the observer will broadcast to")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let t =
  create_obj ~name:_observer
    ~descr:
      "Describes an observer which will control observability activity in the \
       Toolstack"
    ~doccomments:[] ~gen_constructor_destructor:true ~gen_events:true
    ~in_db:true ~lifecycle:[] ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      ([
         uid _observer ~lifecycle:[]
       ; namespace ~name:"name" ~contents:(names None RW) ()
       ]
      @ [
          field ~qualifier:StaticRO ~ty:(Set (Ref _host)) ~lifecycle:[] "hosts"
            "The list of hosts the observer is active on. An empty list means \
             all hosts"
            ~ignore_foreign_key:true ~default_value:(Some (VSet []))
        ; field ~qualifier:StaticRO
            ~ty:(Map (String, String))
            ~lifecycle:[] "attributes"
            "Attributes that observer will add to the data they produce"
            ~default_value:(Some (VMap []))
        ; field ~qualifier:StaticRO ~ty:(Set String) ~lifecycle:[] "endpoints"
            "The list of endpoints where data is exported to. Each endpoint is \
             a URL or the string 'bugtool' refering to the internal logs"
            ~default_value:(Some (VSet []))
        ; field ~qualifier:StaticRO ~ty:(Set String) ~lifecycle:[] "components"
            "The list of xenserver components the observer will broadcast. An \
             empty list means all components"
            ~default_value:(Some (VSet []))
        ; field ~qualifier:StaticRO ~ty:Bool ~lifecycle:[] "enabled"
            "This denotes if the observer is enabled. true if it is enabled \
             and false if it is disabled"
            ~default_value:(Some (VBool false))
        ]
      )
    ~messages:
      [
        register
      ; unregister
      ; set_hosts
      ; set_enabled
      ; set_attributes
      ; set_endpoints
      ; set_components
      ]
    ()
