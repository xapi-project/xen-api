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

let set_status =
  call ~name:"set_status" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the fields of a TracerProvider in the Tracing Library"
    ~params:
      [
        (Ref _tracing, "self", "The TracerProvider")
      ; (Bool, "status", "The status the TracerProvider is to be set to")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_tags =
  call ~name:"set_tags" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the fields of a TracerProvider in the Tracing Library"
    ~params:
      [
        (Ref _tracing, "self", "The TracerProvider")
      ; ( Map (String, String)
        , "tags"
        , "The tags the TracerProvider is to be set to"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_endpoints =
  call ~name:"set_endpoints" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the fields of a TracerProvider in the Tracing Library"
    ~params:
      [
        (Ref _tracing, "self", "The TracerProvider")
      ; ( Set String
        , "endpoints"
        , "The endpoints the TracerProvider is to be set to"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_components =
  call ~name:"set_components" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the fields of a TracerProvider in the Tracing Library"
    ~params:
      [
        (Ref _tracing, "self", "The TracerProvider")
      ; ( Set String
        , "components"
        , "The components the TracerProvider is to be set to"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_filters =
  call ~name:"set_filters" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the fields of a TracerProvider in the Tracing Library"
    ~params:
      [
        (Ref _tracing, "self", "The TracerProvider")
      ; (Set String, "filters", "The filter the TracerProvider is to be set to")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_processors =
  call ~name:"set_processors" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the fields of a TracerProvider in the Tracing Library"
    ~params:
      [
        (Ref _tracing, "self", "The TracerProvider")
      ; ( Set String
        , "processors"
        , "The processors the TracerProvider is to be set to"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let t =
  create_obj ~name:_tracing ~descr:"Describes a TracerProvider" ~doccomments:[]
    ~gen_constructor_destructor:false ~gen_events:true ~in_db:true ~lifecycle:[]
    ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      ([uid _tracing ~lifecycle:[]]
      @ [
          field ~qualifier:DynamicRO ~ty:(Set (Ref _host)) ~lifecycle:[] "hosts"
            "A list of hosts the TracerProvider is to be registered on"
            ~ignore_foreign_key:true
        ; field ~qualifier:DynamicRO ~ty:String ~lifecycle:[] "name_label"
            "A user-assinged label for a TracerProvider"
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[] "tags"
            "Tags to be set on spans to provide extra information"
        ; field ~qualifier:DynamicRO ~ty:(Set String) ~lifecycle:[] "endpoints"
            "Endpoints where spans are exported to"
        ; field ~qualifier:DynamicRO ~ty:(Set String) ~lifecycle:[] "components"
            "Componenets the TraverProvider is to be registered on"
        ; field ~qualifier:DynamicRO ~ty:(Set String) ~lifecycle:[] "filters"
            "Filters for operations that spans will be produced for"
        ; field ~qualifier:DynamicRO ~ty:(Set String) ~lifecycle:[] "processors"
            "Processors that will filter/map spans before being exported"
        ; field ~qualifier:DynamicRO ~ty:Bool ~lifecycle:[] "status"
            "The status of the TracerProvider"
        ]
      )
    ~messages:
      [
        set_status
      ; set_tags
      ; set_endpoints
      ; set_components
      ; set_filters
      ; set_processors
      ]
    ()
