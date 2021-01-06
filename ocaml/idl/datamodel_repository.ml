(*
 * Copyright (C) Citrix Systems Inc.
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

let lifecycle = [Published, rel_next, ""]

let introduce = call
    ~name:"introduce"
    ~in_oss_since:None
    ~in_product_since:rel_next
    ~doc:"Create a new repository record in the database only"
    ~params:[
      String, "name_label", "The name of the repository";
      String, "binary_url", "Base URL of binary packages in this repository";
      String, "source_url", "Base URL of source pacakges in this repository";
    ]
    ~result:(Ref _repository, "The ref of the created repository record.")
    ~allowed_roles:_R_POOL_OP
    ()

let forget = call
    ~name:"forget"
    ~in_oss_since:None
    ~in_product_since:rel_next
    ~doc:"Remove a repository record from the database"
    ~params:[Ref _repository, "self", "The repository to forget about"]
    ~allowed_roles:_R_POOL_OP
    ()

let t =
  create_obj
    ~name: _repository
    ~descr:"Repository for updates"
    ~doccomments:[]
    ~gen_constructor_destructor:false
    ~gen_events:true
    ~in_db:true
    ~lifecycle
    ~persist:PersistEverything
    ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages: [introduce; forget]

    ~contents:
      [ uid       _repository ~lifecycle;
        namespace ~name:"name" ~contents:(names None StaticRO) ();
        field     ~qualifier:StaticRO ~lifecycle
          ~ty:String
          ~default_value:(Some (VString ""))
          "binary_url"
          "Base URL of binary packages in this repository";
        field     ~qualifier:StaticRO ~lifecycle
          ~ty:String
          ~default_value:(Some (VString ""))
          "source_url"
          "Base URL of source pacakges in this repository";
        field     ~qualifier:DynamicRO ~lifecycle
          ~ty:String
          ~default_value:(Some (VString ""))
          "hash"
          "SHA256 checksum of latest updateinfo.xml.gz in this repository";
        field     ~qualifier:DynamicRO ~lifecycle
          ~ty:Bool
          ~default_value:(Some (VBool false))
          "up_to_date"
          "True if all hosts in pool is up to date with this repository";
      ]
    ()
