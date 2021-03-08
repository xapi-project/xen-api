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
    ~lifecycle
    ~doc:"Add the configuration for a new repository"
    ~params:[
      String, "name_label", "The name of the repository";
      String, "name_description", "The description of the repository";
      String, "binary_url", "Base URL of binary packages in this repository";
      String, "source_url", "Base URL of source packages in this repository";
      Bool, "update", "True if the repository is an update repository. This means that updateinfo.xml will be parsed";
    ]
    ~result:(Ref _repository, "The ref of the created repository record.")
    ~allowed_roles:_R_POOL_OP
    ()

let forget = call
    ~name:"forget"
    ~in_oss_since:None
    ~lifecycle
    ~doc:"Remove the repository record from the database"
    ~params:[Ref _repository, "self", "The repository to be removed from the database"]
    ~allowed_roles:_R_POOL_OP
    ()

let apply = call
    ~name:"apply"
    ~in_oss_since:None
    ~lifecycle
    ~doc:"Apply updates on a host"
    ~params:[Ref _host, "host", "The host to be updated"]
    ~pool_internal:true
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_ADMIN
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
    ~messages: [introduce; forget; apply]

    ~contents:
      [ uid       _repository ~lifecycle;
        namespace ~name:"name" ~contents:(names ~writer_roles:_R_POOL_OP None RW) ();
        field     ~qualifier:StaticRO ~lifecycle
          ~ty:String
          ~default_value:(Some (VString ""))
          "binary_url"
          "Base URL of binary packages in this repository";
        field     ~qualifier:StaticRO ~lifecycle
          ~ty:String
          ~default_value:(Some (VString ""))
          "source_url"
          "Base URL of source packages in this repository";
        field     ~qualifier:StaticRO ~lifecycle
           ~ty:Bool
           ~default_value:(Some (VBool false))
           "update"
           "True if updateinfo.xml in this repository needs to be parsed";
        field     ~qualifier:DynamicRO ~lifecycle
          ~ty:String
          ~default_value:(Some (VString ""))
          "hash"
          "SHA256 checksum of latest updateinfo.xml.gz in this repository if its 'update' is true";
        field     ~qualifier:DynamicRO ~lifecycle
          ~ty:Bool
          ~default_value:(Some (VBool false))
          "up_to_date"
          "True if all hosts in pool is up to date with this repository";
      ]
    ()
