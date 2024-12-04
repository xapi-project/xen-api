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

let lifecycle = [(Lifecycle.Published, "1.301.0", "")]

let origin =
  Enum
    ( "origin"
    , [
        ("remote", "The origin of the repository is a remote one")
      ; ("bundle", "The origin of the repository is a local bundle file")
      ; ("remote_pool", "The origin of the repository is a remote pool")
      ]
    )

let introduce =
  call ~name:"introduce" ~in_oss_since:None
    ~lifecycle:[(Published, "1.301.0", "")]
    ~doc:"Add the configuration for a new remote repository"
    ~versioned_params:
      [
        {
          param_type= String
        ; param_name= "name_label"
        ; param_doc= "The name of the repository"
        ; param_release= numbered_release "1.301.0"
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "name_description"
        ; param_doc= "The description of the repository"
        ; param_release= numbered_release "1.301.0"
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "binary_url"
        ; param_doc= "Base URL of binary packages in this repository"
        ; param_release= numbered_release "1.301.0"
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "source_url"
        ; param_doc= "Base URL of source packages in this repository"
        ; param_release= numbered_release "1.301.0"
        ; param_default= None
        }
      ; {
          param_type= Bool
        ; param_name= "update"
        ; param_doc=
            "True if the repository is an update repository. This means that \
             updateinfo.xml will be parsed"
        ; param_release= numbered_release "1.301.0"
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "gpgkey_path"
        ; param_doc= "The GPG public key file name"
        ; param_release= numbered_release "1.301.0"
        ; param_default= Some (VString "")
        }
      ]
    ~result:(Ref _repository, "The ref of the created repository record.")
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let introduce_bundle =
  call ~name:"introduce_bundle" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Add the configuration for a new bundle repository"
    ~params:
      [
        (String, "name_label", "The name of the repository")
      ; (String, "name_description", "The description of the repository")
      ]
    ~result:(Ref _repository, "The ref of the created repository record.")
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let introduce_remote_pool =
  call ~name:"introduce_remote_pool" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Add the configuration for a new remote pool repository"
    ~params:
      [
        (String, "name_label", "The name of the repository")
      ; (String, "name_description", "The description of the repository")
      ; ( String
        , "binary_url"
        , "Base URL of binary packages in the local repository of this remote \
           pool in https://<coordinator-ip>/repository format"
        )
      ; ( String
        , "certificate"
        , "The host certificate of the coordinator of the remote pool"
        )
      ]
    ~result:(Ref _repository, "The ref of the created repository record.")
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let forget =
  call ~name:"forget" ~in_oss_since:None
    ~lifecycle:[(Published, "1.301.0", "")]
    ~doc:"Remove the repository record from the database"
    ~params:
      [
        ( Ref _repository
        , "self"
        , "The repository to be removed from the database"
        )
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let apply =
  call ~name:"apply" ~in_oss_since:None
    ~lifecycle:[(Published, "1.301.0", "")]
    ~doc:"Apply updates on a host"
    ~params:[(Ref _host, "host", "The host to be updated")]
    ~pool_internal:true ~hide_from_docs:true
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let apply_livepatch =
  call ~name:"apply_livepatch" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Apply a livepatch on a host"
    ~params:
      [
        (Ref _host, "host", "The host to be live-patched")
      ; (String, "component", "The name of the component to be live-patched")
      ; ( String
        , "base_build_id"
        , "The build ID of the component to be live-patched"
        )
      ; ( String
        , "base_version"
        , "The RPM version of the component to be live-patched"
        )
      ; ( String
        , "base_release"
        , "The RPM release of the component to be live-patched"
        )
      ; ( String
        , "to_version"
        , "The RPM version of the component after applying the livepatch"
        )
      ; ( String
        , "to_release"
        , "The RPM release of the component after applying the livepatch"
        )
      ]
    ~pool_internal:true ~hide_from_docs:true
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let set_gpgkey_path =
  call ~name:"set_gpgkey_path" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Set the file name of the GPG public key of the repository"
    ~params:
      [
        (Ref _repository, "self", "The repository")
      ; ( String
        , "value"
        , "The file name of the GPG public key of the repository"
        )
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let t =
  create_obj ~name:_repository ~descr:"Repository for updates" ~doccomments:[]
    ~gen_constructor_destructor:false ~gen_events:true ~in_db:true
    ~lifecycle:[(Published, "1.301.0", "")]
    ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ~messages:
      [
        introduce
      ; introduce_bundle
      ; introduce_remote_pool
      ; forget
      ; apply
      ; set_gpgkey_path
      ; apply_livepatch
      ]
    ~contents:
      [
        uid _repository ~lifecycle:[(Published, "1.301.0", "")]
      ; namespace ~name:"name"
          ~contents:
            (names
               ~writer_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
               ~lifecycle:[(Published, rel_rio, "")]
               None RW
            )
          ()
      ; field ~qualifier:StaticRO
          ~lifecycle:[(Published, "1.301.0", "")]
          ~ty:String ~default_value:(Some (VString "")) "binary_url"
          "Base URL of binary packages in this repository"
      ; field ~qualifier:StaticRO
          ~lifecycle:[(Published, "1.301.0", "")]
          ~ty:String ~default_value:(Some (VString "")) "source_url"
          "Base URL of source packages in this repository"
      ; field ~qualifier:StaticRO
          ~lifecycle:[(Published, "1.301.0", "")]
          ~ty:Bool ~default_value:(Some (VBool false)) "update"
          "True if updateinfo.xml in this repository needs to be parsed"
      ; field ~qualifier:DynamicRO
          ~lifecycle:[(Published, "1.301.0", "")]
          ~ty:String ~default_value:(Some (VString "")) "hash"
          "SHA256 checksum of latest updateinfo.xml.gz in this repository if \
           its 'update' is true"
      ; field ~qualifier:DynamicRO
          ~lifecycle:
            [
              (Published, "1.301.0", "")
            ; (Deprecated, "23.18.0", "Dummy transition")
            ; ( Removed
              , "23.18.0"
              , "The up_to_date field of repository was removed"
              )
            ]
          ~ty:Bool ~default_value:(Some (VBool false)) "up_to_date"
          "True if all hosts in pool is up to date with this repository"
      ; field ~qualifier:StaticRO ~lifecycle:[] ~ty:String
          ~default_value:(Some (VString "")) "gpgkey_path"
          "The file name of the GPG public key of this repository"
      ; field ~qualifier:StaticRO ~lifecycle:[] ~ty:origin "origin"
          ~default_value:(Some (VEnum "remote"))
          "The origin of this repository. 'remote' if the origin of the \
           repository is a remote one, 'bundle' if the origin of the \
           repository is a local bundle file, 'remote_pool' if the origin of \
           the repository is a remote pool"
      ; field ~qualifier:StaticRO ~lifecycle:[] ~ty:String
          ~default_value:(Some (VString "")) "certificate"
          "The certificate of the host which hosts this repository"
      ]
    ()
