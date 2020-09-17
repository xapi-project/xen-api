(*
 * Copyright (C) 2020 Citrix Systems Inc.
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

let lifecycle = [Published, rel_stockholm, ""]

let certificate_type =
  Enum ( "certificate_type"
       , [ "host", "Certificate that identifies a single host"
         ; "pool", "Certificate that is trusted by the whole pool"
         ; "host_and_pool", "Certificate that both identifies a host and is trusted by the whole pool."
         ])

let t =
  create_obj
    ~name: _certificate
    ~descr:"Description"
    ~doccomments:[]
    ~gen_constructor_destructor:false
    ~gen_events:true
    ~in_db:true
    ~lifecycle
    ~persist:PersistEverything
    ~in_oss_since:None
    ~messages_default_allowed_roles:_R_READ_ONLY
    ~contents:
      [ uid   _certificate ~lifecycle
      ; field ~qualifier:DynamicRO ~lifecycle:[Published, rel_next, ""]
          ~ty:certificate_type "type"
          "The type of certificate, can be a host certificate, a CA certificate, or both"
      ; field ~qualifier:StaticRO ~lifecycle
          ~ty:(Ref _host) "host" ~default_value:(Some (VRef null_ref))
          "The host that is identified by the certificate"
      ; field ~qualifier:StaticRO ~lifecycle:[Published, rel_next, ""]
          ~ty:(Ref _pool) "pool" ~default_value:(Some (VRef null_ref))
          "The pool that trusts this certificate"
      ; field ~qualifier:StaticRO ~lifecycle
          ~ty:(DateTime) "not_before" ~default_value:(Some (VDateTime Date.never))
          "Date after which the certificate is valid"
      ; field ~qualifier:StaticRO ~lifecycle
          ~ty:(DateTime) "not_after" ~default_value:(Some (VDateTime Date.never))
          "Date before which the certificate is valid"
      ; field ~qualifier:StaticRO ~lifecycle
          ~ty:(String) "fingerprint" ~default_value:(Some (VString ""))
          "The certificate's fingerprint / hash"
      ; field ~qualifier:DynamicRO ~lifecycle:[Published, rel_next, ""]
      ~ty:(Set (Ref _certificate)) "validated_by"
          "The trusted certificates that validate this host certificate"
      ; field ~qualifier:DynamicRO ~lifecycle:[Published, rel_next, ""]
          ~ty:(Set (Ref _certificate)) "validates"
          "The certificates that are validates by this pool certificate"
      ]
    ~messages:
      [
      ]
    ()
