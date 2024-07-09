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

let lifecycle = [(Lifecycle.Published, rel_stockholm, "")]

let certificate_type =
  Enum
    ( "certificate_type"
    , [
        ("ca", "Certificate that is trusted by the whole pool")
      ; ( "host"
        , "Certificate that identifies a single host to entities outside the \
           pool"
        )
      ; ( "host_internal"
        , "Certificate that identifies a single host to other pool members"
        )
      ]
    )

let t =
  create_obj ~name:_certificate
    ~descr:"An X509 certificate used for TLS connections" ~doccomments:[]
    ~gen_constructor_destructor:false ~gen_events:true ~in_db:true ~lifecycle
    ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_READ_ONLY
    ~contents:
      [
        uid _certificate ~lifecycle
      ; field ~qualifier:DynamicRO
          ~lifecycle:[(Published, "1.294.0", "")]
            (* Any cert records existing before the introduction of the 'name' field
             * have no name, because they are of type 'host' *)
          ~ty:String "name" ~default_value:(Some (VString ""))
          "The name of the certificate, only present on certificates of type \
           'ca'"
      ; field ~qualifier:DynamicRO
          ~lifecycle:[(Published, "1.294.0", "")]
            (* Any cert records existing before the introduction of the 'host' field
             * are of type host *)
          ~ty:certificate_type "type" ~default_value:(Some (VEnum "host"))
          "The type of the certificate, either 'ca', 'host' or 'host_internal'"
      ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _host) "host"
          ~default_value:(Some (VRef null_ref))
          "The host where the certificate is installed"
      ; field ~qualifier:StaticRO ~lifecycle ~ty:DateTime "not_before"
          ~default_value:(Some (VDateTime Date.never))
          "Date after which the certificate is valid"
      ; field ~qualifier:StaticRO ~lifecycle ~ty:DateTime "not_after"
          ~default_value:(Some (VDateTime Date.never))
          "Date before which the certificate is valid"
      ; field ~qualifier:StaticRO
          ~lifecycle:
            [(Published, rel_stockholm, ""); (Deprecated, "24.19.0", "")]
          ~ty:String "fingerprint" ~default_value:(Some (VString ""))
          "Use fingerprint_sha256 instead"
      ; field ~qualifier:StaticRO ~lifecycle:[] ~ty:String "fingerprint_sha256"
          ~default_value:(Some (VString ""))
          "The certificate's SHA256 fingerprint / hash"
      ; field ~qualifier:StaticRO ~lifecycle:[] ~ty:String "fingerprint_sha1"
          ~default_value:(Some (VString ""))
          "The certificate's SHA1 fingerprint / hash"
      ]
    ~messages:[] ()
