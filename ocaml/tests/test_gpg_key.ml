(*
 * Copyright (C) 2022 Citrix Systems Inc.
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

open Test_highlevel
open Gpg
open Rpm_gpg_key

module AssertNameIsValid = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (unit, exn) result

    let string_of_input_t s = s

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))
  end

  let transform input =
    try Ok (assert_name_is_valid ~name:input) with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ("", Error Api_errors.(Server_error (gpg_key_name_is_empty, [])))
      ; (" ", Error Api_errors.(Server_error (gpg_key_name_is_invalid, [" "])))
      ; (".", Error Api_errors.(Server_error (gpg_key_name_is_invalid, ["."])))
      ; ( ".abc"
        , Error Api_errors.(Server_error (gpg_key_name_is_invalid, [".abc"]))
        )
      ; ( "a/b/c"
        , Error Api_errors.(Server_error (gpg_key_name_is_invalid, ["a/b/c"]))
        )
      ; ( "0123456789-0123456789-0123456789-0123456789-0123456789-0123456789-0123456789"
        , Error
            Api_errors.(
              Server_error
                ( gpg_key_name_is_too_long
                , [
                    "0123456789-0123456789-0123456789-0123456789-0123456789-0123456789-0123456789"
                  ]
                )
            )
        )
      ; ("a.b", Ok ())
      ; ("a..b", Ok ())
      ; ("a_b", Ok ())
      ; ("a-b", Ok ())
      ]
end)

let fields_of_pubkey_metadata =
  Fmt.Dump.
    [
      field "created"
        (fun (r : PubKeyMetaData.t) -> Float.to_string r.PubKeyMetaData.created)
        string
    ; field "fingerprint"
        (fun (r : PubKeyMetaData.t) -> r.PubKeyMetaData.fingerprint)
        string
    ]
  

module ParsePubkeyMetaData = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (PubKeyMetaData.t, exn) result

    let string_of_input_t s = s

    let string_of_output_t =
      Fmt.(
        str "%a"
          Dump.(result ~ok:(record @@ fields_of_pubkey_metadata) ~error:exn)
      )
  end

  let invalid_md_0 =
    "pub:-:2048:1:D0F25A3CB6792C39:1405418461:1878458461::-:CentOS-7 Debug \
     (CentOS-7 Debuginfo RPMS) <security@centos.org>"

  let invalid_md_1 =
    {|pub:-:2048:1:D0F25A3CB6792C39:a405418461:1878458461::-:CentOS-7 Debug (CentOS-7 Debuginfo RPMS) <security@centos.org>:
fpr:::::::::759D690F60992D526A358CBDD0F25A3CB6792C39:
|}

  let invalid_md_2 =
    {|pub:-:2048:1:D0F25A3CB6792C39:a405418461:1878458461::-:CentOS-7 Debug (CentOS-7 Debuginfo RPMS) <security@centos.org>:
fpr:::::::::759D690F60992D526A358CBDD0F25A3CB6792C3a:
|}

  let valid_md =
    {|pub:-:2048:1:D0F25A3CB6792C39:1405418461:1878458461::-:CentOS-7 Debug (CentOS-7 Debuginfo RPMS) <security@centos.org>:
fpr:::::::::759D690F60992D526A358CBDD0F25A3CB6792C39:
|}

  let transform input = try Ok (parse_pubkey_metadata input) with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ("", Error Api_errors.(Server_error (gpg_key_is_invalid, [])))
      ; (invalid_md_0, Error Api_errors.(Server_error (gpg_key_is_invalid, [])))
      ; (invalid_md_1, Error Api_errors.(Server_error (gpg_key_is_invalid, [])))
      ; (invalid_md_2, Error Api_errors.(Server_error (gpg_key_is_invalid, [])))
      ; ( valid_md
        , Ok
            PubKeyMetaData.
              {
                created= 1405418461.0
              ; fingerprint= "759D690F60992D526A358CBDD0F25A3CB6792C39"
              }
            
        )
      ]
end)

module ExtractPubkeyFromRpm = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = string option

    let string_of_input_t s = s

    let string_of_output_t = Fmt.(str "%a" Dump.(option string))
  end

  let transform input = extract_pubkey_from_rpm ~name:"gpg-pubkey-name" input

  let gpg_pubkey =
    {|Name        : gpg-pubkey
Version     : b6792c39
Release     : 53c4fbdd
Architecture: (none)
Install Date: Mon Oct 17 02:40:11 2022
Group       : Public Keys
Size        : 0
License     : pubkey
Signature   : (none)
Source RPM  : (none)
Build Date  : Tue Jul 15 10:01:01 2014
Build Host  : localhost
Relocations : (not relocatable)
Packager    : CentOS-7 Debug (CentOS-7 Debuginfo RPMS) <security@centos.org>
Summary     : gpg(CentOS-7 Debug (CentOS-7 Debuginfo RPMS) <security@centos.org>)
Description :
-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: rpm-4.11.3 (NSS-3)

mQENBFPE+90BCAChR7lmZuMhY3IXdMSwAFXwnJUMWJbwX1p2OR7XV/YLkKEZ+a0T
xnaEKwe9WDb7k0jUaMkeAu9ACtHKmnGrp5eCI7AJv5gF7GT1k615xyvCyGSCFTaw
VidWtXAK3RF1qKalNCj9sz8utjYVe8UXRcvHU20TlK/J1Z64Ths52UUI9kBWJN+i
QuruxEsMCyylmXeodUfQrFG+zifreCyQ0BYZEiH+6EtIzlPeCOjgjz2AWov19q/H
KXyUoiJpy40NWhEV8dazbpxrsiD5Tlc+bLjhpDTa7aPhhhUjJaSIEP3HBE01+0rz
nf00VrwQlsipRUYuv+8T5nlQ16l8IqknNLbDABEBAAG0PkNlbnRPUy03IERlYnVn
IChDZW50T1MtNyBEZWJ1Z2luZm8gUlBNUykgPHNlY3VyaXR5QGNlbnRvcy5vcmc+
iQE+BBMBAgAoBQJTxPvdAhsDBQkcMgSABgsJCAcDAgYVCAIJCgsEFgIDAQIeAQIX
gAAKCRDQ8lo8tnksOWRFB/9PzxN2l6KSLWHPJX8OD4B7npVoouM+mscK4oHjXSRI
y2u2O2tT2hmcQzan29dahk1hsnyLdQlNSo642rV4ykbOdW9iszCLrw4ZwuQmpm5c
qeHMUPntwz9MFvn9memh85kZuxdFnAHmaxmqNl+5LEPNmmWyfX7TjGyG1Oi2xd7o
yT7lKnVXyLdY8E62RKTuQ/BNnAFpmUVgLbeKVq6qggZYgPOLt7GpLfpCm63aDZoX
OzJM75RqCtiK0n1BSBdUiVQ8+2XXYqRejFOEZa5LrLNDLFfqYA0YgpN/ournTKWt
QLHycvaoJrXZzF2EQjLiU2hHx8vS3bTR0lZsv8tUBHXG
=2CYo
-----END PGP PUBLIC KEY BLOCK-----
|}

  let pubkey =
    {|-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: rpm-4.11.3 (NSS-3)

mQENBFPE+90BCAChR7lmZuMhY3IXdMSwAFXwnJUMWJbwX1p2OR7XV/YLkKEZ+a0T
xnaEKwe9WDb7k0jUaMkeAu9ACtHKmnGrp5eCI7AJv5gF7GT1k615xyvCyGSCFTaw
VidWtXAK3RF1qKalNCj9sz8utjYVe8UXRcvHU20TlK/J1Z64Ths52UUI9kBWJN+i
QuruxEsMCyylmXeodUfQrFG+zifreCyQ0BYZEiH+6EtIzlPeCOjgjz2AWov19q/H
KXyUoiJpy40NWhEV8dazbpxrsiD5Tlc+bLjhpDTa7aPhhhUjJaSIEP3HBE01+0rz
nf00VrwQlsipRUYuv+8T5nlQ16l8IqknNLbDABEBAAG0PkNlbnRPUy03IERlYnVn
IChDZW50T1MtNyBEZWJ1Z2luZm8gUlBNUykgPHNlY3VyaXR5QGNlbnRvcy5vcmc+
iQE+BBMBAgAoBQJTxPvdAhsDBQkcMgSABgsJCAcDAgYVCAIJCgsEFgIDAQIeAQIX
gAAKCRDQ8lo8tnksOWRFB/9PzxN2l6KSLWHPJX8OD4B7npVoouM+mscK4oHjXSRI
y2u2O2tT2hmcQzan29dahk1hsnyLdQlNSo642rV4ykbOdW9iszCLrw4ZwuQmpm5c
qeHMUPntwz9MFvn9memh85kZuxdFnAHmaxmqNl+5LEPNmmWyfX7TjGyG1Oi2xd7o
yT7lKnVXyLdY8E62RKTuQ/BNnAFpmUVgLbeKVq6qggZYgPOLt7GpLfpCm63aDZoX
OzJM75RqCtiK0n1BSBdUiVQ8+2XXYqRejFOEZa5LrLNDLFfqYA0YgpN/ournTKWt
QLHycvaoJrXZzF2EQjLiU2hHx8vS3bTR0lZsv8tUBHXG
=2CYo
-----END PGP PUBLIC KEY BLOCK-----|}

  let tests =
    `QuickAndAutoDocumented [("", None); (" ", None); (gpg_pubkey, Some pubkey)]
end)

let tests =
  make_suite "rpm_gpg_key_"
    [
      ("assert_name_is_valid", AssertNameIsValid.tests)
    ; ("parse_pubkey_metadata", ParsePubkeyMetaData.tests)
    ; ("extract_pubkey_from_rpm", ExtractPubkeyFromRpm.tests)
    ]

let () = Alcotest.run "RPM GPG Key" tests
