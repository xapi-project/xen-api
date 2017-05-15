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

open OUnit
open Test_highlevel

module PbisAuthErrorsCatch= Generic.Make(struct
    module Io = struct
      type input_t = string list
      type output_t = Auth_signature.auth_service_error_tag

      let string_of_input_t = Test_printers.(list string)
      let string_of_output_t output = 
        match output with
        | Auth_signature.E_GENERIC           -> "E_GENERIC"
        | Auth_signature.E_LOOKUP            -> "E_LOOKUP"
        | Auth_signature.E_DENIED            -> "E_DENIED"
        | Auth_signature.E_CREDENTIALS       -> "E_CREDENTIALS"
        | Auth_signature.E_UNAVAILABLE       -> "E_UNAVAILABLE"
        | Auth_signature.E_INVALID_OU        -> "E_INVALID_OU"
        | Auth_signature.E_INVALID_ACCOUNT   -> "E_INVALID_ACCOUNT"

    end

    let transform = Extauth_plugin_ADpbis.match_error_tag

    let tests = [
      [], Auth_signature.E_GENERIC;
      [""; ""], Auth_signature.E_GENERIC;
      [""; "some words"], Auth_signature.E_GENERIC;
      [""; "DNS_ERROR_BAD_PACKET"],       Auth_signature.E_LOOKUP;
      [""; "LW_ERROR_PASSWORD_MISMATCH"], Auth_signature.E_CREDENTIALS;
      [""; "LW_ERROR_INVALID_ACCOUNT"],   Auth_signature.E_INVALID_ACCOUNT;
      [""; "LW_ERROR_ACCESS_DENIED"],     Auth_signature.E_DENIED;
      [""; "LW_ERROR_DOMAIN_IS_OFFLINE"], Auth_signature.E_UNAVAILABLE;
      [""; "LW_ERROR_INVALID_OU"],        Auth_signature.E_INVALID_OU;

      [""; "prefixDNS_ERROR_BAD_PACKETsuffix"],   Auth_signature.E_GENERIC;
      [""; "prefix_DNS_ERROR_BAD_PACKET_suffix"], Auth_signature.E_GENERIC;
      [""; "prefix(DNS_ERROR_BAD_PACKET)suffix"], Auth_signature.E_LOOKUP;
      [""; "prefix.DNS_ERROR_BAD_PACKET.suffix"], Auth_signature.E_LOOKUP;
      [""; "prefix DNS_ERROR_BAD_PACKET suffix"], Auth_signature.E_LOOKUP;
      [""; "prefix\tDNS_ERROR_BAD_PACKET\tsuffix"], Auth_signature.E_LOOKUP;
    ]
  end)

let test =
  "test_extauth_ADpbis" >:::
  [
    "test_pbis_auth_errors_catch" >::: PbisAuthErrorsCatch.tests;
  ]

