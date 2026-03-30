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

open Tls_policy

(* ---- GnuTLS tests ------------------------------------------------------- *)

let test_gnutls_default_policy () =
  let expected =
    "NONE:+VERS-TLS1.2:+AES-256-GCM:+AES-128-GCM:+AEAD:+ECDHE-RSA:+SIGN-ALL:+GROUP-SECP384R1:+COMP-NULL:%SERVER_PRECEDENCE"
  in
  Alcotest.(check string)
    "default GnuTLS policy" expected (Gnutls.default_policy ())

let test_gnutls_default_ciphers () =
  Alcotest.(check string)
    "default GnuTLS ciphers" "+AES-256-GCM:+AES-128-GCM" Gnutls.default_ciphers

let test_gnutls_default_version () =
  Alcotest.(check string)
    "default GnuTLS version" "+VERS-TLS1.2" Gnutls.default_version

let test_gnutls_default_curve () =
  Alcotest.(check string)
    "default GnuTLS curve" "+GROUP-SECP384R1" Gnutls.default_curve

let test_gnutls_default_server_preference () =
  Alcotest.(check string)
    "default GnuTLS server preference" "%SERVER_PRECEDENCE"
    Gnutls.default_server_preference

let test_gnutls_string_of_versions () =
  Alcotest.(check string)
    "GnuTLS string_of_versions TLS1.2+TLS1.3" "+VERS-TLS1.2:+VERS-TLS1.3"
    (Gnutls.string_of_versions [TLS_1_2; TLS_1_3])

let test_gnutls_string_of_ciphers () =
  Alcotest.(check string)
    "GnuTLS string_of_ciphers AES128+AES256" "+AES-128-GCM:+AES-256-GCM"
    (Gnutls.string_of_ciphers [AES_128_GCM; AES_256_GCM])

let test_gnutls_string_of_curves () =
  Alcotest.(check string)
    "GnuTLS string_of_curves secp384r1" "+GROUP-SECP384R1"
    (Gnutls.string_of_curves [Secp384r1])

let test_gnutls_policy_no_server_pref () =
  let policy =
    {
      versions= [TLS_1_2]
    ; ciphers= [AES_256_GCM]
    ; curves= [Secp384r1]
    ; kex= [ECDHE_RSA]
    ; server_preference= false
    }
  in
  let expected =
    "NONE:+VERS-TLS1.2:+AES-256-GCM:+AEAD:+ECDHE-RSA:+SIGN-ALL:+GROUP-SECP384R1:+COMP-NULL"
  in
  Alcotest.(check string)
    "GnuTLS policy without server preference" expected
    (Gnutls.string_of_policy policy)

let test_gnutls_policy_tls13 () =
  let policy =
    {
      versions= [TLS_1_2; TLS_1_3]
    ; ciphers= [AES_256_GCM; AES_128_GCM]
    ; curves= [Secp384r1]
    ; kex= [ECDHE_RSA]
    ; server_preference= true
    }
  in
  let expected =
    "NONE:+VERS-TLS1.2:+VERS-TLS1.3:+AES-256-GCM:+AES-128-GCM:+AEAD:+ECDHE-RSA:+SIGN-ALL:+GROUP-SECP384R1:+COMP-NULL:%SERVER_PRECEDENCE"
  in
  Alcotest.(check string)
    "GnuTLS policy with TLS 1.3" expected
    (Gnutls.string_of_policy policy)

(* ---- OpenSSL tests ------------------------------------------------------ *)

let test_openssl_default_ciphers () =
  Alcotest.(check string)
    "default OpenSSL ciphers"
    "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES128-GCM-SHA256"
    Openssl.default_ciphers

let test_openssl_default_version () =
  Alcotest.(check string)
    "default OpenSSL version" "TLSv1.2" Openssl.default_version

let test_openssl_default_curve () =
  Alcotest.(check string)
    "default OpenSSL curve" "secp384r1" Openssl.default_curve

let test_openssl_default_server_preference () =
  Alcotest.(check string)
    "default OpenSSL server preference" "CIPHER_SERVER_PREFERENCE"
    Openssl.default_server_preference

let test_openssl_string_of_ciphers () =
  Alcotest.(check string)
    "OpenSSL string_of_ciphers AES128 only" "ECDHE-RSA-AES128-GCM-SHA256"
    (Openssl.string_of_ciphers [AES_128_GCM])

let test_openssl_string_of_curves () =
  Alcotest.(check string)
    "OpenSSL string_of_curves secp384r1" "secp384r1"
    (Openssl.string_of_curves [Secp384r1])

let test_openssl_string_of_policy_raises () =
  Alcotest.check_raises "OpenSSL string_of_policy raises"
    (Failure "Not supported") (fun () ->
      ignore
        (Openssl.string_of_policy
           {
             versions= [TLS_1_2]
           ; ciphers= [AES_256_GCM]
           ; curves= [Secp384r1]
           ; kex= [ECDHE_RSA]
           ; server_preference= true
           }
        )
  )

(* ---- Test suite --------------------------------------------------------- *)

let gnutls_tests =
  [
    ("default_policy", `Quick, test_gnutls_default_policy)
  ; ("default_ciphers", `Quick, test_gnutls_default_ciphers)
  ; ("default_version", `Quick, test_gnutls_default_version)
  ; ("default_curve", `Quick, test_gnutls_default_curve)
  ; ("default_server_preference", `Quick, test_gnutls_default_server_preference)
  ; ("string_of_versions", `Quick, test_gnutls_string_of_versions)
  ; ("string_of_ciphers", `Quick, test_gnutls_string_of_ciphers)
  ; ("string_of_curves", `Quick, test_gnutls_string_of_curves)
  ; ("policy_no_server_preference", `Quick, test_gnutls_policy_no_server_pref)
  ; ("policy_tls13", `Quick, test_gnutls_policy_tls13)
  ]

let openssl_tests =
  [
    ("default_ciphers", `Quick, test_openssl_default_ciphers)
  ; ("default_version", `Quick, test_openssl_default_version)
  ; ("default_curve", `Quick, test_openssl_default_curve)
  ; ("default_server_preference", `Quick, test_openssl_default_server_preference)
  ; ("string_of_ciphers", `Quick, test_openssl_string_of_ciphers)
  ; ("string_of_curves", `Quick, test_openssl_string_of_curves)
  ; ("string_of_policy_raises", `Quick, test_openssl_string_of_policy_raises)
  ]

let tests = [("Gnutls", gnutls_tests); ("Openssl", openssl_tests)]

let () = Alcotest.run "Tls_policy" tests
