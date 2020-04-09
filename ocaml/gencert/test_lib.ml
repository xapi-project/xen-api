(** This module tests the PKI validation
 *)
open Gencertlib.Lib
open Api_errors

open Rresult.R.Infix

(* Initialize RNG for testing certificates *)
let () = Mirage_crypto_rng_unix.initialize ()

let time_of_rfc3339 date =
  match Ptime.of_rfc3339 date with
  | Ok (time, _, _) -> time
  | Error _ -> raise (Failure ("Date is in the wrong format: " ^ date))

let valid_from = time_of_rfc3339 "2020-01-01T00:00:00Z"

let valid_until = time_of_rfc3339 "2021-01-01T00:00:00Z"

let host_name =
  X509.Distinguished_name.Relative_distinguished_name.singleton (CN "localhost")

let load_test_data file =
  Xapi_stdext_unix.Unixext.string_of_file @@ "test_data/certificates/" ^ file ^ ".pem"

let valid_private_keys = ["pkey_rsa_2048"; "pkey_rsa_4096"]

(* ( file_name, error_type, error_message list ) *)
let invalid_private_keys =
  [ "pkey_rsa_1024", server_certificate_key_rsa_length_not_supported,
    [ "1024" ]
  ; "pkey_rsa_8192", server_certificate_key_rsa_length_not_supported,
    [ "8192" ]
  ; "pkey_rsa_n3_2048", server_certificate_key_rsa_multi_not_supported, []
  ; "pkey_ed25519", server_certificate_key_algorithm_not_supported,
    [ "1.3.101.112" ]
  ; "pkey_bogus", server_certificate_key_invalid, []
  ]

(* ( description, leaf_private_key, time_of_validation, signature_algorithm ) *)
let valid_leaf_certificates =
  [ "Valid, SHA256, matches key",
    "pkey_rsa_2048",
    "2020-02-01T00:00:00Z",
    `SHA256 ]

(* ( description, leaf_private_key, expected_private_key, time_of_validation,
     signature_algorithm, error_type, error_message list ) *)
let invalid_leaf_certificates =
  [ "Not valid yet, SHA256, matching key",
    "pkey_rsa_2048",
    "pkey_rsa_2048",
    "2019-01-01T00:00:00Z",
    `SHA256,
    server_certificate_not_valid_yet,
    [ "2019-01-01T00:00:00Z"; "2020-01-01T00:00:00Z" ]
  ; "Expired, SHA256, matching key",
    "pkey_rsa_2048",
    "pkey_rsa_2048",
    "2022-01-01T00:00:00Z",
    `SHA256,
    server_certificate_expired,
    [ "2022-01-01T00:00:00Z"; "2021-01-01T00:00:00Z" ]
  ; "Valid, SHA256, keys do not match",
    "pkey_rsa_2048",
    "pkey_rsa_4096",
    "2020-02-01T00:00:00Z",
    `SHA256,
    server_certificate_key_mismatch,
    []
  ; "Valid, SHA1, matching keys",
    "pkey_rsa_2048",
    "pkey_rsa_2048",
    "2020-02-01T00:00:00Z",
    `SHA1,
    server_certificate_signature_not_supported,
    []
  ]

(* ( certificate_name, leaf_private_key, time_of_validation, error_tye,
     error_message list ) *)
let corrupt_certificates =
  [ "cert_bogus",
    "pkey_rsa_2048",
    "2020-02-01T00:00:00Z",
    server_certificate_invalid,
    []
  ]

let key_chain = List.init 3 (fun _ -> `RSA (Mirage_crypto_pk.Rsa.generate ~bits:2048 ()))

(* ( certificate_name, leaf_private_key, time_of_validation, error_tye,
     error_message list ) *)
let corrupt_chain_certificates =
  [ "cert_bogus",
    "pkey_rsa_2048",
    "2020-02-01T00:00:00Z",
    server_certificate_chain_invalid,
    []
  ; "bogus",
    "pkey_rsa_2048",
    "2020-02-01T00:00:00Z",
    server_certificate_chain_invalid,
    []
  ]

let server_error err reason =
  Server_error (err, reason)

let test_valid_key key_name () =
  match validate_private_key (load_test_data key_name) with
  | Ok _ -> ()
  | Error (`Msg (_, msg)) ->
    Alcotest.fail @@ Format.asprintf
      "Valid key could not be validated: %a" Fmt.(Dump.list string) msg

let test_invalid_key key_name error reason () =
  match validate_private_key (load_test_data key_name) with
  | Ok _ ->
    Alcotest.fail "Invalid key was validated without errors"
  | Error (`Msg msg) ->
      Alcotest.(check @@ pair string @@ list string)
        "Error must match" (error, reason) msg

let valid_keys_tests =
  List.map (fun name ->
    "Validation of a supported key: " ^ name, `Quick, Ok (test_valid_key name))
  valid_private_keys

let invalid_keys_tests =
  List.map (fun (name, error, reason) ->
    "Validation of an unsupported key: " ^ name,
    `Quick, Ok (test_invalid_key name error reason))
  invalid_private_keys

let test_valid_cert ~kind cert time pkey =
  match validate_certificate kind cert time pkey with
  | Ok _ -> ()
  | Error (`Msg (_, msg)) ->
    Alcotest.fail @@ Format.asprintf
      "Valid certificate could not be validated: %a" Fmt.(Dump.list string) msg

let test_invalid_cert ~kind cert time pkey error reason =
  match validate_certificate kind cert time pkey with
  | Ok _ ->
    Alcotest.fail "Invalid certificate was validated without errors"
  | Error (`Msg msg) ->
      Alcotest.(check @@ pair string @@ list string)
        "Error must match" (error, reason) msg

let load_pkcs8 name =
  X509.Private_key.decode_pem (Cstruct.of_string (load_test_data name))
  |> Rresult.R.reword_error (fun (`Msg msg) ->
    `Msg (Printf.sprintf "Could not load private key with name '%s': %s" name msg))

let sign_cert host_name ~pkey_sign digest pkey_leaf =
  let csr = X509.Signing_request.create [host_name] ~digest pkey_leaf in
  X509.Signing_request.sign csr ~valid_from ~valid_until ~digest ~hash_whitelist:[digest] pkey_sign [host_name]

let sign_leaf_cert host_name digest pkey_leaf =
  load_pkcs8 "pkey_rsa_4096" >>= fun pkey_sign ->
  sign_cert host_name ~pkey_sign digest pkey_leaf
  >>| X509.Certificate.encode_pem
  >>| Cstruct.to_string

let valid_leaf_cert_tests =
  let test_valid_leaf_cert cert time pkey () =
    test_valid_cert ~kind:Leaf cert time pkey
  in

  List.map (fun (name, pkey_leaf_name, time, digest) ->
    let cert_test =
      load_pkcs8 pkey_leaf_name >>= fun pkey_leaf ->
      sign_leaf_cert host_name digest pkey_leaf >>| fun cert ->
      test_valid_leaf_cert cert (time_of_rfc3339 time) pkey_leaf
    in
    "Validation of a supported certificate: " ^ name, `Quick, cert_test)
  valid_leaf_certificates

let test_corrupt_leaf_cert (cert_name, pkey_name, time, error, reason) =
  let cert = load_test_data cert_name in
  let time = time_of_rfc3339 time in
  let test_cert = load_pkcs8 pkey_name >>| fun pkey ->
    let test () = test_invalid_cert ~kind:Leaf cert time pkey error reason
    in
    test
  in
  "Validation of a corrupted certificate", `Quick, test_cert

let test_invalid_leaf_cert (name, pkey_leaf_name, pkey_expected_name, time, digest, error, reason) =
  let test_invalid_leaf_cert cert time pkey error reason () =
    test_invalid_cert ~kind:Leaf cert time pkey error reason
  in

  let test_cert =
    load_pkcs8 pkey_leaf_name >>= fun pkey_leaf ->
    load_pkcs8 pkey_expected_name >>= fun pkey_expected ->
    sign_leaf_cert host_name digest pkey_leaf >>| fun cert ->
    test_invalid_leaf_cert cert (time_of_rfc3339 time) pkey_expected error reason
  in
  "Validation of an unsupported certificate: " ^ name, `Quick, test_cert

let invalid_leaf_cert_tests =
  List.map test_corrupt_leaf_cert corrupt_certificates @
  List.map test_invalid_leaf_cert invalid_leaf_certificates

let test_valid_cert_chain chain time pkey () =
  test_valid_cert ~kind:Chain chain time pkey

let test_invalid_cert_chain cert time pkey error reason () =
  test_invalid_cert ~kind:Chain cert time pkey error reason

let valid_chain_cert_tests =
  let time = time_of_rfc3339 "2020-02-01T00:00:00Z" in
  let test_cert =
    load_pkcs8 "pkey_rsa_4096" >>= fun pkey_root ->
    let pkey, chain = List.fold_left (fun (pkey_sign, chain_result) pkey ->
      let result = chain_result
        >>| fun chain -> sign_cert host_name ~pkey_sign `SHA256 pkey
        >>| fun cert -> cert :: chain
      in
      pkey, Rresult.R.join result
    ) (pkey_root, Ok []) key_chain in
    chain
    >>| X509.Certificate.encode_pem_multiple
    >>| Cstruct.to_string
    >>| fun chain -> test_valid_cert_chain chain time pkey
  in
  ["Validation of a supported certificate chain", `Quick, test_cert]

let invalid_chain_cert_tests =
  List.map (fun (chain_name, pkey_name, time, error, reason) ->
    let chain = load_test_data chain_name in
    let test_cert =
      load_pkcs8 pkey_name >>| fun pkey ->
      test_invalid_cert_chain chain (time_of_rfc3339 time) pkey error reason
    in
    "Validation of an unsupported certificate chain", `Quick, test_cert)
  corrupt_chain_certificates

let load_test = function
  | name, speed, Ok test ->
      name, speed, test
  | name, speed, Error (`Msg msg) ->
      name, speed, Alcotest.fail
        (Printf.sprintf "Problem preparing test '%s': %s" name msg)

let all_tests = valid_keys_tests @ invalid_keys_tests @
                valid_leaf_cert_tests @ invalid_leaf_cert_tests @
                valid_chain_cert_tests @ invalid_chain_cert_tests

let certificate_test = List.map load_test all_tests

let () =
  Alcotest.run "Gencert Library" ["Certificate tests", certificate_test]
