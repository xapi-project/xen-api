(** This module tests the PKI validation
 *)
open Certificates
open Api_errors

open Rresult.R.Infix

let load_test_data file = Stdext.Unixext.string_of_file @@ "test_data/certificates/" ^ file ^ ".pem"

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

let server_error err reason =
  Server_error (err, reason)

let test_valid_key key_name () =
  let _priv = validate_private_key (load_test_data key_name) in
  ()

let test_invalid_key key_name error reason () =
  let key = load_test_data key_name in
  Alcotest.check_raises ""
    (server_error error reason)
    (fun () -> let _priv = validate_private_key key in ())

let valid_keys_tests =
  List.map (fun name ->
    "Validation of a supported key: " ^ name, `Quick, Ok (test_valid_key name))
  valid_private_keys

let invalid_keys_tests =
  List.map (fun (name, error, reason) ->
    "Validation of an unsupported key: " ^ name,
    `Quick, Ok (test_invalid_key name error reason))
  invalid_private_keys

let load_test = function
  | name, speed, Ok test ->
      name, speed, test
  | name, speed, Error (`Msg msg) ->
      name, speed, Alcotest.fail
        (Printf.sprintf "Problem preparing test '%s': %s" name msg)

let all_tests = valid_keys_tests @ invalid_keys_tests

let test = List.map load_test all_tests
