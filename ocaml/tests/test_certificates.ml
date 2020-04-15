(** This module tests xapi-related PKI code
 *)

let hashables =
  [ "foobar",
    "C3:AB:8F:F1:37:20:E8:AD:90:47:DD:39:46:6B:3C:89:74:E5:92:C2:FA:38:3D:4A:39:60:71:4C:AE:F0:C4:F2"
  ]

let pp_hash_test =
  List.map (fun (hashable, expected) ->
    let test_hash () =
      let digest = Cstruct.of_string hashable
      |> Mirage_crypto.Hash.digest `SHA256
      in
      Alcotest.(check string) "fingerprints must match" expected (Certificates.pp_hash digest)
    in
    Printf.sprintf {|Validation of hash printing of "%s"|} hashable,
    `Quick, test_hash)
  hashables

let test = pp_hash_test
