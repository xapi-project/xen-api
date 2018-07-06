open! Core
open Async
open Expect_test_helpers
open Protocol_version_header

module Legacy = struct
  type t = int list [@@deriving bin_io]
end

let test ~us:(p_us, v_us) ~peer:(p_peer, v_peer) =
  let us = create ~protocol:p_us ~supported_versions:v_us in
  let peer = create ~protocol:p_peer ~supported_versions:v_peer in
  let result = negotiate ~allow_legacy_peer:false ~us ~peer in
  print_s [%message "" ~_:(result: int Or_error.t)]
;;

let test_legacy ~allow_legacy_peer ~us:(p_us, v_us) ~peer =
  let us = create ~protocol:p_us ~supported_versions:v_us in
  let peer =
    let str = Binable.to_string (module Legacy) peer in
    Binable.of_string (module Protocol_version_header) str
  in
  let result = negotiate ~allow_legacy_peer ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)]
;;

let%expect_test _ =
  test ~us:(Krb, [1]) ~peer:(Krb, [1]);
  let%bind () =
    [%expect {| (Ok 1) |}]
  in
  test ~us:(Krb, [1]) ~peer:(Krb, [2]);
  let%bind () =
    [%expect {|
      (Error (
        "[Protocol_version_header.negotiate]: no shared version numbers"
        (us_versions   (1))
        (peer_versions (2))
        (protocol Krb)))
    |}]
  in
  test ~us:(Krb, [1]) ~peer:(Rpc, [1]);
  let%bind () =
    [%expect {|
      (Error (
        "[Protocol_version_header.negotiate]: conflicting magic protocol numbers"
        (us_protocol   Krb)
        (peer_protocol Rpc))) |}]
  in
  test_legacy ~allow_legacy_peer:false ~us:(Krb, [1; 2]) ~peer:[1];
  let%bind () =
    [%expect {|
      (Error (
        "[Protocol_version_header.negotiate]: conflicting magic protocol numbers"
        (us_protocol   Krb)
        (peer_protocol Unknown))) |}]
  in
  test_legacy ~allow_legacy_peer:true ~us:(Krb, [1; 2]) ~peer:[1];
  let%bind () =
    [%expect {| (Ok 1) |}]
  in
  test_legacy ~allow_legacy_peer:true ~us:(Krb, [2]) ~peer:[1];
  let%bind () =
    [%expect {|
      (Error (
        "[Protocol_version_header.negotiate]: no shared version numbers"
        (us_versions   (2))
        (peer_versions (1))
        (protocol Krb)))
    |}]
  in
  return ()
;;
