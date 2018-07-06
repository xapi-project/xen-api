open Core_kernel
open Expect_test_helpers_kernel

open Md5

let%expect_test "digest_bin_prot" =
  let test (type t) (t : t) (module M : Binable.S with type t = t) =
    print_s [%sexp (digest_bin_prot M.bin_writer_t t : Md5.t)]
  in
  test () (module Unit);
  test 1337 (module Int);
  [%expect {|
    93b885adfe0da089cdf634904fd59f71
    22eaa1d1a43daf2abf8bb3f7b8d0128c |}];
;;
