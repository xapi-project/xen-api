open! Core_kernel
open  Expect_test_helpers_kernel

let%expect_test _ =
  print_and_check_comparable_sexps [%here] (module Percent) [
    Percent.zero;
    Percent.of_bp         15.;
    Percent.of_percentage 15.;
    Percent.of_mult       15.;
  ];
  [%expect {|
    (Set (0x 15bp 15% 15x))
    (Map (
      (0x   0)
      (15bp 1)
      (15%  2)
      (15x  3))) |}];
;;

let%expect_test "accept float in set and map sexps" =
  let module Of_string (M : Sexpable.S1) = struct
    type t = string M.t [@@deriving sexp]
  end in
  let test (module M : Sexpable) string =
    print_s (M.sexp_of_t (M.t_of_sexp (Sexp.of_string string)))
  in
  test (module Percent.Set) {| (0 0.0001 0.01 1) |};
  [%expect {| (0x 1bp 1% 1x) |}];
  test (module Of_string (Percent.Map)) {|
    ((0      "arbitrary value")
     (0.0001 "arbitrary value")
     (0.01   "arbitrary value")
     (1      "arbitrary value")) |};
  [%expect {|
    ((0x  "arbitrary value")
     (1bp "arbitrary value")
     (1%  "arbitrary value")
     (1x  "arbitrary value")) |}];
;;
