open! Core_kernel
open! Expect_test_helpers_kernel

let%expect_test "[sexp_of_int] respects [sexp_of_int_style]" =
  let sexp_of_int = Core_kernel.Core_kernel_stable.sexp_of_int in
  let r = Int_conversions.sexp_of_int_style in
  let old = !r in
  r := `Underscores;
  print_s [%sexp (1234 : int)];
  [%expect {|
    1_234 |}];
  r := `No_underscores;
  print_s [%sexp (1234 : int)];
  [%expect {|
    1234 |}];
  r := old;
;;

let%expect_test "older [int_of_sexp] supports both [sexp_of_int_style]s" =
  let sexp_of_int = Core_kernel.Core_kernel_stable.sexp_of_int in
  let int_of_sexp = Sexplib.Std.int_of_sexp in
  let print () = print_s [%sexp (int_of_sexp (sexp_of_int 1234) : int)] in
  let r = Int_conversions.sexp_of_int_style in
  let old = !r in
  r := `Underscores;
  print ();
  [%expect {|
    1_234 |}];
  r := `No_underscores;
  print ();
  [%expect {|
    1234 |}];
  r := old;
;;
