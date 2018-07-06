open! Core_kernel
open! Import
open! Sexp

module With_text = struct
  open! With_text
  let sexp_of_il = sexp_of_list sexp_of_int
  let il_of_sexp = list_of_sexp int_of_sexp

  let il_of_text text = Or_error.ok_exn (of_text il_of_sexp text)
  let il_of_value il  = of_value sexp_of_il il

  module IL = struct
    type t = int list [@@deriving compare, sexp_of]
    let equal = [%compare.equal: t]
  end

  let t = il_of_value [3;4]

  let%expect_test _ =
    print_s [%sexp (text t : string)];
    [%expect {| "(3 4)" |}];
  ;;

  let t' = il_of_text (text t)

  let%expect_test _ =
    require_equal [%here] (module IL) (value t') [3;4];
    [%expect {| |}];
  ;;

  let%expect_test _ =
    print_s [%sexp (t : il t)];
    [%expect {| "(3 4)" |}];
  ;;

  let%expect_test _ =
    require_equal [%here] (module IL) (value (t_of_sexp il_of_sexp (Atom "(3 4)"))) [3;4];
    [%expect {| |}];
  ;;

  let%expect_test _ =
    require_equal [%here] (module IL) [8;9]
      (value (il_of_text ";this is a comment\n (8; foo\n 9)   \n "));
    [%expect {| |}];
  ;;

  let%expect_test _ =
    require_does_raise [%here] (fun () -> il_of_text "(1 2 bla)");
    [%expect {|
      (Sexplib.Conv.Of_sexp_error
        (Sexplib.Sexp.Annotated.Conv_exn :1:5 (
          Failure "int_of_sexp: (Failure int_of_string)"))
        bla) |}];
  ;;

  let%expect_test _ =
    require_does_raise [%here] (fun () ->
      t_of_sexp il_of_sexp (Sexp.of_string "\"(1 2 bla)\""));
    [%expect {|
      (Sexplib.Conv.Of_sexp_error
        (Sexplib.Sexp.Annotated.Conv_exn :1:5 (
          Failure "int_of_sexp: (Failure int_of_string)"))
        bla) |}];
  ;;
end
