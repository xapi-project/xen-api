open! Core_kernel
open  Core_kernel_private.Digit_string_helpers
open Import

let int63_ten = Int63.of_int 10

let rec digits_of int63 =
  if Int63.( < ) int63 int63_ten
  then 1
  else Int.succ (digits_of (Int63.( / ) int63 int63_ten))

let max_int63_digits =
  (* subtract one because int63 cannot encode all values with as many digits as
     [max_value] *)
  (digits_of Int63.max_value) - 1

let%expect_test "max_int63_digits" =
  print_s [%sexp (max_int63_digits : int)];
  [%expect {| 18 |}];
;;

let max_with ~digits =
  Int63.pred (Int63.pow int63_ten (Int63.of_int digits))

let test_write_int63 ~digits ?(verbose = true) ?(align = digits) write_int63 =
  let print_endline = if verbose then print_endline else ignore in
  let require_does_raise here f =
    (* uses above print_endline, so if verbose is false, prints nothing on exn *)
    match f () with
    | _             -> require_does_raise here ignore
    | exception exn -> print_endline (Exn.to_string exn)
  in
  let max = max_with ~digits in
  print_endline "Expecting success:";
  (* show resulting strings at boundary values *)
  let show int63 =
    let bytes = Bytes.make digits '!' in
    write_int63 bytes ~pos:0 int63;
    printf !"%*Ld -> %S\n" align (Int63.to_int64 int63) (Bytes.to_string bytes)
  in
  show Int63.zero;
  show max;
  (* test success behavior for lots of correct values *)
  let expect_success_exn int63 =
    let bytes = Bytes.make (1 + digits + 1) '!' in
    write_int63 bytes ~pos:1 int63;
    [%test_result: string]
      (Bytes.to_string bytes)
      ~expect:(sprintf ("!%0*Ld!") digits (Int63.to_int64 int63))
  in
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      (Int63.gen_log_uniform_incl Int63.zero max)
      ~examples:[Int63.zero; max]
      ~sexp_of:Int63.sexp_of_t
      ~f:expect_success_exn);
  (* test failure cases *)
  print_endline "";
  print_endline "Expecting failure:";
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make 0 '?') ~pos:0 Int63.zero);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:(-1) Int63.zero);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:1 Int63.zero);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:0 Int63.minus_one);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:0 (Int63.succ max));
;;

let test_write_int write_int ~digits =
  let write bytes ~pos int63 =
    write_int bytes ~pos (Int63.to_int_exn int63)
  in
  test_write_int63 write ~digits
;;

let%expect_test "write_1_digit_int" =
  test_write_int write_1_digit_int ~digits:1;
  [%expect {|
    Expecting success:
    0 -> "0"
    9 -> "9"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: pos=-1 out of range for string of length 1")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: pos=1 out of range for string of length 1")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: -1 out of range [0, 9]")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: 10 out of range [0, 9]") |}];
;;

let%expect_test "write_2_digit_int" =
  test_write_int write_2_digit_int ~digits:2;
  [%expect {|
    Expecting success:
     0 -> "00"
    99 -> "99"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: pos=-1 out of range for string of length 2")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: 2 digits do not fit at pos 1 in string of length 2")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: -1 out of range [0, 99]")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: 100 out of range [0, 99]") |}];
;;


let%expect_test "write_3_digit_int" =
  test_write_int write_3_digit_int ~digits:3;
  [%expect {|
    Expecting success:
      0 -> "000"
    999 -> "999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: pos=-1 out of range for string of length 3")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: 3 digits do not fit at pos 1 in string of length 3")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: -1 out of range [0, 999]")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: 1000 out of range [0, 999]") |}];
;;


let%expect_test "write_4_digit_int" =
  test_write_int write_4_digit_int ~digits:4;
  [%expect {|
    Expecting success:
       0 -> "0000"
    9999 -> "9999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: pos=-1 out of range for string of length 4")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: 4 digits do not fit at pos 1 in string of length 4")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: -1 out of range [0, 9999]")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: 10000 out of range [0, 9999]") |}];
;;

let%expect_test "write_5_digit_int" =
  test_write_int write_5_digit_int ~digits:5;
  [%expect {|
    Expecting success:
        0 -> "00000"
    99999 -> "99999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: pos=-1 out of range for string of length 5")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: 5 digits do not fit at pos 1 in string of length 5")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: -1 out of range [0, 99999]")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: 100000 out of range [0, 99999]") |}];
;;

let%expect_test "write_6_digit_int" =
  test_write_int write_6_digit_int ~digits:6;
  [%expect {|
    Expecting success:
         0 -> "000000"
    999999 -> "999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: pos=-1 out of range for string of length 6")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: 6 digits do not fit at pos 1 in string of length 6")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: -1 out of range [0, 999999]")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: 1000000 out of range [0, 999999]") |}];
;;

let%expect_test "write_7_digit_int" =
  test_write_int write_7_digit_int ~digits:7;
  [%expect {|
    Expecting success:
          0 -> "0000000"
    9999999 -> "9999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: pos=-1 out of range for string of length 7")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: 7 digits do not fit at pos 1 in string of length 7")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: -1 out of range [0, 9999999]")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: 10000000 out of range [0, 9999999]") |}];
;;

let%expect_test "write_8_digit_int" =
  test_write_int write_8_digit_int ~digits:8;
  [%expect {|
    Expecting success:
           0 -> "00000000"
    99999999 -> "99999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: pos=-1 out of range for string of length 8")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: 8 digits do not fit at pos 1 in string of length 8")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: -1 out of range [0, 99999999]")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: 100000000 out of range [0, 99999999]") |}];
;;

let%expect_test "write_9_digit_int" =
  test_write_int write_9_digit_int ~digits:9;
  [%expect {|
    Expecting success:
            0 -> "000000000"
    999999999 -> "999999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: pos=-1 out of range for string of length 9")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: 9 digits do not fit at pos 1 in string of length 9")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: -1 out of range [0, 999999999]")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: 1000000000 out of range [0, 999999999]") |}];
;;

let%expect_test "write_int63" =
  for digits = 1 to max_int63_digits do
    require_does_not_raise [%here] (fun () ->
      test_write_int63
        ~verbose:(digits = max_int63_digits)
        ~align:max_int63_digits
        ~digits
        (write_int63 ~digits))
  done;
  [%expect {|
                     0 -> "0"
                     9 -> "9"
                     0 -> "00"
                    99 -> "99"
                     0 -> "000"
                   999 -> "999"
                     0 -> "0000"
                  9999 -> "9999"
                     0 -> "00000"
                 99999 -> "99999"
                     0 -> "000000"
                999999 -> "999999"
                     0 -> "0000000"
               9999999 -> "9999999"
                     0 -> "00000000"
              99999999 -> "99999999"
                     0 -> "000000000"
             999999999 -> "999999999"
                     0 -> "0000000000"
            9999999999 -> "9999999999"
                     0 -> "00000000000"
           99999999999 -> "99999999999"
                     0 -> "000000000000"
          999999999999 -> "999999999999"
                     0 -> "0000000000000"
         9999999999999 -> "9999999999999"
                     0 -> "00000000000000"
        99999999999999 -> "99999999999999"
                     0 -> "000000000000000"
       999999999999999 -> "999999999999999"
                     0 -> "0000000000000000"
      9999999999999999 -> "9999999999999999"
                     0 -> "00000000000000000"
     99999999999999999 -> "99999999999999999"
    Expecting success:
                     0 -> "000000000000000000"
    999999999999999999 -> "999999999999999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_int63: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_int63: pos=-1 out of range for string of length 18")
    (Invalid_argument
      "Digit_string_helpers.write_int63: 18 digits do not fit at pos 1 in string of length 18")
    (Invalid_argument
      "Digit_string_helpers.write_int63: -1 out of range [0, 999999999999999999]")
    (Invalid_argument
      "Digit_string_helpers.write_int63: 1000000000000000000 out of range [0, 999999999999999999]") |}];
  (* write more digits than Int63.max_value fills *)
  let bytes = Bytes.make 50 '_' in
  write_int63 bytes ~pos:10 ~digits:30 Int63.max_value;
  print_s [%sexp (bytes : Bytes.t)];
  [%expect {| __________000000000004611686018427387903__________ |}];
;;

let test_read_int63 ?(verbose = true) read_int63 ~digits =
  let print_endline = if verbose then print_endline else ignore in
  let require_does_raise here f =
    match f () with
    | _             -> require_does_raise here ignore
    | exception exn -> print_endline (Exn.to_string exn)
  in
  let max = max_with ~digits in
  print_endline "Expecting success:";
  (* show resulting strings at boundary values *)
  let show int63 =
    let string = sprintf "%0*Ld" digits (Int63.to_int64 int63) in
    let parsed = read_int63 string ~pos:0 in
    printf !"%*S -> %{Int63}\n" (max_int63_digits + 2) string parsed
  in
  show Int63.zero;
  show max;
  (* test success behavior for lots of correct values *)
  let expect_success_exn int63 =
    let string = sprintf "!%0*Ld!" digits (Int63.to_int64 int63) in
    let parsed = read_int63 string ~pos:1 in
    [%test_result: Int63.t] parsed ~expect:int63
  in
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      (Int63.gen_log_uniform_incl Int63.zero max)
      ~examples:[Int63.zero; max]
      ~sexp_of:Int63.sexp_of_t
      ~f:expect_success_exn);
  (* test failure cases *)
  print_endline "";
  print_endline "Expecting failure:";
  require_does_raise [%here] (fun () ->
    read_int63 "" ~pos:0);
  require_does_raise [%here] (fun () ->
    read_int63 (sprintf "%0*Ld" digits 0L) ~pos:(-1));
  require_does_raise [%here] (fun () ->
    read_int63 (sprintf "%0*Ld" digits 0L) ~pos:1);
  require_does_raise [%here] (fun () ->
    read_int63 (String.make digits '!') ~pos:0);
;;

let test_read_int read_int ~digits =
  let read string ~pos =
    Int63.of_int (read_int string ~pos)
  in
  test_read_int63 read ~digits
;;

let%expect_test "read_1_digit_int" =
  test_read_int read_1_digit_int ~digits:1;
  [%expect {|
    Expecting success:
                     "0" -> 0
                     "9" -> 9

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_1_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_1_digit_int: pos=-1 out of range for string of length 1")
    (Invalid_argument
      "Digit_string_helpers.read_1_digit_int: pos=1 out of range for string of length 1")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_2_digit_int" =
  test_read_int read_2_digit_int ~digits:2;
  [%expect {|
    Expecting success:
                    "00" -> 0
                    "99" -> 99

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_2_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_2_digit_int: pos=-1 out of range for string of length 2")
    (Invalid_argument
      "Digit_string_helpers.read_2_digit_int: 2 digits do not fit at pos 1 in string of length 2")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_3_digit_int" =
  test_read_int read_3_digit_int ~digits:3;
  [%expect {|
    Expecting success:
                   "000" -> 0
                   "999" -> 999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_3_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_3_digit_int: pos=-1 out of range for string of length 3")
    (Invalid_argument
      "Digit_string_helpers.read_3_digit_int: 3 digits do not fit at pos 1 in string of length 3")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_4_digit_int" =
  test_read_int read_4_digit_int ~digits:4;
  [%expect {|
    Expecting success:
                  "0000" -> 0
                  "9999" -> 9999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_4_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_4_digit_int: pos=-1 out of range for string of length 4")
    (Invalid_argument
      "Digit_string_helpers.read_4_digit_int: 4 digits do not fit at pos 1 in string of length 4")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_5_digit_int" =
  test_read_int read_5_digit_int ~digits:5;
  [%expect {|
    Expecting success:
                 "00000" -> 0
                 "99999" -> 99999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_5_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_5_digit_int: pos=-1 out of range for string of length 5")
    (Invalid_argument
      "Digit_string_helpers.read_5_digit_int: 5 digits do not fit at pos 1 in string of length 5")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_6_digit_int" =
  test_read_int read_6_digit_int ~digits:6;
  [%expect {|
    Expecting success:
                "000000" -> 0
                "999999" -> 999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_6_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_6_digit_int: pos=-1 out of range for string of length 6")
    (Invalid_argument
      "Digit_string_helpers.read_6_digit_int: 6 digits do not fit at pos 1 in string of length 6")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_7_digit_int" =
  test_read_int read_7_digit_int ~digits:7;
  [%expect {|
    Expecting success:
               "0000000" -> 0
               "9999999" -> 9999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_7_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_7_digit_int: pos=-1 out of range for string of length 7")
    (Invalid_argument
      "Digit_string_helpers.read_7_digit_int: 7 digits do not fit at pos 1 in string of length 7")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_8_digit_int" =
  test_read_int read_8_digit_int ~digits:8;
  [%expect {|
    Expecting success:
              "00000000" -> 0
              "99999999" -> 99999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_8_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_8_digit_int: pos=-1 out of range for string of length 8")
    (Invalid_argument
      "Digit_string_helpers.read_8_digit_int: 8 digits do not fit at pos 1 in string of length 8")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_9_digit_int" =
  test_read_int read_9_digit_int ~digits:9;
  [%expect {|
    Expecting success:
             "000000000" -> 0
             "999999999" -> 999999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_9_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_9_digit_int: pos=-1 out of range for string of length 9")
    (Invalid_argument
      "Digit_string_helpers.read_9_digit_int: 9 digits do not fit at pos 1 in string of length 9")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_int63" =
  for digits = 1 to max_int63_digits do
    require_does_not_raise [%here] (fun () ->
      test_read_int63 ~verbose:(digits = max_int63_digits) ~digits (read_int63 ~digits))
  done;
  [%expect {|
                     "0" -> 0
                     "9" -> 9
                    "00" -> 0
                    "99" -> 99
                   "000" -> 0
                   "999" -> 999
                  "0000" -> 0
                  "9999" -> 9999
                 "00000" -> 0
                 "99999" -> 99999
                "000000" -> 0
                "999999" -> 999999
               "0000000" -> 0
               "9999999" -> 9999999
              "00000000" -> 0
              "99999999" -> 99999999
             "000000000" -> 0
             "999999999" -> 999999999
            "0000000000" -> 0
            "9999999999" -> 9999999999
           "00000000000" -> 0
           "99999999999" -> 99999999999
          "000000000000" -> 0
          "999999999999" -> 999999999999
         "0000000000000" -> 0
         "9999999999999" -> 9999999999999
        "00000000000000" -> 0
        "99999999999999" -> 99999999999999
       "000000000000000" -> 0
       "999999999999999" -> 999999999999999
      "0000000000000000" -> 0
      "9999999999999999" -> 9999999999999999
     "00000000000000000" -> 0
     "99999999999999999" -> 99999999999999999
    Expecting success:
    "000000000000000000" -> 0
    "999999999999999999" -> 999999999999999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_int63: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_int63: pos=-1 out of range for string of length 18")
    (Invalid_argument
      "Digit_string_helpers.read_int63: 18 digits do not fit at pos 1 in string of length 18")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
  (* read more digits than Int63 can represent *)
  let string = String.make 50 '0' in
  let int63 = read_int63 string ~pos:10 ~digits:30 in
  print_s [%sexp (int63 : Int63.t)];
  require_equal [%here] (module Int63) int63 Int63.zero;
  [%expect {| 0 |}];
  (* read Int63.max_value without overflowing *)
  let string = sprintf "%010d%030Ld%010d" 0 (Int63.to_int64 Int63.max_value) 0 in
  let int63 = read_int63 string ~pos:10 ~digits:30 in
  print_s [%sexp (int63 : Int63.t)];
  require_equal [%here] (module Int63) int63 Int63.max_value;
  [%expect {| 4_611_686_018_427_387_903 |}];
  (* raise on overflow *)
  let string = String.make 50 '9' in
  require_does_raise [%here] (fun () ->
    read_int63 string ~pos:10 ~digits:30);
  [%expect {| (Invalid_argument "Digit_string_helpers.read_int63: overflow reading int63") |}];
;;
