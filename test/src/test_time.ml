open! Core_kernel
open  Expect_test_helpers_kernel

let utc date_string ofday_string =
  Time.of_date_ofday
    (Date.of_string date_string)
    (Time.Ofday.of_string ofday_string)
    ~zone:Time.Zone.utc

let examples = [
  Time.epoch;
  utc "2001-01-01" "00:00:00";
  utc "2013-10-07" "09:30:00";
  utc "2017-07-28" "11:57:00.000123";
]

let%expect_test "Time.Stable.With_utc_sexp.V2" =
  print_and_check_stable_type [%here] (module Time.Stable.With_utc_sexp.V2)
    examples;
  [%expect {|
    (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
    ((sexp (1970-01-01 00:00:00.000000Z))
     (bin_io "\000\000\000\000\000\000\000\000"))
    ((sexp (2001-01-01 00:00:00.000000Z)) (bin_io "\000\000\000@\228'\205A"))
    ((sexp (2013-10-07 09:30:00.000000Z))
     (bin_io "\000\000\000\198\159\148\212A"))
    ((sexp (2017-07-28 11:57:00.000123Z)) (bin_io "\004\002\000\163\201^\214A")) |}];
;;

let span_examples =
  let units =
    [ Time.Span.nanosecond
    ; Time.Span.microsecond
    ; Time.Span.millisecond
    ; Time.Span.second
    ; Time.Span.minute
    ; Time.Span.hour
    ; Time.Span.day
    ]
  in
  let pos_and_neg_units =
    units @ List.map units ~f:Time.Span.neg
  in
  Time.Span.zero
  :: pos_and_neg_units
  @  List.map pos_and_neg_units ~f:(fun span -> Time.Span.scale span Float.pi)

let%expect_test "Time.Stable.Span.V1" =
  print_and_check_stable_type [%here] (module struct
    include Time.Stable.Span.V1

    (* [V1] does not precisely round-trip for all suffixes. So we use a comparison that
       requires accuracy up to one part in a million. *)
    let compare t1 t2 =
      let open Time.Span in
      let magnitude = max (abs t1) (abs t2) in
      let epsilon = Time.Span.( / ) magnitude 1_000_000. in
      let diff = t1 - t2 in
      if diff < neg epsilon
      then -1
      else if diff > epsilon
      then 1
      else 0
  end)
    span_examples;
  [%expect {|
    (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
    ((sexp   0s)
     (bin_io "\000\000\000\000\000\000\000\000"))
    ((sexp   1e-06ms)
     (bin_io "\149\214&\232\011.\017>"))
    ((sexp   0.001ms)
     (bin_io "\141\237\181\160\247\198\176>"))
    ((sexp   1ms)
     (bin_io "\252\169\241\210MbP?"))
    ((sexp   1s)
     (bin_io "\000\000\000\000\000\000\240?"))
    ((sexp   1m)
     (bin_io "\000\000\000\000\000\000N@"))
    ((sexp   1h)
     (bin_io "\000\000\000\000\000 \172@"))
    ((sexp   1d)
     (bin_io "\000\000\000\000\000\024\245@"))
    ((sexp   -1e-06ms)
     (bin_io "\149\214&\232\011.\017\190"))
    ((sexp   -0.001ms)
     (bin_io "\141\237\181\160\247\198\176\190"))
    ((sexp   -1ms)
     (bin_io "\252\169\241\210MbP\191"))
    ((sexp   -1s)
     (bin_io "\000\000\000\000\000\000\240\191"))
    ((sexp   -1m)
     (bin_io "\000\000\000\000\000\000N\192"))
    ((sexp   -1h)
     (bin_io "\000\000\000\000\000 \172\192"))
    ((sexp   -1d)
     (bin_io "\000\000\000\000\000\024\245\192"))
    ((sexp   3.14159e-06ms)
     (bin_io "\229;!po\252*>"))
    ((sexp   0.00314159ms)
     (bin_io "}t\128\211\132Z\202>"))
    ((sexp   3.14159ms)
     (bin_io "\195q\139\182e\188i?"))
    ((sexp   3.14159s)
     (bin_io "\024-DT\251!\t@"))
    ((sexp   3.14159m)
     (bin_io "F\234\255\158\219\143g@"))
    ((sexp   3.14159h)
     (bin_io "\162\235\015\229\221\022\198@"))
    ((sexp   3.14159d)
     (bin_io "\186\240\203k&\145\016A"))
    ((sexp   -3.14159e-06ms)
     (bin_io "\229;!po\252*\190"))
    ((sexp   -0.00314159ms)
     (bin_io "}t\128\211\132Z\202\190"))
    ((sexp   -3.14159ms)
     (bin_io "\195q\139\182e\188i\191"))
    ((sexp   -3.14159s)
     (bin_io "\024-DT\251!\t\192"))
    ((sexp   -3.14159m)
     (bin_io "F\234\255\158\219\143g\192"))
    ((sexp   -3.14159h)
     (bin_io "\162\235\015\229\221\022\198\192"))
    ((sexp   -3.14159d)
     (bin_io "\186\240\203k&\145\016\193")) |}];
;;

let%expect_test "Time.Stable.Span.V2" =
  print_and_check_stable_type [%here] (module Time.Stable.Span.V2)
    ~cr:Comment
    span_examples;
  [%expect {|
    (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
    ((sexp   0s)
     (bin_io "\000\000\000\000\000\000\000\000"))
    ((sexp   1ns)
     (bin_io "\149\214&\232\011.\017>"))
    ((sexp   1us)
     (bin_io "\141\237\181\160\247\198\176>"))
    ((sexp   1ms)
     (bin_io "\252\169\241\210MbP?"))
    ((sexp   1s)
     (bin_io "\000\000\000\000\000\000\240?"))
    ((sexp   1m)
     (bin_io "\000\000\000\000\000\000N@"))
    ((sexp   1h)
     (bin_io "\000\000\000\000\000 \172@"))
    ((sexp   1d)
     (bin_io "\000\000\000\000\000\024\245@"))
    ((sexp   -1ns)
     (bin_io "\149\214&\232\011.\017\190"))
    ((sexp   -1us)
     (bin_io "\141\237\181\160\247\198\176\190"))
    ((sexp   -1ms)
     (bin_io "\252\169\241\210MbP\191"))
    ((sexp   -1s)
     (bin_io "\000\000\000\000\000\000\240\191"))
    ((sexp   -1m)
     (bin_io "\000\000\000\000\000\000N\192"))
    ((sexp   -1h)
     (bin_io "\000\000\000\000\000 \172\192"))
    ((sexp   -1d)
     (bin_io "\000\000\000\000\000\024\245\192"))
    ((sexp   3.1415926535897931ns)
     (bin_io "\229;!po\252*>"))
    ((sexp   3.1415926535897931us)
     (bin_io "}t\128\211\132Z\202>"))
    (* require-failed: lib/core_kernel/test/src/test_time.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       3.1415926535897931us)
      (sexp           3.1415926535897931us)
      (sexp_roundtrip 3.1415926535897931us))
    ((sexp   3.1415926535897931ms)
     (bin_io "\195q\139\182e\188i?"))
    ((sexp   3.1415926535897931s)
     (bin_io "\024-DT\251!\t@"))
    ((sexp   3.1415926535897927m)
     (bin_io "F\234\255\158\219\143g@"))
    ((sexp   3.1415926535897931h)
     (bin_io "\162\235\015\229\221\022\198@"))
    ((sexp   3.1415926535897936d)
     (bin_io "\186\240\203k&\145\016A"))
    ((sexp   -3.1415926535897931ns)
     (bin_io "\229;!po\252*\190"))
    ((sexp   -3.1415926535897931us)
     (bin_io "}t\128\211\132Z\202\190"))
    (* require-failed: lib/core_kernel/test/src/test_time.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       -3.1415926535897931us)
      (sexp           -3.1415926535897931us)
      (sexp_roundtrip -3.1415926535897931us))
    ((sexp   -3.1415926535897931ms)
     (bin_io "\195q\139\182e\188i\191"))
    ((sexp   -3.1415926535897931s)
     (bin_io "\024-DT\251!\t\192"))
    ((sexp   -3.1415926535897927m)
     (bin_io "F\234\255\158\219\143g\192"))
    ((sexp   -3.1415926535897931h)
     (bin_io "\162\235\015\229\221\022\198\192"))
    ((sexp   -3.1415926535897936d)
     (bin_io "\186\240\203k&\145\016\193")) |}];
;;

let%expect_test "Span.to_parts + Span.create" =
  List.iter span_examples ~f:(fun span ->
    let parts = Time.Span.to_parts span in
    Core_kernel.print_s [%sexp ((span, parts) : Time.Span.t * Time.Span.Parts.t)];
    let { sign; hr; min; sec; ms; us; ns } : Time.Span.Parts.t = parts in
    let round_trip = Time.Span.create ~sign ~hr ~min ~sec ~ms ~us ~ns () in
    let abs_diff = Time.Span.abs (Time.Span.( - ) span round_trip) in
    require [%here] (Time.Span.( < ) abs_diff Time.Span.nanosecond)
      ~if_false_then_print_s:
        (lazy [%message
          "round-trip failed"
            (span       : Time.Span.t)
            (parts      : Time.Span.Parts.t)
            (round_trip : Time.Span.t)
            (abs_diff   : Time.Span.t)]));
  [%expect {|
    (0s ((sign Zero) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 0)))
    (1e-06ms ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 1)))
    (0.001ms ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 1) (ns 0)))
    (1ms ((sign Pos) (hr 0) (min 0) (sec 0) (ms 1) (us 0) (ns 0)))
    (1s ((sign Pos) (hr 0) (min 0) (sec 1) (ms 0) (us 0) (ns 0)))
    (1m ((sign Pos) (hr 0) (min 1) (sec 0) (ms 0) (us 0) (ns 0)))
    (1h ((sign Pos) (hr 1) (min 0) (sec 0) (ms 0) (us 0) (ns 0)))
    (1d ((sign Pos) (hr 24) (min 0) (sec 0) (ms 0) (us 0) (ns 0)))
    (-1e-06ms ((sign Neg) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 1)))
    (-0.001ms ((sign Neg) (hr 0) (min 0) (sec 0) (ms 0) (us 1) (ns 0)))
    (-1ms ((sign Neg) (hr 0) (min 0) (sec 0) (ms 1) (us 0) (ns 0)))
    (-1s ((sign Neg) (hr 0) (min 0) (sec 1) (ms 0) (us 0) (ns 0)))
    (-1m ((sign Neg) (hr 0) (min 1) (sec 0) (ms 0) (us 0) (ns 0)))
    (-1h ((sign Neg) (hr 1) (min 0) (sec 0) (ms 0) (us 0) (ns 0)))
    (-1d ((sign Neg) (hr 24) (min 0) (sec 0) (ms 0) (us 0) (ns 0)))
    (3.14159e-06ms ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 3)))
    (0.00314159ms ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 3) (ns 142)))
    (3.14159ms ((sign Pos) (hr 0) (min 0) (sec 0) (ms 3) (us 141) (ns 593)))
    (3.14159s ((sign Pos) (hr 0) (min 0) (sec 3) (ms 141) (us 592) (ns 654)))
    (3.14159m ((sign Pos) (hr 0) (min 3) (sec 8) (ms 495) (us 559) (ns 215)))
    (3.14159h ((sign Pos) (hr 3) (min 8) (sec 29) (ms 733) (us 552) (ns 923)))
    (3.14159d ((sign Pos) (hr 75) (min 23) (sec 53) (ms 605) (us 270) (ns 158)))
    (-3.14159e-06ms ((sign Neg) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 3)))
    (-0.00314159ms ((sign Neg) (hr 0) (min 0) (sec 0) (ms 0) (us 3) (ns 142)))
    (-3.14159ms ((sign Neg) (hr 0) (min 0) (sec 0) (ms 3) (us 141) (ns 593)))
    (-3.14159s ((sign Neg) (hr 0) (min 0) (sec 3) (ms 141) (us 592) (ns 654)))
    (-3.14159m ((sign Neg) (hr 0) (min 3) (sec 8) (ms 495) (us 559) (ns 215)))
    (-3.14159h ((sign Neg) (hr 3) (min 8) (sec 29) (ms 733) (us 552) (ns 923)))
    (-3.14159d ((sign Neg) (hr 75) (min 23) (sec 53) (ms 605) (us 270) (ns 158))) |}];
;;

let ofday_examples =
  List.filter_map span_examples ~f:(fun span ->
    if Time.Span.( >= ) span Time.Span.zero
    && Time.Span.( <  ) span Time.Span.day
    then Some (Time.Ofday.of_span_since_start_of_day span)
    else None)

let%expect_test "Ofday.to_parts + Ofday.create" =
  List.iter ofday_examples ~f:(fun ofday ->
    let parts = Time.Ofday.to_parts ofday in
    Core_kernel.print_s [%sexp ((ofday, parts) : Time.Ofday.t * Time.Span.Parts.t)];
    let { sign = _; hr; min; sec; ms; us; ns } : Time.Span.Parts.t = parts in
    let round_trip = Time.Ofday.create ~hr ~min ~sec ~ms ~us ~ns () in
    let abs_diff = Time.Span.abs (Time.Ofday.diff ofday round_trip) in
    require [%here] (Time.Span.( < ) abs_diff Time.Span.nanosecond)
      ~if_false_then_print_s:
        (lazy [%message
          "round-trip failed"
            (ofday      : Time.Ofday.t)
            (parts      : Time.Span.Parts.t)
            (round_trip : Time.Ofday.t)
            (abs_diff   : Time.Span.t)]));
  [%expect {|
    (00:00:00.000000 ((sign Zero) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 0)))
    (00:00:00.000000 ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 1)))
    (00:00:00.000001 ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 1) (ns 0)))
    (00:00:00.001000 ((sign Pos) (hr 0) (min 0) (sec 0) (ms 1) (us 0) (ns 0)))
    (00:00:01.000000 ((sign Pos) (hr 0) (min 0) (sec 1) (ms 0) (us 0) (ns 0)))
    (00:01:00.000000 ((sign Pos) (hr 0) (min 1) (sec 0) (ms 0) (us 0) (ns 0)))
    (01:00:00.000000 ((sign Pos) (hr 1) (min 0) (sec 0) (ms 0) (us 0) (ns 0)))
    (00:00:00.000000 ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 0) (ns 3)))
    (00:00:00.000003 ((sign Pos) (hr 0) (min 0) (sec 0) (ms 0) (us 3) (ns 142)))
    (00:00:00.003142
     ((sign Pos) (hr 0) (min 0) (sec 0) (ms 3) (us 141) (ns 593)))
    (00:00:03.141593
     ((sign Pos) (hr 0) (min 0) (sec 3) (ms 141) (us 592) (ns 654)))
    (00:03:08.495559
     ((sign Pos) (hr 0) (min 3) (sec 8) (ms 495) (us 559) (ns 215)))
    (03:08:29.733553
     ((sign Pos) (hr 3) (min 8) (sec 29) (ms 733) (us 552) (ns 923))) |}];
;;

let%expect_test "time zone offset parsing" =
  let test string =
    print_endline (Time.to_string (Time.of_string string));
  in
  test "2000-01-01 12:34:56.789012-00:00";
  test "2000-01-01 12:34:56.789012-0:00";
  test "2000-01-01 12:34:56.789012-00";
  test "2000-01-01 12:34:56.789012-0";
  [%expect {|
    2000-01-01 12:34:56.789012Z
    2000-01-01 12:34:56.789012Z
    2000-01-01 12:34:56.789012Z
    2000-01-01 12:34:56.789012Z |}];
  test "2000-01-01 12:34:56.789012-05:00";
  test "2000-01-01 12:34:56.789012-5:00";
  test "2000-01-01 12:34:56.789012-05";
  test "2000-01-01 12:34:56.789012-5";
  [%expect {|
    2000-01-01 17:34:56.789012Z
    2000-01-01 17:34:56.789012Z
    2000-01-01 17:34:56.789012Z
    2000-01-01 17:34:56.789012Z |}];
  test "2000-01-01 12:34:56.789012-23:00";
  test "2000-01-01 12:34:56.789012-23";
  [%expect {|
    2000-01-02 11:34:56.789012Z
    2000-01-02 11:34:56.789012Z |}];
  test "2000-01-01 12:34:56.789012-24:00";
  test "2000-01-01 12:34:56.789012-24";
  [%expect {|
    2000-01-02 12:34:56.789012Z
    2000-01-02 12:34:56.789012Z |}];

;;
let%expect_test "time zone invalid offset parsing" =
  let test here string =
    require_does_raise here (fun () ->
      Time.of_string string)
  in
  test [%here] "2000-01-01 12:34:56.789012-0:";
  test [%here] "2000-01-01 12:34:56.789012-00:";
  test [%here] "2000-01-01 12:34:56.789012-0:0";
  test [%here] "2000-01-01 12:34:56.789012-00:0";
  test [%here] "2000-01-01 12:34:56.789012-:";
  test [%here] "2000-01-01 12:34:56.789012-:00";
  test [%here] "2000-01-01 12:34:56.789012-";
  [%expect {|
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-0:"
     ("Time.Ofday: invalid string"
      0:
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-00:"
     ("Time.Ofday: invalid string"
      00:
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-0:0"
     ("Time.Ofday: invalid string"
      0:0
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-00:0"
     ("Time.Ofday: invalid string"
      00:0
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-:"
     (Invalid_argument "index out of bounds"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-:00"
     (Failure "Char.get_digit_exn ':': not a digit"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-"
     (Invalid_argument "index out of bounds")) |}];
  test [%here] "2000-01-01 12:34:56.789012-25:00";
  test [%here] "2000-01-01 12:34:56.789012-25";
  [%expect {|
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-25:00"
     ("Time.Ofday: invalid string" 25:00 "hours out of bounds"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-25"
     ("Time.Ofday: invalid string" 25:00 "hours out of bounds")) |}];
  test [%here] "2000-01-01 12:34:56.789012--1:00";
  test [%here] "2000-01-01 12:34:56.789012--1";
  [%expect {|
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012--1:00"
     (Failure "Char.get_digit_exn '-': not a digit"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012--1"
     (Invalid_argument "index out of bounds")) |}];

;;
