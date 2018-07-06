open! Core_kernel
open  Expect_test_helpers_kernel

type time_ns = Time_ns.t [@@deriving compare]
let sexp_of_time_ns = Time_ns.Alternate_sexp.sexp_of_t
;;

type time_span = Time_ns.Span.t [@@deriving compare]
let sexp_of_time_span = Time_ns.Span.Alternate_sexp.sexp_of_t
;;

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map ns_since_epoch =
    Int63.gen_incl
      (Time_ns.to_int63_ns_since_epoch Time_ns.min_value)
      (Time_ns.to_int63_ns_since_epoch Time_ns.max_value)
  in
  Time_ns.of_int63_ns_since_epoch ns_since_epoch
;;

let randomly_round gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind time_ns = gen in
  let%map  unit =
    Quickcheck.Generator.of_list
      [ Time_ns.Span.second
      ; Time_ns.Span.of_int_ms 100
      ; Time_ns.Span.of_int_ms  10
      ; Time_ns.Span.millisecond
      ; Time_ns.Span.of_int_us 100
      ; Time_ns.Span.of_int_us  10
      ; Time_ns.Span.microsecond
      ; Time_ns.Span.of_int63_ns (Int63.of_int 100)
      ; Time_ns.Span.of_int63_ns (Int63.of_int  10)
      ; Time_ns.Span.nanosecond
      ]
  in
  let span_ns = Time_ns.to_span_since_epoch time_ns in
  let rounded_span_ns =
    Time_ns.Span.scale_int63 unit
      (Time_ns.Span.div span_ns unit)
  in
  Time_ns.of_span_since_epoch rounded_span_ns
;;

let quickcheck here ?(gen = gen) f =
  require_does_not_raise here (fun () ->
    Quickcheck.test gen ~f
      ~sexp_of:[%sexp_of: time_ns]
      ~examples:[ Time_ns.min_value; Time_ns.epoch; Time_ns.max_value ])
;;

let%test_module "Time_ns.Alternate_sexp" =
  (module struct
    module Span = Time_ns.Span
    ;;

    let epoch_date = Date.create_exn ~y:1970 ~m:Jan ~d:1
    ;;

    let make ?(h=0) ?(m=0) ?(s=0) ?(ms=0) ?(us=0) ?(ns=0) date =
      let d = Date.diff (Date.of_string date) epoch_date in
      let spans =
        [ Span.scale_int Span.day         d
        ; Span.scale_int Span.hour        h
        ; Span.scale_int Span.minute      m
        ; Span.scale_int Span.second      s
        ; Span.scale_int Span.millisecond ms
        ; Span.scale_int Span.microsecond us
        ; Span.scale_int Span.nanosecond  ns
        ]
      in
      let span = List.fold ~init:Span.zero ~f:Span.( + ) spans in
      Time_ns.of_span_since_epoch span
    ;;

    let%expect_test "sexp format" =
      let examples =
        [ Time_ns.min_value
        ; Time_ns.max_value
        ; Time_ns.epoch
        ; make "2001-01-01"
        ; make "2001-01-01" ~h:16 ~m:23 ~s:42
        ; make "2013-10-07" ~h:09 ~m:14 ~s:47 ~ms:999 ~us:749 ~ns:999
        ; make "2013-10-07" ~h:09 ~m:14 ~s:47 ~ms:999 ~us:750 ~ns:000
        ; make "2013-10-07" ~h:09 ~m:14 ~s:47 ~ms:999 ~us:750 ~ns:001
        ]
      in
      List.iter examples ~f:(fun time_ns ->
        print_s [%sexp (time_ns : Time_ns.Alternate_sexp.t)];
        let round_trip =
          (Time_ns.Alternate_sexp.t_of_sexp
             (Time_ns.Alternate_sexp.sexp_of_t time_ns))
        in
        require [%here] (Time_ns.equal time_ns round_trip)
          ~if_false_then_print_s:
            (lazy [%message
              "Time_ns.Alternate_sexp round-trip failed"
                (time_ns    : time_ns)
                (round_trip : time_ns)]));
      [%expect {|
        "1835-02-03 00:00:00Z"
        "2104-11-29 00:00:00Z"
        "1970-01-01 00:00:00Z"
        "2001-01-01 00:00:00Z"
        "2001-01-01 16:23:42Z"
        "2013-10-07 09:14:47.999749999Z"
        "2013-10-07 09:14:47.99975Z"
        "2013-10-07 09:14:47.999750001Z" |}];
    ;;

    let%expect_test "round-trip" =
      (* randomly round spans to make sure we round-trip at various precisions, since it
         affects number of decimal places *)
      quickcheck [%here] ~gen:(randomly_round gen) (fun time_ns ->
        [%test_result: time_ns]
          (Time_ns.Alternate_sexp.t_of_sexp
             (Time_ns.Alternate_sexp.sexp_of_t time_ns))
          ~expect:time_ns);
      [%expect {| |}];
    ;;
  end)

let%test_module "Time_ns.Utc.to_date_and_span_since_start_of_day" =
  (module struct

    let%expect_test "span is non-negative and less than 1 day" =
      quickcheck [%here] (fun time_ns ->
        let date, span_since_start_of_day =
          Core_kernel.Time_ns.Utc.to_date_and_span_since_start_of_day time_ns
        in
        if Time_ns.Span.( <  ) span_since_start_of_day Time_ns.Span.zero
        || Time_ns.Span.( >= ) span_since_start_of_day Time_ns.Span.day
        then begin
          raise_s [%message
            "span_since_start_of_day is out of bounds"
              (time_ns                 : time_ns)
              (date                    : Date.t)
              (span_since_start_of_day : time_span)]
        end);
      [%expect {| |}];
    ;;

    let%expect_test "round-trip" =
      quickcheck [%here] (fun time_ns ->
        let date, span_since_start_of_day =
          Core_kernel.Time_ns.Utc.to_date_and_span_since_start_of_day time_ns
        in
        let round_trip_time_ns =
          Time_ns.Utc.of_date_and_span_since_start_of_day date span_since_start_of_day
        in
        [%test_result: time_ns]
          round_trip_time_ns
          ~expect:time_ns);
      [%expect {| |}];
    ;;
  end)

let succ time_ns =
  time_ns
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.succ
  |> Time_ns.of_int63_ns_since_epoch

let pred time_ns =
  time_ns
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.pred
  |> Time_ns.of_int63_ns_since_epoch

let%expect_test "Stable.Alternate_sexp.V1" =
  print_and_check_stable_type [%here] (module Time_ns.Stable.Alternate_sexp.V1) [
    Time_ns.min_value;
    Time_ns.min_value |> succ;
    Time_ns.min_value |> succ |> succ;
    Time_ns.epoch |> pred;
    Time_ns.epoch;
    Time_ns.epoch |> succ;
    Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn   999_999_999_999_999_999L);
    Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn 1_000_000_000_000_000_000L);
    Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn 1_000_000_000_000_000_001L);
    Time_ns.max_value |> pred |> pred;
    Time_ns.max_value |> pred;
    Time_ns.max_value;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   "1835-02-03 00:00:00Z")
     (bin_io "\252\000\000\011\239\162\209\234\196"))
    ((sexp   "1835-02-03 00:00:00.000000001Z")
     (bin_io "\252\001\000\011\239\162\209\234\196"))
    ((sexp   "1835-02-03 00:00:00.000000002Z")
     (bin_io "\252\002\000\011\239\162\209\234\196"))
    ((sexp   "1969-12-31 23:59:59.999999999Z")
     (bin_io "\255\255"))
    ((sexp   "1970-01-01 00:00:00Z")
     (bin_io "\000"))
    ((sexp   "1970-01-01 00:00:00.000000001Z")
     (bin_io "\001"))
    ((sexp   "2001-09-09 01:46:39.999999999Z")
     (bin_io "\252\255\255c\167\179\182\224\r"))
    ((sexp   "2001-09-09 01:46:40Z")
     (bin_io "\252\000\000d\167\179\182\224\r"))
    ((sexp   "2001-09-09 01:46:40.000000001Z")
     (bin_io "\252\001\000d\167\179\182\224\r"))
    ((sexp   "2104-11-28 23:59:59.999999998Z")
     (bin_io "\252\254\255\244\016].\021;"))
    ((sexp   "2104-11-28 23:59:59.999999999Z")
     (bin_io "\252\255\255\244\016].\021;"))
    ((sexp   "2104-11-29 00:00:00Z")
     (bin_io "\252\000\000\245\016].\021;")) |}];
;;
