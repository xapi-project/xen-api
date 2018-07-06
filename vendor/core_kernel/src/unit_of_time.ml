open! Import

type t =
  | Nanosecond
  | Microsecond
  | Millisecond
  | Second
  | Minute
  | Hour
  | Day
[@@deriving sexp, compare, hash]

let%test_unit "Span.Unit_of_time.t" =
  [%test_result: int] (compare Nanosecond  Microsecond) ~expect:(-1);
  [%test_result: int] (compare Microsecond Millisecond) ~expect:(-1);
  [%test_result: int] (compare Millisecond Second)      ~expect:(-1);
  [%test_result: int] (compare Second      Minute)      ~expect:(-1);
  [%test_result: int] (compare Minute      Hour)        ~expect:(-1);
  [%test_result: int] (compare Hour        Day)         ~expect:(-1)
