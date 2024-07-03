let uuid_strings =
  [
    "deadbeef-dead-beef-dead-beef00000075"
  ; "deadbeef-dead-beef-dead-beef00000075-extra-characters"
  ; "DEADBEEF-DEAD-BEEF-DEAD-BEEF00000075"
  ]

let non_uuid_strings =
  [
    ""
  ; "deadbeef"
  ; "deadbeef-dead-beef-dead-beef000000"
  ; "deadbeef-dead-beef-dead-beef0000007"
  ; "deadbeef-dead-beef-dead-beeffoobar75"
  ]

let uuid_arrays =
  [
    [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15|]
  ; (* bytes after the 16th are ignored *)
    [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16|]
  ; [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17|]
  ]

let non_uuid_arrays =
  [[|0|]; [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14|]]

let uuid_v4_cases =
  [
    ((0L, 0L), "00000000-0000-4000-8000-000000000000")
  ; ((-1L, -1L), "ffffffff-ffff-4fff-bfff-ffffffffffff")
  ; ((0x89ab_cdefL, 0x0123_4567L), "00000000-89ab-4def-8000-000001234567")
  ]

let uuid_v7_times =
  let of_ms ms = Ptime.Span.of_float_s (ms /. 1000.0) |> Option.get in
  let power_of_2_ms n =
    Float.pow 2.0 (Float.of_int n) |> of_ms |> Ptime.Span.truncate ~frac_s:3
  in
  let zero = of_ms 0.0 in
  let ms = of_ms 1.0 in
  let ps = Ptime.Span.of_d_ps (0, 1L) |> Option.get in
  (* Using RFC9562 "method 3" for representiong sub-millisecond fractions,
     that smallest amount of time a v7 UUID can represent is 1 / 4096 ms,
     which is (just less than) 244141 picoseconds *)
  let tick = Ptime.Span.of_d_ps (0, 244141L) |> Option.get in
  let ( - ) = Ptime.Span.sub in
  let to_d_ps = Ptime.Span.to_d_ps in
  [
    (zero |> to_d_ps, "00000000-0000-7000-8000-000000000000")
  ; (tick |> to_d_ps, "00000000-0000-7001-8000-000000000000")
  ; (ms |> to_d_ps, "00000000-0001-7000-8000-000000000000")
  ; (ms - ps |> to_d_ps, "00000000-0000-7fff-8000-000000000000")
    (* Test a wide range of dates - but we can't get much bigger than
       epoch + 2^47 milliseconds, and that puts us in the year 6429 and Ptime
       only allows dates up to the year 9999 *)
  ; (power_of_2_ms 05 |> to_d_ps, "00000000-0020-7000-8000-000000000000")
  ; (power_of_2_ms 15 |> to_d_ps, "00000000-8000-7000-8000-000000000000")
  ; (power_of_2_ms 25 |> to_d_ps, "00000200-0000-7000-8000-000000000000")
  ; (power_of_2_ms 35 |> to_d_ps, "00080000-0000-7000-8000-000000000000")
  ; (power_of_2_ms 45 |> to_d_ps, "20000000-0000-7000-8000-000000000000")
  ; (power_of_2_ms 47 |> to_d_ps, "80000000-0000-7000-8000-000000000000")
  ; (power_of_2_ms 47 - ps |> to_d_ps, "7fffffff-ffff-7fff-8000-000000000000")
  ]

let uuid_v7_adjustments =
  let ( * ) = Int64.mul in
  let ( + ) = Int64.add in
  let ( / ) = Int64.div in
  let ps_in_ns = 1_000L in
  let ns_in_ms = 1_000_000L in
  let ps_in_ms = ps_in_ns * ns_in_ms in
  let ns_in_day = ns_in_ms * 1000L * 60L * 60L * 24L in
  [
    (0L, (0, 0L))
  ; (ns_in_ms / 2L, (0, ps_in_ms / 2L))
  ; (ns_in_ms, (0, ps_in_ms))
  ; (ns_in_day, (1, 0L))
  ; (ns_in_day + (ns_in_ms / 4L), (1, ps_in_ms / 4L))
  ]

let uuid_v7_bytes =
  [
    (1L, "00000000-0000-7000-8000-000000000001")
  ; (-1L, "00000000-0000-7000-bfff-ffffffffffff")
  ; (0x1234_5678_9abc_def0L, "00000000-0000-7000-9234-56789abcdef0")
  ]

type resource

let uuid_testable : (module Alcotest.TESTABLE with type t = resource Uuidx.t) =
  Alcotest.testable Uuidx.pp Uuidx.equal

let pp_array arr =
  Printf.sprintf "[|%s|]"
    (Array.to_list arr |> List.map string_of_int |> String.concat "; ")

let roundtrip_tests testing_uuid =
  let expected = Some testing_uuid in
  let test_string () =
    let result = Uuidx.(to_string testing_uuid |> of_string) in
    Alcotest.(check @@ option uuid_testable)
      "Roundtrip string conversion" expected result
  in
  let test_array () =
    let result = Uuidx.(to_int_array testing_uuid |> of_int_array) in
    Alcotest.(check @@ option uuid_testable)
      "Roundtrip array conversion" expected result
  in
  [
    ("Roundtrip string conversion", `Quick, test_string)
  ; ("Roundtrip array conversion", `Quick, test_array)
  ]

let uuid_v4_tests ((upper, lower), expected_as_string) =
  let expected =
    match Uuidx.of_string expected_as_string with
    | Some uuid ->
        uuid
    | None ->
        Alcotest.fail
          (Printf.sprintf "Couldn't convert to UUID: %s" expected_as_string)
  in
  let test () =
    let result = Uuidx.make_v4_uuid upper lower in
    Alcotest.(check @@ uuid_testable) "make UUIDv4" expected result
  in
  (expected_as_string, [("Make UUIDv4 from bytes", `Quick, test)])

let uuid_v7_time_tests (t, expected_as_string) =
  let expected =
    match Uuidx.of_string expected_as_string with
    | Some uuid ->
        uuid
    | None ->
        Alcotest.fail
          (Printf.sprintf "Couldn't convert to UUID: %s" expected_as_string)
  in
  let test () =
    let result = Uuidx.make_v7_uuid_from_parts t 0L 0L in
    Alcotest.(check @@ uuid_testable) "make UUIDv7" expected result
  in
  (expected_as_string, [("Make UUIDv7 from time", `Quick, test)])

let uuid_v7_adjustment_tests (nanos, (days, picos)) =
  let day0 = 20000 in
  let bs = 0x01234_5678_9abc_defL in
  let expected = Uuidx.make_v7_uuid_from_parts (day0 + days, picos) 0L bs in
  let expected_as_string = Uuidx.to_string expected in
  let test () =
    let result = Uuidx.make_v7_uuid_from_parts (day0, 0L) nanos bs in
    Alcotest.(check @@ uuid_testable) "make UUIDv7" expected result
  in
  (expected_as_string, [("Make UUIDv7 with nano adjustment", `Quick, test)])

let uuid_v7_bytes_tests (bs, expected_as_string) =
  let expected =
    match Uuidx.of_string expected_as_string with
    | Some uuid ->
        uuid
    | None ->
        Alcotest.fail
          (Printf.sprintf "Couldn't convert to UUID: %s" expected_as_string)
  in
  let test () =
    let result = Uuidx.make_v7_uuid_from_parts (0, 0L) 0L bs in
    Alcotest.(check @@ uuid_testable) "make UUIDv7" expected result
  in
  (expected_as_string, [("Make UUIDv7 from bytes", `Quick, test)])

let string_roundtrip_tests testing_string =
  let testing_uuid =
    match Uuidx.of_string testing_string with
    | Some uuid ->
        uuid
    | None ->
        Alcotest.fail
          (Printf.sprintf "Couldn't convert to UUID: %s" testing_string)
  in
  let test () =
    let expected = String.lowercase_ascii (String.sub testing_string 0 36) in
    let result = Uuidx.to_string testing_uuid in
    Alcotest.(check string) "Roundtrip conversion" expected result
  in
  ( testing_string
  , ("Roundtrip conversion", `Quick, test) :: roundtrip_tests testing_uuid
  )

let array_roundtrip_tests testing_array =
  let testing_uuid =
    match Uuidx.of_int_array testing_array with
    | Some uuid ->
        uuid
    | None ->
        Alcotest.fail
          (Printf.sprintf "Couldn't convert to UUID: %s" (pp_array testing_array)
          )
  in
  let test () =
    let expected = Array.init 16 (fun i -> testing_array.(i)) in
    let result = Uuidx.to_int_array testing_uuid in
    Alcotest.(check @@ array int) "Roundtrip conversion" expected result
  in
  ( pp_array testing_array
  , ("Roundtrip conversion", `Quick, test) :: roundtrip_tests testing_uuid
  )

let invalid_string_tests testing_string =
  let test () =
    Alcotest.(check @@ option uuid_testable)
      "Must not be converted to UUID" None
      (Uuidx.of_string testing_string)
  in
  (testing_string, [("Fail to convert from string", `Quick, test)])

let invalid_array_tests testing_array =
  let test () =
    Alcotest.(check @@ option uuid_testable)
      "Must not be converted to UUID" None
      (Uuidx.of_int_array testing_array)
  in
  (pp_array testing_array, [("Fail to convert from array", `Quick, test)])

let regression_tests =
  List.concat
    [
      List.map string_roundtrip_tests uuid_strings
    ; List.map array_roundtrip_tests uuid_arrays
    ; List.map invalid_string_tests non_uuid_strings
    ; List.map invalid_array_tests non_uuid_arrays
    ; List.map uuid_v4_tests uuid_v4_cases
    ; List.map uuid_v7_time_tests uuid_v7_times
    ; List.map uuid_v7_adjustment_tests uuid_v7_adjustments
    ; List.map uuid_v7_bytes_tests uuid_v7_bytes
    ]

let () = Alcotest.run "Uuid" regression_tests
