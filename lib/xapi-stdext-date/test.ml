open Xapi_stdext_date.Date

let check_float = Alcotest.(check @@ float 1e-2 )
let check_float_neq = Alcotest.(check @@ neg @@ float 1e-2)
let check_string = Alcotest.(check string)
let check_true str = Alcotest.(check bool) str true
let dash_time_str = "2020-04-07T08:28:32Z"
let no_dash_utc_time_str = "20200407T08:28:32Z"

let iso8601_tests =
  let test_of_float_invertible () =
    let non_int_time = 1586245987.70200706 in
    let time = non_int_time |> Float.floor in
    check_float "to_float inverts of_float" time (time |> of_float |> to_float);
    check_true "of_float inverts to_float" @@ eq (time |> of_float) (time |> of_float |> to_float |> of_float);
  in

  let test_only_utc () =
    let utc = "2020-12-20T18:10:19Z" in
    let _ = of_string utc in (* UTC is valid *)
    let non_utc = "2020-12-20T18:10:19+02:00" in
    let exn = Invalid_argument "date.ml:of_string: 2020-12-20T18:10:19+02:00" in
    Alcotest.check_raises "only UTC is accepted" exn (fun () ->  of_string non_utc |> ignore)
  in

  let test_ca333908 () =
    check_float "dash time and no dash time have same float repr"
                (dash_time_str |> of_string |> to_float)
                (no_dash_utc_time_str |> of_string |> to_float)
  in

  let test_of_string_invertible_when_no_dashes () =
    check_string "to_string inverts of_string" no_dash_utc_time_str (no_dash_utc_time_str |> of_string |> to_string);
    check_true "of_string inverts to_string" (eq (no_dash_utc_time_str |> of_string) (no_dash_utc_time_str |> of_string |> to_string |> of_string));
  in

  (* CA-338243 - breaking backwards compatibility will break XC and XRT *)
  let test_to_string_backwards_compatibility () =
    check_string "to_string is backwards compatible" no_dash_utc_time_str
      (dash_time_str |> of_string |> to_string)
  in

  let test_localtime_string () =
    let[@warning "-8"] (Ok (t, _, _)) =
      Ptime.of_rfc3339 "2020-04-07T09:01:28Z"
    in
    let minus_2_hrs = -7200 in
    let plus_3_hrs = 10800 in
    let zero_hrs = 0 in
    check_string "can subtract 2 hours" (_localtime_string (Some minus_2_hrs) t) "20200407T07:01:28";
    check_string "can add 3 hours" (_localtime_string (Some plus_3_hrs) t) "20200407T12:01:28";
    check_string "can add None" (_localtime_string None t) "20200407T09:01:28";
    check_string "can add zero" (_localtime_string (Some zero_hrs) t) "20200407T09:01:28"
  in

  (* sanity check (on top of test_localtime_string) that localtime produces valid looking output *)
  let test_ca342171 () =
    (* no exception is thrown + backward compatible formatting *)
    let localtime_string = localtime () |> to_string in
    Alcotest.(check int) "localtime string has correct number of chars"
      (String.length localtime_string) (String.length no_dash_utc_time_str - 1);
    Alcotest.(check bool) "localtime string does not contain a Z" false (String.contains localtime_string 'Z')
  in

  [ "test_of_float_invertible", `Quick, test_of_float_invertible
  ; "test_only_utc", `Quick, test_only_utc
  ; "test_ca333908", `Quick, test_ca333908
  ; "test_of_string_invertible_when_no_dashes", `Quick, test_of_string_invertible_when_no_dashes
  ; "test_to_string_backwards_compatibility", `Quick, test_to_string_backwards_compatibility
  ; "test_localtime_string", `Quick, test_localtime_string
  ; "test_ca342171", `Quick, test_ca342171
  ]

let () = Alcotest.run "Date" [ "ISO 8601", iso8601_tests ]
