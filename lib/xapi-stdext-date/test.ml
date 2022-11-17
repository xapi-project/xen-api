open Xapi_stdext_date.Date

let check_float = Alcotest.(check @@ float 1e-2)

let check_float_neq = Alcotest.(check @@ neg @@ float 1e-2)

let check_string = Alcotest.(check string)

let check_true str = Alcotest.(check bool) str true

let dash_time_str = "2020-04-07T08:28:32Z"

let no_dash_utc_time_str = "20200407T08:28:32Z"

let tests =
  let test_of_unix_time_invertible () =
    let non_int_time = 1586245987.70200706 in
    let time = non_int_time |> Float.floor in
    check_float "to_unix_time inverts of_unix_time" time
      (time |> of_unix_time |> to_unix_time) ;
    check_true "of_unix_time inverts to_unix_time"
    @@ eq (time |> of_unix_time)
         (time |> of_unix_time |> to_unix_time |> of_unix_time)
  in
  let test_only_utc () =
    let utc = "2020-12-20T18:10:19Z" in
    let _ = of_iso8601 utc in
    (* UTC is valid *)
    let non_utc = "2020-12-20T18:10:19+02:00" in
    let exn =
      Invalid_argument
        "Xapi_stdext_date__Date.of_iso8601: 2020-12-20T18:10:19+02:00"
    in
    Alcotest.check_raises "only UTC is accepted" exn (fun () ->
        of_iso8601 non_utc |> ignore
    )
  in
  let test_ca333908 () =
    check_float "dash time and no dash time represent the same unix timestamp"
      (dash_time_str |> of_iso8601 |> to_unix_time)
      (no_dash_utc_time_str |> of_iso8601 |> to_unix_time)
  in
  let test_of_iso8601_invertible_when_no_dashes () =
    check_string "to_rfc3339 inverts of_iso8601" no_dash_utc_time_str
      (no_dash_utc_time_str |> of_iso8601 |> to_rfc3339) ;
    check_true "of_iso8601 inverts to_rfc3339"
      (eq
         (no_dash_utc_time_str |> of_iso8601)
         (no_dash_utc_time_str |> of_iso8601 |> to_rfc3339 |> of_iso8601)
      )
  in
  (* CA-338243 - breaking backwards compatibility will break XC and XRT *)
  let test_to_rfc3339_backwards_compatibility () =
    check_string "to_rfc3339 is backwards compatible" no_dash_utc_time_str
      (dash_time_str |> of_iso8601 |> to_rfc3339)
  in
  let test_localtime_string () =
    let[@warning "-8"] (Ok (t, _, _)) =
      Ptime.of_rfc3339 "2020-04-07T09:01:28Z"
    in
    let minus_2_hrs = -7200 in
    let plus_3_hrs = 10800 in
    let zero_hrs = 0 in
    check_string "can subtract 2 hours"
      (_localtime_string (Some minus_2_hrs) t)
      "20200407T07:01:28" ;
    check_string "can add 3 hours"
      (_localtime_string (Some plus_3_hrs) t)
      "20200407T12:01:28" ;
    check_string "can add None" (_localtime_string None t) "20200407T09:01:28" ;
    check_string "can add zero"
      (_localtime_string (Some zero_hrs) t)
      "20200407T09:01:28"
  in
  (* sanity check (on top of test_localtime_string) that localtime produces valid looking output *)
  let test_ca342171 () =
    (* no exception is thrown + backward compatible formatting *)
    let localtime_string = localtime () |> to_rfc3339 in
    Alcotest.(check int)
      "localtime string has correct number of chars"
      (String.length localtime_string)
      (String.length no_dash_utc_time_str - 1) ;
    Alcotest.(check bool)
      "localtime string does not contain a Z" false
      (String.contains localtime_string 'Z')
  in
  let test_xsi894 () =
    let missing_tz_no_dash = "20201210T17:19:20" in
    let missing_tz_dash = "2020-12-10T17:19:20" in
    check_string "can process missing tz no dash" missing_tz_no_dash
      (missing_tz_no_dash |> of_iso8601 |> to_rfc3339) ;
    check_string "can process missing tz with dashes, but return without dashes"
      missing_tz_no_dash
      (missing_tz_dash |> of_iso8601 |> to_rfc3339) ;
    check_float "to_unix_time assumes UTC" 1607620760.
      (missing_tz_no_dash |> of_iso8601 |> to_unix_time) ;
    let localtime' = localtime () in
    check_string "to_rfc3339 inverts of_iso8601 for localtime"
      (localtime' |> to_rfc3339)
      (localtime' |> to_rfc3339 |> of_iso8601 |> to_rfc3339)
  in
  let test_email_date (unix_timestamp, expected) =
    let formatted = of_unix_time unix_timestamp |> to_rfc822 in
    check_string "String is properly RFC-822-formatted" expected formatted
  in
  let test_email_dates () =
    let dates =
      [
        (-1221847200., "Tue, 14 Apr 1931 06:00:00 GMT")
      ; (0., "Thu, 1 Jan 1970 00:00:00 GMT")
      ; (626637180., "Thu, 9 Nov 1989 17:53:00 GMT")
      ; (2889734400., "Thu, 28 Jul 2061 00:00:00 GMT")
      ]
    in
    List.iter test_email_date dates
  in
  [
    ("test_of_unix_time_invertible", `Quick, test_of_unix_time_invertible)
  ; ("test_only_utc", `Quick, test_only_utc)
  ; ("test_ca333908", `Quick, test_ca333908)
  ; ( "test_of_iso8601_invertible_when_no_dashes"
    , `Quick
    , test_of_iso8601_invertible_when_no_dashes
    )
  ; ( "test_to_rfc3339_backwards_compatibility"
    , `Quick
    , test_to_rfc3339_backwards_compatibility
    )
  ; ("test_localtime_string", `Quick, test_localtime_string)
  ; ("test_ca342171", `Quick, test_ca342171)
  ; ("test_xsi894", `Quick, test_xsi894)
  ; ("RFC 822 formatting", `Quick, test_email_dates)
  ]

let () = Alcotest.run "Date" [("Conversions", tests)]
