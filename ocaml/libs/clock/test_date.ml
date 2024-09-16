open Clock.Date

let check_float = Alcotest.(check @@ float 1e-2)

let check_string = Alcotest.(check string)

let check_true str = Alcotest.(check bool) str true

let dash_time_str = "2020-04-07T08:28:32Z"

let no_dash_utc_time_str = "20200407T08:28:32Z"

let best_effort_iso8601_to_rfc3339 x =
  let x =
    try
      Scanf.sscanf x "%04d%02d%02dT%s" (fun y mon d rest ->
          Printf.sprintf "%04d-%02d-%02dT%s" y mon d rest
      )
    with _ -> x
  in
  let tz =
    try
      Scanf.sscanf x "%04d-%02d-%02dT%02d:%02d:%02d%s" (fun _ _ _ _ _ _ tz ->
          Some tz
      )
    with _ -> None
  in
  match tz with
  | None | Some "" ->
      (* the caller didn't specify a tz, use the Unqualified Local Time *)
      Printf.sprintf "%s-00:00" x
  | Some _ ->
      x

let tests =
  let test_of_unix_time_invertible () =
    let non_int_time = 1586245987.70200706 in
    let time = non_int_time |> Float.floor in
    check_float "to_unix_time inverts of_unix_time" time
      (time |> of_unix_time |> to_unix_time) ;
    check_true "of_unix_time inverts to_unix_time"
    @@ equal (time |> of_unix_time)
         (time |> of_unix_time |> to_unix_time |> of_unix_time)
  in
  let test_iso8601 () =
    let utc = "2020-12-20T18:10:19Z" in
    let _ = of_iso8601 utc in
    (* UTC is valid *)
    let non_utc = "2020-12-20T18:10:19+02:00" in
    let _ = of_iso8601 non_utc in
    ()
  in
  let test_roundtrip_conversion () =
    let non_utc = ["20201220T18:10:19+02:00"; "20201220T18:10:19-08:45"] in
    let test spec =
      let result = spec |> of_iso8601 |> to_rfc3339 in
      Alcotest.(check string) "Roundtrip conversion be consistent" spec result
    in
    List.iter test non_utc
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
      (equal
         (no_dash_utc_time_str |> of_iso8601)
         (no_dash_utc_time_str |> of_iso8601 |> to_rfc3339 |> of_iso8601)
      )
  in
  (* CA-338243 - breaking backwards compatibility will break XC and XRT *)
  let test_to_rfc3339_backwards_compatibility () =
    check_string "to_rfc3339 is backwards compatible" no_dash_utc_time_str
      (dash_time_str |> of_iso8601 |> to_rfc3339)
  in
  let test_localtime () =
    let time = localtime () in
    match
      time
      |> to_rfc3339
      |> best_effort_iso8601_to_rfc3339
      |> Ptime.of_rfc3339
      |> Ptime.rfc3339_error_to_msg
    with
    | Ok (_, tz, _) ->
        Alcotest.(check @@ option int)
          "localtime generates a timestamp without timezone" None tz
    | Error (`Msg msg) ->
        Alcotest.failf "Unexpected error: %s" msg
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
    let canonical = "20201210T17:19:20Z" in
    let missing_tz_no_dash = "20201210T17:19:20" in
    let missing_tz_dash = "2020-12-10T17:19:20" in
    check_string
      "Timestamp without timezones nor dashes is accepted, gets converted to \
       UTC"
      canonical
      (missing_tz_no_dash |> of_iso8601 |> to_rfc3339) ;
    check_string
      "Timestamp without timezones, and dashes is accepted, gets converted to \
       UTC"
      canonical
      (missing_tz_dash |> of_iso8601 |> to_rfc3339)
  in
  let test_email_date (unix_timestamp, expected) =
    let formatted = of_unix_time unix_timestamp |> to_rfc822 in
    check_string "String is properly RFC-822-formatted" expected formatted
  in
  let test_no_timezone_to_unix () =
    (* this is allowed, but it will print a warning to stdout *)
    let missing_tz_no_dash = "20201210T17:19:20" in
    let with_tz_no_dash = "20201210T17:19:20Z" in
    let to_unix_time dt = dt |> of_iso8601 |> to_unix_time in
    check_float "Datetime without timezone assumes it's in UTC"
      (to_unix_time with_tz_no_dash)
      (to_unix_time missing_tz_no_dash)
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
  ; ("test_only_utc", `Quick, test_iso8601)
  ; ("Roundtrip conversion", `Quick, test_roundtrip_conversion)
  ; ("test_ca333908", `Quick, test_ca333908)
  ; ( "test_of_iso8601_invertible_when_no_dashes"
    , `Quick
    , test_of_iso8601_invertible_when_no_dashes
    )
  ; ( "test_to_rfc3339_backwards_compatibility"
    , `Quick
    , test_to_rfc3339_backwards_compatibility
    )
  ; ("localtime is printed without timezone", `Quick, test_localtime)
  ; ("test_localtime_string", `Quick, test_localtime_string)
  ; ("test_ca342171", `Quick, test_ca342171)
  ; ("Parsing datetimes without timezones", `Quick, test_xsi894)
  ; ( "Date w/o timezone to POSIX time conversion"
    , `Quick
    , test_no_timezone_to_unix
    )
  ; ("RFC 822 formatting", `Quick, test_email_dates)
  ]

let () = Alcotest.run "Date" [("Conversions", tests)]
