(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Test_highlevel
open Pool_periodic_update_sync

module TestUpdateSyncDelay = struct
  let utc_offset_s = 0 (* UTC timezone for test *)
  let utc8_offset_s = 8 * 60 * 60 (* UTC +8 timezone for test *)
  let utc_1_offset_s = -1 * 60 * 60 (* UTC -1 timezone for test *)

  let test_day_of_next_sync_1 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res = day_of_next_sync ~now ~tz_offset_s ~frequency:Daily in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 5), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_1, daily" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_2 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 31), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res = day_of_next_sync ~now ~tz_offset_s ~frequency:Daily in
    let next_sync =
      Ptime.of_date_time ((2023, 6, 1), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_2, daily, end of month" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_3 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 12, 31), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res = day_of_next_sync ~now ~tz_offset_s ~frequency:Daily in
    let next_sync =
      Ptime.of_date_time ((2024, 1, 1), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_3, daily, end of year" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_4 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res = day_of_next_sync ~now ~tz_offset_s ~frequency:(Weekly 6) in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 6), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_4, weekly, this week" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_5 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res = day_of_next_sync ~now ~tz_offset_s ~frequency:(Weekly 1) in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 8), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_5, weekly, next week" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_6 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res = day_of_next_sync ~now ~tz_offset_s ~frequency:(Weekly 4) in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 11), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_6, weekly, next week 2" true
      (Ptime.equal res next_sync)

  let test_time_until_next_sync_1 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((0, 0, 1), tz_offset_s)) |> Option.get
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 6), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    let delay = (2. *. 24. *. 60. *. 60.) -. 1. in
    let res = time_until_next_sync ~now ~next_sync in
    Alcotest.(check (float Float.epsilon))
      "test_time_until_next_sync_1" delay
      (Ptime.Span.to_float_s res)

  let test_time_until_next_sync_2 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((23, 0, 1), tz_offset_s)) |> Option.get
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 5), ((1, 0, 2), tz_offset_s)) |> Option.get
    in
    let delay = (2. *. 60. *. 60.) +. 1. in
    let res = time_until_next_sync ~now ~next_sync in
    Alcotest.(check (float Float.epsilon))
      "test_time_until_next_sync_2" delay
      (Ptime.Span.to_float_s res)

  let test_time_until_next_sync_3 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 4, 29), ((0, 0, 1), tz_offset_s)) |> Option.get
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 4), ((0, 1, 6), tz_offset_s)) |> Option.get
    in
    let delay = (5. *. 24. *. 60. *. 60.) -. 1. +. 66. in
    let res = time_until_next_sync ~now ~next_sync in
    Alcotest.(check (float Float.epsilon))
      "test_update_sync_delay_for_next_schedule_internal_3, next month" delay
      (Ptime.Span.to_float_s res)

  let test_time_until_next_sync_4 tz_offset_s =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((23, 0, 1), tz_offset_s)) |> Option.get
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 5), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    let delay = 2. *. 60. *. 60. in
    let res = time_until_next_sync ~now ~next_sync in
    Alcotest.(check (float Float.epsilon))
      "test_time_until_next_sync_4" delay
      (Ptime.Span.to_float_s res)

  let test =
    [
      ( "day_of_next_sync daily, UTC TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_1 utc_offset_s)
      )
    ; ( "day_of_next_sync daily, UTC +8 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_1 utc8_offset_s)
      )
    ; ( "day_of_next_sync daily, UTC -1 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_1 utc_1_offset_s)
      )
    ; ( "day_of_next_sync daily, end of month, UTC TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_2 utc_offset_s)
      )
    ; ( "day_of_next_sync daily, end of month, UTC +8 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_2 utc8_offset_s)
      )
    ; ( "day_of_next_sync daily, end of month, UTC -1 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_2 utc_1_offset_s)
      )
    ; ( "day_of_next_sync daily, end of year, UTC TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_3 utc_offset_s)
      )
    ; ( "day_of_next_sync daily, end of year, UTC +8 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_3 utc8_offset_s)
      )
    ; ( "day_of_next_sync daily, end of year, UTC -1 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_3 utc_1_offset_s)
      )
    ; ( "day_of_next_sync weekly, this week, UTC TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_4 utc_offset_s)
      )
    ; ( "day_of_next_sync weekly, this week, UTC +8 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_4 utc8_offset_s)
      )
    ; ( "day_of_next_sync weekly, this week, UTC -1 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_4 utc_1_offset_s)
      )
    ; ( "day_of_next_sync weekly, next week, UTC TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_5 utc_offset_s)
      )
    ; ( "day_of_next_sync weekly, next week, UTC +8 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_5 utc8_offset_s)
      )
    ; ( "day_of_next_sync weekly, next week, UTC -1 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_5 utc_1_offset_s)
      )
    ; ( "day_of_next_sync weekly, next week, 2, UTC TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_6 utc_offset_s)
      )
    ; ( "day_of_next_sync weekly, next week, 2, UTC +8 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_6 utc8_offset_s)
      )
    ; ( "day_of_next_sync weekly, next week, 2, UTC -1 TZ"
      , `Quick
      , (fun () -> test_day_of_next_sync_6 utc_1_offset_s)
      )
    ; ( "calc_delay, more than 2 hours, UTC TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_1 utc_offset_s)
      )
    ; ( "calc_delay, more than 2 hours, UTC +8 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_1 utc8_offset_s)
      )
    ; ( "calc_delay, more than 2 hours, UTC -1 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_1 utc_1_offset_s)
      )
    ; ( "calc_delay, 2 hours and 1 sec, UTC TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_2 utc_offset_s)
      )
    ; ( "calc_delay, 2 hours and 1 sec, UTC +8 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_2 utc8_offset_s)
      )
    ; ( "calc_delay, 2 hours and 1 sec, UTC -1 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_2 utc_1_offset_s)
      )
    ; ( "calc_delay, next month, UTC TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_3 utc_offset_s)
      )
    ; ( "calc_delay, next month, UTC +8 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_3 utc8_offset_s)
      )
    ; ( "calc_delay, next month, UTC -1 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_3 utc_1_offset_s)
      )
    ; ( "calc_delay, less than 2 hours, UTC TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_4 utc_offset_s)
      )
    ; ( "calc_delay, less than 2 hours, UTC +8 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_4 utc8_offset_s)
      )
    ; ( "calc_delay, less than 2 hours, UTC -1 TZ"
      , `Quick
      , (fun () -> test_time_until_next_sync_4 utc_1_offset_s)
      )
    ]
end

let tests =
  make_suite "pool_periodic_update_sync_"
    [("periodic_update_sync", TestUpdateSyncDelay.test)]

let () = Alcotest.run "Pool Periodic Update Sync" tests
