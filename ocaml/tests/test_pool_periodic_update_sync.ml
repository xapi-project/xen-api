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
  let tz_offset_s = 8 * 60 * 60 (* UTC +8 timezone for test *)

  let test_day_of_next_sync_1 () =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      day_of_next_sync ~now ~tz_offset_s ~frequency:Daily
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 5), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_1, daily" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_2 () =
    let now =
      Ptime.of_date_time ((2023, 5, 31), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      day_of_next_sync ~now ~tz_offset_s ~frequency:Daily
    in
    let next_sync =
      Ptime.of_date_time ((2023, 6, 1), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_2, daily, end of month" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_3 () =
    let now =
      Ptime.of_date_time ((2023, 12, 31), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      day_of_next_sync ~now ~tz_offset_s ~frequency:Daily
    in
    let next_sync =
      Ptime.of_date_time ((2024, 1, 1), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_3, daily, end of year" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_4 () =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      day_of_next_sync ~now ~tz_offset_s ~frequency:(Weekly 6)
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 6), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_4, weekly, this week" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_5 () =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      day_of_next_sync ~now ~tz_offset_s ~frequency:(Weekly 1)
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 8), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_5, weekly, next week" true
      (Ptime.equal res next_sync)

  let test_day_of_next_sync_6 () =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      day_of_next_sync ~now ~tz_offset_s ~frequency:(Weekly 4)
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 11), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_day_of_next_sync_6, weekly, next week 2" true
      (Ptime.equal res next_sync)

  let test_time_until_next_sync_1 () =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((0, 0, 1), tz_offset_s)) |> Option.get
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 6), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    let delay = (2. *. 24. *. 60. *. 60.) -. 1. in
    let res = time_until_next_sync ~now ~next_sync in
    Alcotest.(check (float Float.epsilon))
      "test_time_until_next_sync_1"
      delay (Ptime.Span.to_float_s res)

  let test_time_until_next_sync_2 () =
    let now =
      Ptime.of_date_time ((2023, 5, 4), ((0, 0, 1), tz_offset_s)) |> Option.get
    in
    let next_sync =
      Ptime.of_date_time ((2023, 5, 6), ((0, 1, 6), tz_offset_s)) |> Option.get
    in
    let delay = (2. *. 24. *. 60. *. 60.) -. 1. +. 66. in
    let res = time_until_next_sync ~now ~next_sync in
    Alcotest.(check (float Float.epsilon))
      "test_time_until_next_sync_2"
      delay (Ptime.Span.to_float_s res)

  let test_time_until_next_sync_3 () =
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

  let test =
    [
      ( "day_of_next_sync daily"
      , `Quick
      , test_day_of_next_sync_1
      )
    ; ( "day_of_next_sync daily, end of month"
      , `Quick
      , test_day_of_next_sync_2
      )
    ; ( "day_of_next_sync daily, end of year"
      , `Quick
      , test_day_of_next_sync_3
      )
    ; ( "day_of_next_sync weekly, this week"
      , `Quick
      , test_day_of_next_sync_4
      )
    ; ( "day_of_next_sync weekly, next week"
      , `Quick
      , test_day_of_next_sync_5
      )
    ; ( "day_of_next_sync weekly, next week, 2"
      , `Quick
      , test_day_of_next_sync_6
      )
    ; ( "calc_delay, without random delay"
      , `Quick
      , test_time_until_next_sync_1
      )
    ; ( "calc_delay, with random delay"
      , `Quick
      , test_time_until_next_sync_2
      )
    ; ( "calc_delay, next month"
      , `Quick
      , test_time_until_next_sync_3
      )
    ]
end

let tests =
  make_suite "pool_periodic_update_sync_"
    [("periodic_update_sync", TestUpdateSyncDelay.test)]

let () = Alcotest.run "Pool Periodic Update Sync" tests
