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
open Xapi_pool_helpers

let secs_random_within_an_hour =
  PeriodicUpdateSync.seconds_random_within_an_hour ()

module TestDailyUpdateSyncDelay = struct
  let hour_configed_int = 1

  let test_not_first_run_1_sec_earlier () =
    let open Unix in
    let tm_now_1_sec_earlier =
      {
        tm_year= 123
      ; tm_mon= 0
      ; tm_mday= 1
      ; tm_hour= hour_configed_int - 1
      ; tm_min= 59
      ; tm_sec= 59
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = 1 + (24 * 60 * 60) + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.daily_update_sync_delay ~first_run:false
        ~tm_now:tm_now_1_sec_earlier ~hour_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "daily_update_sync_delay" delay_next_run (Float.to_int res)

  let test_not_first_run_on_time () =
    let open Unix in
    let tm_on_time =
      {
        tm_year= 123
      ; tm_mon= 0
      ; tm_mday= 1
      ; tm_hour= hour_configed_int
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = (24 * 60 * 60) + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.daily_update_sync_delay ~first_run:false
        ~tm_now:tm_on_time ~hour_configed_int ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "daily_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_1_sec_earlier () =
    let open Unix in
    let tm_1_sec_earlier =
      {
        tm_year= 123
      ; tm_mon= 0
      ; tm_mday= 1
      ; tm_hour= hour_configed_int - 1
      ; tm_min= 59
      ; tm_sec= 59
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = 1 + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.daily_update_sync_delay ~first_run:true
        ~tm_now:tm_1_sec_earlier ~hour_configed_int ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "daily_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_on_time () =
    let open Unix in
    let tm_on_time =
      {
        tm_year= 123
      ; tm_mon= 0
      ; tm_mday= 1
      ; tm_hour= hour_configed_int
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = (24 * 60 * 60) + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.daily_update_sync_delay ~first_run:true
        ~tm_now:tm_on_time ~hour_configed_int ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "daily_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_later () =
    let open Unix in
    let hours_later = 2 in
    let min_now = 2 in
    let sec_now = 3 in
    let tm_now_later =
      {
        tm_year= 123
      ; tm_mon= 0
      ; tm_mday= 1
      ; tm_hour= hour_configed_int + hours_later
      ; tm_min= min_now
      ; tm_sec= sec_now
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run =
      (24 * 60 * 60)
      - (hours_later * 60 * 60)
      - (min_now * 60)
      - sec_now
      + secs_random_within_an_hour
    in
    let res =
      PeriodicUpdateSync.daily_update_sync_delay ~first_run:true
        ~tm_now:tm_now_later ~hour_configed_int ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "daily_update_sync_delay" delay_next_run (Float.to_int res)

  let test =
    [
      ( "not first run, 1 second earlier"
      , `Quick
      , test_not_first_run_1_sec_earlier
      )
    ; ("not first run, on time", `Quick, test_not_first_run_on_time)
    ; ("first run, 1 second earlier", `Quick, test_first_run_1_sec_earlier)
    ; ("first_run, on time", `Quick, test_first_run_on_time)
    ; ("first_run, later", `Quick, test_first_run_later)
    ]
end

module TestWeeklyUpdateSyncDelay = struct
  let year_run_test = 2023 - 1900

  (* April *)
  let month_run_test = 3

  let mday_run_test = 6

  (* April 6th 2023 is Thursday *)
  let wday_run_test = 4

  (* Unix.tm_wday: 0..6, while day_configed_int: 1..7 *)
  let day_configed_int = wday_run_test + 1

  let day_configed_int_minus = wday_run_test - 7

  let hour_configed_int = 1

  let test_not_first_run_1_sec_earlier () =
    let open Unix in
    let tm_1_sec_earlier =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_configed_int - 1
      ; tm_min= 59
      ; tm_sec= 59
      ; tm_wday= wday_run_test
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = 1 + (7 * 24 * 60 * 60) + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.weekly_update_sync_delay ~first_run:false
        ~tm_now:tm_1_sec_earlier ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "weekly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_not_first_run_on_time () =
    let open Unix in
    let tm_on_time =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_configed_int
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= wday_run_test
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = (7 * 24 * 60 * 60) + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.weekly_update_sync_delay ~first_run:false
        ~tm_now:tm_on_time ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "weekly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_1_sec_earlier () =
    let open Unix in
    let tm_1_sec_earlier =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_configed_int - 1
      ; tm_min= 59
      ; tm_sec= 59
      ; tm_wday= wday_run_test
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = 1 + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.weekly_update_sync_delay ~first_run:true
        ~tm_now:tm_1_sec_earlier ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "weekly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_1_day_earlier () =
    let open Unix in
    let tm_1_day_earlier =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test - 1
      ; tm_hour= hour_configed_int
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= wday_run_test - 1
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = (24 * 60 * 60) + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.weekly_update_sync_delay ~first_run:true
        ~tm_now:tm_1_day_earlier ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "weekly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_on_time () =
    let open Unix in
    let tm_on_time =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_configed_int
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= wday_run_test
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run = (7 * 24 * 60 * 60) + secs_random_within_an_hour in
    let res =
      PeriodicUpdateSync.weekly_update_sync_delay ~first_run:true
        ~tm_now:tm_on_time ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "weekly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_1_hour_later () =
    let open Unix in
    let tm_1_hour_later =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_configed_int + 1
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= wday_run_test
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run =
      (7 * 24 * 60 * 60) - (60 * 60) + secs_random_within_an_hour
    in
    let res =
      PeriodicUpdateSync.weekly_update_sync_delay ~first_run:true
        ~tm_now:tm_1_hour_later ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "weekly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_1_day_later () =
    let open Unix in
    let tm_1_day_later =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test + 1
      ; tm_hour= hour_configed_int
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= wday_run_test + 1
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let delay_next_run =
      (7 * 24 * 60 * 60) - (24 * 60 * 60) + secs_random_within_an_hour
    in
    let res =
      PeriodicUpdateSync.weekly_update_sync_delay ~first_run:true
        ~tm_now:tm_1_day_later ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "weekly_update_sync_delay" delay_next_run (Float.to_int res)

  let test =
    [
      ( "not first run, 1 second earlier"
      , `Quick
      , test_not_first_run_1_sec_earlier
      )
    ; ("not first run, on time", `Quick, test_not_first_run_on_time)
    ; ("first run, 1 second earlier", `Quick, test_first_run_1_sec_earlier)
    ; ("first run, 1 day earlier", `Quick, test_first_run_1_day_earlier)
    ; ("first run, on time", `Quick, test_first_run_on_time)
    ; ("first run, 1 hour later", `Quick, test_first_run_1_hour_later)
    ; ("first run, 1 day later", `Quick, test_first_run_1_day_later)
    ]
end

module TestMonthlyUpdateSyncDelay = struct
  let test_not_first_run_1_sec_earlier () =
    let year_run_test = 2023 - 1900 in
    (* April *)
    let month_run_test = 3 in
    let mday_run_test = 7 in
    let day_configed_int = mday_run_test in
    let hour_configed_int = 1 in
    let num_of_days_of_april = 30 in
    let delay_next_run =
      1 + (num_of_days_of_april * 24 * 60 * 60) + secs_random_within_an_hour
    in

    let open Unix in
    let tm_1_sec_earlier =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_configed_int - 1
      ; tm_min= 59
      ; tm_sec= 59
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:false
        ~tm_now:tm_1_sec_earlier ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in

    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_not_first_run_on_time () =
    let year_run_test = 2023 - 1900 in
    (* April *)
    let month_run_test = 3 in
    let mday_run_test = 7 in
    let day_configed_int = mday_run_test in
    let hour_configed_int = 1 in
    let num_of_days_of_april = 30 in
    let delay_next_run =
      (num_of_days_of_april * 24 * 60 * 60) + secs_random_within_an_hour
    in

    let open Unix in
    let tm_on_time =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_configed_int
      ; tm_min= 0
      ; tm_sec= 0
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:false
        ~tm_now:tm_on_time ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in

    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_trigger_next_month () =
    let year_run_test = 2023 - 1900 in
    (* April *)
    let month_run_test = 3 in
    let mday_run_test = 7 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = 1 in
    let hour_configed_int = 1 in
    let num_of_days_of_april = 30 in
    let delay_next_run =
      ((num_of_days_of_april - mday_run_test + 1) * 24 * 60 * 60)
      - (hour_run_test * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + ((day_configed_int - 1) * 24 * 60 * 60)
      + (hour_configed_int * 60 * 60)
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_leap_month_trigger_next_month () =
    let year_run_test = 2024 - 1900 in
    (* Feb *)
    let month_run_test = 1 in
    let mday_run_test = 7 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = 1 in
    let hour_configed_int = 1 in
    let num_of_days_of_leap_feb = 29 in
    let delay_next_run =
      ((num_of_days_of_leap_feb - mday_run_test + 1) * 24 * 60 * 60)
      - (hour_run_test * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + ((day_configed_int - 1) * 24 * 60 * 60)
      + (hour_configed_int * 60 * 60)
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_leap_month_minus_day_trigger_this_month () =
    let year_run_test = 2024 - 1900 in
    (* Feb *)
    let month_run_test = 1 in
    let mday_run_test = 7 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = -1 in
    let hour_configed_int = 1 in
    let num_of_days_of_leap_feb = 29 in
    let delay_next_run =
      ((num_of_days_of_leap_feb - mday_run_test + 1) * 24 * 60 * 60)
      - (hour_run_test * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + (day_configed_int * 24 * 60 * 60)
      + (hour_configed_int * 60 * 60)
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_trigger_this_month () =
    let year_run_test = 2023 - 1900 in
    (* April *)
    let month_run_test = 3 in
    let mday_run_test = 7 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = 7 in
    let hour_configed_int = 12 in
    let delay_next_run =
      ((day_configed_int - mday_run_test) * 24 * 60 * 60)
      + ((hour_configed_int - hour_run_test) * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_minus_day_trigger_this_month () =
    let year_run_test = 2023 - 1900 in
    (* April *)
    let month_run_test = 3 in
    let mday_run_test = 7 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = -1 in
    let hour_configed_int = 1 in
    let num_of_days_of_april = 30 in
    let delay_next_run =
      ((num_of_days_of_april - mday_run_test + 1) * 24 * 60 * 60)
      - (hour_run_test * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + (day_configed_int * 24 * 60 * 60)
      + (hour_configed_int * 60 * 60)
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in

    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_minus_day_trigger_next_month () =
    let year_run_test = 2023 - 1900 in
    (* April *)
    let month_run_test = 3 in
    let mday_run_test = 30 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = -1 in
    let hour_configed_int = 1 in
    let num_of_days_of_april = 30 in
    let num_of_days_of_may = 31 in
    let delay_next_run =
      ((num_of_days_of_april - mday_run_test + 1) * 24 * 60 * 60)
      - (hour_run_test * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + ((num_of_days_of_may + day_configed_int) * 24 * 60 * 60)
      + (hour_configed_int * 60 * 60)
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in

    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_trigger_next_year () =
    let year_run_test = 2023 - 1900 in
    let month_run_test = 11 in
    (* Dec *)
    let mday_run_test = 7 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = 1 in
    let hour_configed_int = 1 in
    let num_of_days_of_dec = 31 in
    let delay_next_run =
      ((num_of_days_of_dec - mday_run_test + 1) * 24 * 60 * 60)
      - (hour_run_test * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + ((day_configed_int - 1) * 24 * 60 * 60)
      + (hour_configed_int * 60 * 60)
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test_first_run_minus_day_trigger_next_year () =
    let year_run_test = 2023 - 1900 in
    let month_run_test = 11 in
    (* Dec *)
    let mday_run_test = 30 in
    let hour_run_test = 11 in
    let min_run_test = 30 in
    let sec_run_test = 6 in
    let day_configed_int = -2 in
    let hour_configed_int = 1 in
    let num_of_days_of_dec = 31 in
    let num_of_days_of_jan = 31 in
    let delay_next_run =
      ((num_of_days_of_dec - mday_run_test + 1) * 24 * 60 * 60)
      - (hour_run_test * 60 * 60)
      - (min_run_test * 60)
      - sec_run_test
      + ((num_of_days_of_jan + day_configed_int) * 24 * 60 * 60)
      + (hour_configed_int * 60 * 60)
      + secs_random_within_an_hour
    in

    let open Unix in
    let tm_run =
      {
        tm_year= year_run_test
      ; tm_mon= month_run_test
      ; tm_mday= mday_run_test
      ; tm_hour= hour_run_test
      ; tm_min= min_run_test
      ; tm_sec= sec_run_test
      ; tm_wday= 0
      ; tm_yday= 0
      ; tm_isdst= false
      }
    in
    let res =
      PeriodicUpdateSync.monthly_update_sync_delay ~first_run:true
        ~tm_now:tm_run ~hour_configed_int ~day_configed_int
        ~secs_random_within_an_hour
    in
    Alcotest.(check int)
      "monthly_update_sync_delay" delay_next_run (Float.to_int res)

  let test =
    [
      ( "not first run, 1 second earlier"
      , `Quick
      , test_not_first_run_1_sec_earlier
      )
    ; ("not first run, on time", `Quick, test_not_first_run_on_time)
    ; ( "first run, first trigger next month"
      , `Quick
      , test_first_run_trigger_next_month
      )
    ; ( "first run on leap month, first trigger next month"
      , `Quick
      , test_first_run_leap_month_trigger_next_month
      )
    ; ( "first run on leap month, minus day, first trigger this month"
      , `Quick
      , test_first_run_leap_month_minus_day_trigger_this_month
      )
    ; ( "first run, first trigger this month"
      , `Quick
      , test_first_run_trigger_this_month
      )
    ; ( "first run, minus day, first trigger this month"
      , `Quick
      , test_first_run_minus_day_trigger_this_month
      )
    ; ( "first run, minus day, first trigger next month"
      , `Quick
      , test_first_run_minus_day_trigger_next_month
      )
    ; ( "first run on Dec, first trigger next Jan"
      , `Quick
      , test_first_run_trigger_next_year
      )
    ; ( "first run on Dec, minus day, first trigger next Jan"
      , `Quick
      , test_first_run_minus_day_trigger_next_year
      )
    ]
end

let tests =
  make_suite "pool_helpers_"
    [
      ("daily_update_sync_delay", TestDailyUpdateSyncDelay.test)
    ; ("weekly_update_sync_delay", TestWeeklyUpdateSyncDelay.test)
    ; ("monthly_update_sync_delay", TestMonthlyUpdateSyncDelay.test)
    ]

let () = Alcotest.run "Pool Helpers" tests
