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

type time_without_tz = int * int * int

let timezones = [(0, "UTC"); (8 * 60 * 60, "UTC +8"); (-1 * 60 * 60, "UTC -1")]

let of_date_time_tz d t tz_s = Ptime.of_date_time (d, (t, tz_s)) |> Option.get

module TestDayOfNextSync = struct
  let ptime = Alcotest.testable Ptime.pp Ptime.equal

  type test_case = {
      description: string
    ; now: Ptime.date * time_without_tz
    ; frequency: frequency
    ; expected: Ptime.date * time_without_tz
  }

  let test_cases =
    [
      {
        description= "daily"
      ; now= ((2023, 5, 4), (13, 14, 15))
      ; frequency= Daily
      ; expected= ((2023, 5, 5), (0, 0, 0))
      }
    ; {
        description= "daily, end of month"
      ; now= ((2023, 5, 31), (13, 14, 15))
      ; frequency= Daily
      ; expected= ((2023, 6, 1), (0, 0, 0))
      }
    ; {
        description= "daily, end of year"
      ; now= ((2023, 12, 31), (13, 14, 15))
      ; frequency= Daily
      ; expected= ((2024, 1, 1), (0, 0, 0))
      }
    ; {
        description= "weekly, this week"
      ; now= ((2023, 5, 4), (13, 14, 15))
      ; frequency= Weekly 6
      ; expected= ((2023, 5, 6), (0, 0, 0))
      }
    ; {
        description= "weekly, different day of next week"
      ; now= ((2023, 5, 4), (13, 14, 15))
      ; frequency= Weekly 1
      ; expected= ((2023, 5, 8), (0, 0, 0))
      }
    ; {
        description= "weekly, same day of next week"
      ; now= ((2023, 5, 4), (13, 14, 15))
      ; frequency= Weekly 4
      ; expected= ((2023, 5, 11), (0, 0, 0))
      }
    ]

  let test {description; now= d_a, t_a; frequency; expected= d_b, t_b}
      tz_offset_s () =
    let expected = of_date_time_tz d_b t_b tz_offset_s in
    let now = of_date_time_tz d_a t_a tz_offset_s in
    let actual = day_of_next_sync ~now ~tz_offset_s ~frequency in
    Alcotest.check ptime description expected actual

  let tests_for_each_tz_from_test_case ({description; _} as test_case) =
    let test_from_tz (tz, tz_name) =
      let description = Printf.sprintf "%s, %s" description tz_name in
      (description, `Quick, test {test_case with description} tz)
    in
    List.map test_from_tz timezones

  let tests = List.concat_map tests_for_each_tz_from_test_case test_cases
end

module TestTimeUntilNextSync = struct
  let ptime_span = Alcotest.testable Ptime.Span.pp Ptime.Span.equal

  type test_case = {
      description: string
    ; now: Ptime.date * time_without_tz
    ; next_sync: Ptime.date * time_without_tz
    ; expected: int
  }

  let test_cases =
    [
      {
        description= "same month, more than 2 hours"
      ; now= ((2023, 5, 4), (0, 0, 1))
      ; next_sync= ((2023, 5, 6), (0, 0, 0))
      ; expected= (2 * 24 * 60 * 60) - 1
      }
    ; {
        description= "different month, more than 2 hours"
      ; now= ((2023, 4, 29), (0, 0, 1))
      ; next_sync= ((2023, 5, 4), (0, 1, 6))
      ; expected= (5 * 24 * 60 * 60) - 1 + 60 + 6
      }
    ; {
        description= "2 hours and 1 second"
      ; now= ((2023, 5, 4), (23, 0, 1))
      ; next_sync= ((2023, 5, 5), (1, 0, 2))
      ; expected= (2 * 60 * 60) + 1
      }
    ; {
        description= "2 hours"
      ; now= ((2023, 5, 4), (23, 0, 1))
      ; next_sync= ((2023, 5, 5), (1, 0, 1))
      ; expected= 2 * 60 * 60
      }
    ; {
        description= "1 hour and 59 second"
      ; now= ((2023, 5, 4), (23, 0, 1))
      ; next_sync= ((2023, 5, 5), (1, 0, 0))
      ; expected= 2 * 60 * 60
      }
    ]

  let test {description; now= d_a, t_a; next_sync= d_b, t_b; expected}
      tz_offset_s () =
    let next_sync = of_date_time_tz d_b t_b tz_offset_s in
    let now = of_date_time_tz d_a t_a tz_offset_s in
    let actual = time_until_next_sync ~now ~next_sync in
    Alcotest.check ptime_span description (Ptime.Span.of_int_s expected) actual

  let tests_for_each_tz_from_test_case ({description; _} as test_case) =
    let test_from_tz (tz, tz_name) =
      let description = Printf.sprintf "%s, %s" description tz_name in
      (description, `Quick, test {test_case with description} tz)
    in
    List.map test_from_tz timezones

  let tests = List.concat_map tests_for_each_tz_from_test_case test_cases
end

let tests =
  make_suite "pool_periodic_update_sync-"
    [
      ("day_of_next_sync", TestDayOfNextSync.tests)
    ; ("time_until_next_sync", TestTimeUntilNextSync.tests)
    ]

let () = Alcotest.run "Pool Periodic Update Sync" tests
