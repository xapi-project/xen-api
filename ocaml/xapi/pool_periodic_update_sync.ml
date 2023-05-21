(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module D = Debug.Make (struct let name = "pool_periodic_update_sync" end)

open D
open Client

let periodic_update_sync_task_name = "Periodic update synchronization"

exception UpdateSync_RetryNumExceeded of int

let seconds_random_within_a_day () =
  if Xapi_fist.disable_periodic_update_sync_sec_randomness () then
    0.
  else
    let secs_of_a_day = 24. *. 60. *. 60. in
    let secs_random = Random.float secs_of_a_day in
    secs_random

let frequency_to_str ~frequency =
  match frequency with
  | `daily ->
      "daily"
  | `monthly ->
      "monthly"
  | `weekly ->
      "weekly"

let weekday_to_int = function
  | `Sun ->
      0
  | `Mon ->
      1
  | `Tue ->
      2
  | `Wed ->
      3
  | `Thu ->
      4
  | `Fri ->
      5
  | `Sat ->
      6

let utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency
    ~day_configed_int =
  let y, m, d = fst (Ptime.to_date_time ~tz_offset_s utc_now) in
  let utc_start_of_today =
    Ptime.of_date_time ((y, m, d), ((0, 0, 0), tz_offset_s)) |> Option.get
  in

  match frequency with
  | `daily ->
      (* schedule in the next day *)
      let one_day_span = Ptime.Span.of_d_ps (1, 0L) |> Option.get in
      Ptime.add_span utc_start_of_today one_day_span |> Option.get
  | `weekly ->
      let days =
        let today_wday = Ptime.weekday ~tz_offset_s utc_now |> weekday_to_int in
        match today_wday < day_configed_int with
        | true ->
            day_configed_int - today_wday
        | false ->
            day_configed_int + 7 - today_wday
      in
      let span = Ptime.Span.of_d_ps (days, 0L) |> Option.get in
      Ptime.add_span utc_start_of_today span |> Option.get

let next_scheduled_datetime ~delay ~utc_now ~tz_offset_s =
  let span = Ptime.Span.of_float_s delay |> Option.get in
  let target = Ptime.add_span utc_now span |> Option.get in
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s target in
  (y, m, d, hh, mm, ss)

let print_next_schedule ~delay ~utc_now ~tz_offset_s =
  debug "[PeriodicUpdateSync] delay for next update sync: %f seconds" delay ;
  let y, m, d, hh, mm, ss =
    next_scheduled_datetime ~delay ~utc_now ~tz_offset_s
  in
  debug
    "[PeriodicUpdateSync] next update sync scheduled at pool time: %d-%d-%d, \
     %d:%d:%d "
    y m d hh mm ss

let update_sync_delay_for_next_schedule_internal ~utc_now
    ~utc_start_of_next_sched_day ~seconds_in_a_day =
  let random_span = Ptime.Span.of_float_s seconds_in_a_day |> Option.get in
  let utc_next_schedule =
    Ptime.add_span utc_start_of_next_sched_day random_span |> Option.get
  in
  Ptime.diff utc_next_schedule utc_now |> Ptime.Span.to_float_s

let update_sync_delay_for_next_schedule ~__context =
  let frequency =
    Db.Pool.get_update_sync_frequency ~__context
      ~self:(Helpers.get_pool ~__context)
  in
  let day_configed =
    Db.Pool.get_update_sync_day ~__context ~self:(Helpers.get_pool ~__context)
  in
  debug
    "[PeriodicUpdateSync] update_sync_delay_for_next_schedule, frequency=%s, \
     day_configed=%Ld"
    (frequency_to_str ~frequency)
    day_configed ;
  let day_configed_int = Int64.to_int day_configed in
  let utc_now = Ptime_clock.now () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
  let seconds_in_a_day = seconds_random_within_a_day () in
  let utc_start_of_next_sched_day =
    utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency
      ~day_configed_int
  in
  let delay =
    update_sync_delay_for_next_schedule_internal ~utc_now
      ~utc_start_of_next_sched_day ~seconds_in_a_day
  in
  print_next_schedule ~delay ~utc_now ~tz_offset_s ;
  delay

let rec update_sync () =
  Server_helpers.exec_with_new_task "periodic_update_sync" (fun __context ->
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          ( try
              ignore
                (Client.Pool.sync_updates ~rpc ~session_id
                   ~self:(Helpers.get_pool ~__context)
                   ~force:false ~token:"" ~token_id:""
                )
            with _ ->
              warn "Periodic update sync failed" ;
              let frequency =
                Db.Pool.get_update_sync_frequency ~__context
                  ~self:(Helpers.get_pool ~__context)
              in
              let now =
                Ptime_clock.now ()
                |> Xapi_stdext_date.Date.of_ptime
                |> Xapi_stdext_date.Date.to_string
              in
              Xapi_alert.add ~msg:Api_messages.periodic_update_sync_failed
                ~cls:`Pool
                ~obj_uuid:
                  (Db.Pool.get_uuid ~__context
                     ~self:(Helpers.get_pool ~__context)
                  )
                ~body:
                  ("<body><message>Periodic update sync("
                  ^ frequency_to_str ~frequency
                  ^ ") failed.</message><date>"
                  ^ now
                  ^ "</date></body>"
                  )
          ) ;
          Xapi_periodic_scheduler.add_to_queue periodic_update_sync_task_name
            Xapi_periodic_scheduler.OneShot
            (update_sync_delay_for_next_schedule ~__context)
            update_sync
      )
  )

let set_enabled ~__context ~value =
  debug "[PeriodicUpdateSync] set_enabled: %B" value ;
  if value then (
    Xapi_periodic_scheduler.remove_from_queue periodic_update_sync_task_name ;
    Xapi_periodic_scheduler.add_to_queue periodic_update_sync_task_name
      Xapi_periodic_scheduler.OneShot
      (update_sync_delay_for_next_schedule ~__context)
      update_sync
  ) else
    Xapi_periodic_scheduler.remove_from_queue periodic_update_sync_task_name
