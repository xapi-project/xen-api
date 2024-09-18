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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
open Client

type frequency = Daily | Weekly of int

let frequency_of_freq_and_day freq day =
  match (freq, Int64.to_int day) with
  | `daily, _ ->
      Daily
  | `weekly, d ->
      Weekly d

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let periodic_update_sync_task_name = "Periodic update synchronization"

let secs_per_hour = 60 * 60

let update_sync_minimum_interval = Ptime.Span.of_int_s (2 * secs_per_hour)

let secs_per_day = 24 * secs_per_hour

let random_delay () =
  if Xapi_fist.disable_periodic_update_sync_sec_randomness () then
    Ptime.Span.zero
  else
    Ptime.Span.of_int_s (Random.int secs_per_day)

let frequency_to_str ~frequency =
  match frequency with `daily -> "daily" | `weekly -> "weekly"

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

let day_of_next_sync ~now ~tz_offset_s ~frequency =
  let y, m, d = fst (Ptime.to_date_time ~tz_offset_s now) in
  let beginning_of_day =
    Ptime.of_date_time ((y, m, d), ((0, 0, 0), tz_offset_s)) |> Option.get
  in
  let delay_of d = Ptime.Span.of_d_ps (d, 0L) |> Option.get in

  let days =
    match frequency with
    | Daily ->
        1
    | Weekly configured_day ->
        let today = Ptime.weekday ~tz_offset_s now |> weekday_to_int in
        if today < configured_day then
          configured_day - today (* 1 to 6 days *)
        else
          configured_day - today + 7 (* 1 to 7 days *)
  in
  Ptime.add_span beginning_of_day (delay_of days) |> Option.get

let time_until_next_sync_internal ~now ~next_sync =
  let delay = Ptime.diff next_sync now in
  if Xapi_fist.disable_periodic_update_sync_sec_randomness () then
    delay
  else if Ptime.Span.compare delay update_sync_minimum_interval > 0 then
    delay
  else (* Enforce a minimum of 2 hours between schedules *)
    update_sync_minimum_interval

let time_until_next_sync ~now ~next_sync =
  match Xapi_fist.set_periodic_update_sync_delay () with
  | None ->
      time_until_next_sync_internal ~now ~next_sync
  | Some delay -> (
    try
      let seconds = int_of_string (String.trim delay) in
      Ptime.Span.of_int_s seconds
    with _ ->
      debug
        "[PeriodicUpdateSync] failed to interpret periodic update sync delay: \
         \"%s\""
        delay ;
      time_until_next_sync_internal ~now ~next_sync
  )

let seconds_until_next_schedule ~__context =
  let frequency =
    Db.Pool.get_update_sync_frequency ~__context
      ~self:(Helpers.get_pool ~__context)
  in
  let day_of_week =
    Db.Pool.get_update_sync_day ~__context ~self:(Helpers.get_pool ~__context)
  in
  debug
    "[PeriodicUpdateSync] seconds_until_next_schedule, frequency=%s, \
     day_configed=%Ld"
    (frequency_to_str ~frequency)
    day_of_week ;
  let frequency = frequency_of_freq_and_day frequency day_of_week in
  let now = Ptime_clock.now () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
  let delay = random_delay () in
  let next_day = day_of_next_sync ~now ~tz_offset_s ~frequency in
  let next_sync = Ptime.add_span next_day delay |> Option.get in
  let delay = time_until_next_sync ~now ~next_sync |> Ptime.Span.to_float_s in
  debug "[PeriodicUpdateSync] delay for next update sync: %f seconds" delay ;
  delay

let rec update_sync () =
  debug "[PeriodicUpdateSync] periodic update synchronization start..." ;
  Server_helpers.exec_with_new_task "periodic_update_sync" (fun __context ->
      finally
        (fun () ->
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              try
                ignore
                  (Client.Pool.sync_updates ~rpc ~session_id
                     ~self:(Helpers.get_pool ~__context)
                     ~force:false ~token:"" ~token_id:""
                  )
              with e ->
                let exc = Printexc.to_string e in
                warn "Periodic update sync failed with exception %s" exc ;
                let now = Xapi_stdext_date.Date.(now () |> to_rfc3339) in
                let body =
                  Printf.sprintf
                    "<body><message>Periodic update sync \
                     failed.</message><exception>%s</exception><date>%s</date></body>"
                    exc now
                in
                let obj_uuid =
                  Db.Pool.get_uuid ~__context ~self:(Helpers.get_pool ~__context)
                in
                Xapi_alert.add ~msg:Api_messages.periodic_update_sync_failed
                  ~cls:`Pool ~obj_uuid ~body
          )
        )
        (add_to_queue ~__context)
  )

and add_to_queue ~__context () =
  let open Xapi_periodic_scheduler in
  add_to_queue periodic_update_sync_task_name OneShot
    (seconds_until_next_schedule ~__context)
    update_sync

let set_enabled ~__context ~value =
  Xapi_periodic_scheduler.remove_from_queue periodic_update_sync_task_name ;
  if value then
    add_to_queue ~__context ()
