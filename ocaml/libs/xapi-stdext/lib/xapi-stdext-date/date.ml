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

let months =
  [|
     "Jan"
   ; "Feb"
   ; "Mar"
   ; "Apr"
   ; "May"
   ; "Jun"
   ; "Jul"
   ; "Aug"
   ; "Sep"
   ; "Oct"
   ; "Nov"
   ; "Dec"
  |]

let days = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]

type print_timezone = Empty | TZ of string

(* we must store the print_type with iso8601 to handle the case where the local time zone is UTC *)
type t = Ptime.date * Ptime.time * print_timezone

let utc = TZ "Z"

let of_dt print_type dt =
  let date, time = dt in
  (date, time, print_type)

let to_dt (date, time, _) = (date, time)

let best_effort_iso8601_to_rfc3339 x =
  (* (a) add dashes
   * (b) add UTC tz if no tz provided *)
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
      (* the caller didn't specify a tz. we must try to add one so that ptime can at least attempt to parse *)
      (Printf.sprintf "%sZ" x, Empty)
  | Some tz ->
      (x, TZ tz)

let of_iso8601 x =
  let rfc3339, print_timezone = best_effort_iso8601_to_rfc3339 x in
  match Ptime.of_rfc3339 rfc3339 |> Ptime.rfc3339_error_to_msg with
  | Error _ ->
      invalid_arg (Printf.sprintf "%s: %s" __FUNCTION__ x)
  | Ok (t, tz, _) -> (
    match tz with
    | None | Some 0 ->
        Ptime.to_date_time t |> of_dt print_timezone
    | Some _ ->
        invalid_arg (Printf.sprintf "%s: %s" __FUNCTION__ x)
  )

let to_rfc3339 ((y, mon, d), ((h, min, s), _), print_type) =
  match print_type with
  | TZ tz ->
      Printf.sprintf "%04i%02i%02iT%02i:%02i:%02i%s" y mon d h min s tz
  | Empty ->
      Printf.sprintf "%04i%02i%02iT%02i:%02i:%02i" y mon d h min s

let weekday ~year ~mon ~day =
  let a = (14 - mon) / 12 in
  let y = year - a in
  let m = mon + (12 * a) - 2 in
  (day + y + (y / 4) - (y / 100) + (y / 400) + (31 * m / 12)) mod 7

let to_rfc822 ((year, mon, day), ((h, min, s), _), print_type) =
  let timezone =
    match print_type with Empty | TZ "Z" -> "GMT" | TZ tz -> tz
  in
  let weekday = weekday ~year ~mon ~day in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d %s" days.(weekday) day
    months.(mon - 1)
    year h min s timezone

let to_ptime_t t =
  match to_dt t |> Ptime.of_date_time with
  | Some t ->
      t
  | None ->
      let _, (_, offset), _ = t in
      invalid_arg
        (Printf.sprintf "%s: dt='%s', offset='%i' is invalid" __FUNCTION__
           (to_rfc3339 t) offset
        )

let to_ptime = to_ptime_t

let of_ptime t = Ptime.to_date_time t |> of_dt utc

let of_unix_time s =
  match Ptime.of_float_s s with
  | None ->
      invalid_arg (Printf.sprintf "%s: %f" __FUNCTION__ s)
  | Some t ->
      of_ptime t

let to_unix_time t = to_ptime_t t |> Ptime.to_float_s

let _localtime current_tz_offset t =
  let tz_offset_s = current_tz_offset |> Option.value ~default:0 in
  let localtime = t |> Ptime.to_date_time ~tz_offset_s |> of_dt Empty in
  let _, (_, localtime_offset), _ = localtime in
  if localtime_offset <> tz_offset_s then
    invalid_arg
      (Printf.sprintf "%s: offsets don't match. offset='%i', t='%s'"
         __FUNCTION__ tz_offset_s (Ptime.to_rfc3339 t)
      ) ;
  localtime

let _localtime_string current_tz_offset t =
  _localtime current_tz_offset t |> to_rfc3339

let localtime () =
  _localtime (Ptime_clock.current_tz_offset_s ()) (Ptime_clock.now ())

let now () = of_ptime (Ptime_clock.now ())

let epoch = of_ptime Ptime.epoch

let is_earlier ~than t = Ptime.is_earlier ~than:(to_ptime than) (to_ptime t)

let is_later ~than t = Ptime.is_later ~than:(to_ptime than) (to_ptime t)

let diff a b = Ptime.diff (to_ptime a) (to_ptime b)

let compare_print_tz a b =
  match (a, b) with
  | Empty, Empty ->
      0
  | TZ a_s, TZ b_s ->
      String.compare a_s b_s
  | Empty, TZ _ ->
      -1
  | TZ _, Empty ->
      1

let compare ((_, _, a_z) as a) ((_, _, b_z) as b) =
  let ( <?> ) a b = if a = 0 then b else a in
  Ptime.compare (to_ptime a) (to_ptime b) <?> compare_print_tz a_z b_z

let eq x y = compare x y = 0

let never = epoch

let of_string = of_iso8601

let to_string = to_rfc3339

let of_float = of_unix_time

let to_float = to_unix_time

let rfc822_of_float = of_unix_time

let rfc822_to_string = to_rfc822

type iso8601 = t

type rfc822 = t
