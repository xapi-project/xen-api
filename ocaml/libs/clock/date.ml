(* Copyright (C) Cloud Software Group Inc.
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
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

(* iso8601 allows datetimes to not contain any timezone information.
   Unfortunately we need to maintain this information because this means that
   the timestamp cannot be converted back to a timestamp with UTC as a
   reference. When serializing timezoneless timestamps, the timezone must be
   avoided yet again. *)
type tz = int option

type t = Ptime.date * Ptime.time * tz

let utc = Some 0

let of_dt tz dt =
  let date, time = dt in
  (date, time, tz)

let to_dt (date, time, _) = (date, time)

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

let of_iso8601 x =
  let rfc3339 = best_effort_iso8601_to_rfc3339 x in
  match Ptime.of_rfc3339 rfc3339 |> Ptime.rfc3339_error_to_msg with
  | Error _ ->
      invalid_arg (Printf.sprintf "%s: %s" __FUNCTION__ x)
  | Ok (t, None, _) ->
      Ptime.to_date_time t |> of_dt None
  | Ok (t, Some tz, _) ->
      Ptime.to_date_time ~tz_offset_s:tz t |> of_dt (Some tz)

let print_tz tz_s =
  match tz_s with
  | None ->
      ""
  | Some 0 ->
      "Z"
  | Some tz ->
      let tz_sign = if tz < 0 then '-' else '+' in
      let all_tz_minutes = tz / 60 |> Int.abs in
      let tz_h = all_tz_minutes / 60 in
      let tz_min = all_tz_minutes mod 60 in
      Printf.sprintf "%c%02d:%02d" tz_sign tz_h tz_min

let to_rfc3339 ((y, mon, d), ((h, min, s), _), tz) =
  (* Must be compatible with iso8601 as well. Because some client limitations,
     the hyphens between year, month and day have to be absent
  *)
  let tz = print_tz tz in
  Printf.sprintf "%04i%02i%02iT%02i:%02i:%02i%s" y mon d h min s tz

(* Extracted from tondering.dk/claus/cal/chrweek.php#calcdow *)
let weekday ~year ~mon ~day =
  let a = (14 - mon) / 12 in
  let y = year - a in
  let m = mon + (12 * a) - 2 in
  (day + y + (y / 4) - (y / 100) + (y / 400) + (31 * m / 12)) mod 7

let to_rfc822 ((year, mon, day), ((h, min, s), _), tz) =
  let timezone = match print_tz tz with "Z" -> "GMT" | tz -> tz in
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
  let localtime = t |> Ptime.to_date_time ~tz_offset_s |> of_dt None in
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

let compare_tz a b =
  match (a, b) with
  | None, None ->
      0
  | Some a_s, Some b_s ->
      Int.compare a_s b_s
  | None, Some _ ->
      -1
  | Some _, None ->
      1

let compare ((_, _, a_z) as a) ((_, _, b_z) as b) =
  let ( <?> ) a b = if a = 0 then b else a in
  Ptime.compare (to_ptime a) (to_ptime b) <?> compare_tz a_z b_z

let eq x y = compare x y = 0
