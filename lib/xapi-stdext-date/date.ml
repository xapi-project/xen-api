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

(* ==== RFC822 ==== *)
type rfc822 = string

let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let rfc822_of_float x =
  let time = Unix.gmtime x in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT"
    days.(time.Unix.tm_wday) time.Unix.tm_mday
    months.(time.Unix.tm_mon) (time.Unix.tm_year+1900)
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let rfc822_to_string x = x

(* ==== ISO8601/RFC3339 ==== *)

type print_timezone = Empty | TZ of string
(* we must store the print_type with iso8601 to handle the case where the local time zone is UTC *)
type iso8601 = Ptime.date * Ptime.time * print_timezone

let utc = TZ "Z"

let of_dt print_type dt = let (date, time) = dt in (date, time, print_type)
let to_dt (date, time, _) = (date, time)

let best_effort_iso8601_to_rfc3339 x =
  (* (a) add dashes
   * (b) add UTC tz if no tz provided *)
  let x =
    try
      Scanf.sscanf x "%04d%02d%02dT%s"
        (fun y mon d rest ->
          Printf.sprintf "%04d-%02d-%02dT%s" y mon d rest)
    with _ ->
      x
  in
  let tz =
    try
      Scanf.sscanf x "%04d-%02d-%02dT%02d:%02d:%02d%s"
        (fun _ _ _ _ _ _ tz -> Some tz)
    with _ -> None
  in
  match tz with
  | None | Some "" ->
    (* the caller didn't specify a tz. we must try to add one so that ptime can at least attempt to parse *)
    (Printf.sprintf "%sZ" x, Empty)
  | Some tz  ->
    (x, TZ tz)

let of_string x =
  let (rfc3339, print_timezone) = best_effort_iso8601_to_rfc3339 x in
  match Ptime.of_rfc3339 rfc3339 |> Ptime.rfc3339_error_to_msg with
  | Error (`Msg e) -> invalid_arg (Printf.sprintf "date.ml:of_string: %s" x)
  | Ok (t, tz, _)  -> match tz with
                      | None | Some 0 -> Ptime.to_date_time t |> of_dt print_timezone
                      | Some _        -> invalid_arg (Printf.sprintf "date.ml:of_string: %s" x)

let to_string ((y,mon,d), ((h,min,s), _), print_type) =
  match print_type with
  | TZ tz -> Printf.sprintf "%04i%02i%02iT%02i:%02i:%02i%s" y mon d h min s tz
  | Empty -> Printf.sprintf "%04i%02i%02iT%02i:%02i:%02i" y mon d h min s

let to_ptime_t t =
  match to_dt t |> Ptime.of_date_time with
  | Some t -> t
  | None ->
    let (_, (_, offset), _) = t in
    invalid_arg (Printf.sprintf "date.ml:to_t: dt='%s', offset='%i' is invalid" (to_string t) offset)

let of_float s =
  match Ptime.of_float_s s with
  | None -> invalid_arg (Printf.sprintf "date.ml:of_float: %f" s)
  | Some t -> Ptime.to_date_time t |> of_dt utc

let to_float t = to_ptime_t t |> Ptime.to_float_s

let _localtime current_tz_offset t =
  let tz_offset_s = current_tz_offset |> Option.value ~default:0 in
  let localtime = t |> Ptime.to_date_time ~tz_offset_s |> of_dt Empty in
  let (_, (_, localtime_offset), _) = localtime in
  if localtime_offset <> tz_offset_s then
    invalid_arg (
      Printf.sprintf "date.ml:_localtime: offsets don't match. offset='%i', t='%s'"
        tz_offset_s
        (Ptime.to_rfc3339 t)
    );
  localtime

let _localtime_string current_tz_offset t =
  _localtime current_tz_offset t |> to_string

let localtime () =
  _localtime (Ptime_clock.current_tz_offset_s ()) (Ptime_clock.now ())

let assert_utc _ = ()

let never = of_float 0.0

let eq x y = x = y
