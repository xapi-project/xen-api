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

type iso8601 = string
type rfc822 = string

let of_float x = 
  let time = Unix.gmtime x in
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year+1900)
    (time.Unix.tm_mon+1)
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_sec

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

(* Convert tm in localtime to calendar time, x *)
let to_float_localtime x = 
  Scanf.sscanf x "%04d%02d%02dT%02d:%02d:%02d"
    (fun y mon d h min s ->
       fst (Unix.mktime { Unix.tm_year = y - 1900;
                          tm_mon = mon - 1;
                          tm_mday = d;
                          tm_hour = h;
                          tm_min = min;
                          tm_sec = s;
                          (* These are ignored: *)
                          tm_wday = 0; tm_yday = 0; tm_isdst = true;
                        }))

(* Convert tm in UTC back into calendar time x (using offset between above
   UTC and localtime fns to determine offset between UTC and localtime, then
   correcting for this)
*)
let to_float x =
  let t = Unix.time() in
  let offset = (to_float_localtime (of_float t)) -. t in
  (to_float_localtime x) -. offset

let to_string x = x
let of_string x = x

let assert_utc x =
  try
    Scanf.sscanf x "%_[0-9]T%_[0-9]:%_[0-9]:%_[0-9]Z" ()
  with _ -> invalid_arg x

let never = of_float 0.0
