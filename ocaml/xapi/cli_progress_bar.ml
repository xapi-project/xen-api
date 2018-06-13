(*
 * Copyright (C) 2011-2013 Citrix Inc
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

module type Floatable = sig
  type t
  val to_float: t -> float
end

module Make(T: Floatable) = struct
  type t = {
    max_value: T.t;
    mutable current_value: T.t;
    width: int;
    line: bytes;
    mutable spin_index: int;
    start_time: float;
    mutable summarised: bool;
  }

  let prefix_s = "[*] "
  let prefix = String.length prefix_s
  let suffix_s = "  (   % ETA   :  :  )"
  let suffix = String.length suffix_s

  let spinner = [| '-'; '\\'; '|'; '/' |]

  let create width current_value max_value =
    let line = Bytes.make width ' ' in
    String.blit prefix_s 0 line 0 prefix;
    String.blit suffix_s 0 line (width - suffix - 1) suffix;
    let spin_index = 0 in
    let start_time = Unix.gettimeofday () in
    { max_value; current_value; width; line; spin_index; start_time; summarised = false }

  let percent t = int_of_float (T.(to_float t.current_value /. (to_float t.max_value) *. 100.))

  let bar_width t value =
    int_of_float (T.(to_float value /. (to_float t.max_value) *. (float_of_int (t.width - prefix - suffix))))

  let hms secs =
    let h = secs / 3600 in
    let m = (secs mod 3600) / 60 in
    let s = secs mod 60 in
    Printf.sprintf "%02d:%02d:%02d" h m s

  let eta t =
    let time_so_far = Unix.gettimeofday () -. t.start_time in
    let total_time = T.(to_float t.max_value /. (to_float t.current_value)) *. time_so_far in
    let remaining = int_of_float (total_time -. time_so_far) in
    hms remaining

  let string_of_bar t =
    let w = bar_width t t.current_value in
    Bytes.set t.line 1 spinner.(t.spin_index);
    t.spin_index <- (t.spin_index + 1) mod (Array.length spinner);
    for i = 0 to w - 1 do
      Bytes.set t.line (prefix + i) (if i = w - 1 then '>' else '#')
    done;
    let percent = Printf.sprintf "%3d" (percent t) in
    String.blit percent 0 t.line (t.width - 19) 3;
    let eta = eta t in
    String.blit eta 0 t.line (t.width - 10) (String.length eta);
    Bytes.to_string t.line

  let update t new_value =
    let new_value = min new_value t.max_value in
    let old_bar = bar_width t t.current_value in
    let new_bar = bar_width t new_value in
    t.current_value <- new_value;
    new_bar <> old_bar

  let summarise t =
    if not t.summarised then begin
      t.summarised <- true;
      Printf.sprintf "Total time: %s\n" (hms (int_of_float (Unix.gettimeofday () -. t.start_time)));
    end else ""
end


