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

type t = {
  debug: bool;
  verb: bool;
  unbuffered: bool;
  path: string list;
}

let colon = Re.Str.regexp_string ":"

let make debug verb unbuffered path =
  let path = Re.Str.split colon path in
  { debug; verb; unbuffered; path }

(* Keep this in sync with OCaml's Unix.file_descr *)
let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x

let ( |> ) a b = b a

let parse_size x =
  let kib = 1024L in
  let mib = Int64.mul kib kib in
  let gib = Int64.mul mib kib in
  let tib = Int64.mul gib kib in
  let endswith suffix x =
    let suffix' = String.length suffix in
    let x' = String.length x in
    x' >= suffix' && (String.sub x (x' - suffix') suffix' = suffix) in
  let remove suffix x =
    let suffix' = String.length suffix in
    let x' = String.length x in
    String.sub x 0 (x' - suffix') in
  try
    if endswith "KiB" x then Int64.(mul kib (of_string (remove "KiB" x)))
    else if endswith "MiB" x then Int64.(mul mib (of_string (remove "MiB" x)))
    else if endswith "GiB" x then Int64.(mul gib (of_string (remove "GiB" x)))
    else if endswith "TiB" x then Int64.(mul tib (of_string (remove "TiB" x)))
    else Int64.of_string x
  with _ ->
    failwith (Printf.sprintf "Cannot parse size: %s" x)

module type Floatable = sig
  type t
  val to_float: t -> float
end

let hms secs =
    let h = secs / 3600 in
    let m = (secs mod 3600) / 60 in
    let s = secs mod 60 in
    Printf.sprintf "%02d:%02d:%02d" h m s

let size bytes =
  let open Int64 in
  let kib = 1024L in
  let mib = mul kib 1024L in
  let gib = mul mib 1024L in
  let tib = mul gib 1024L in
  if div bytes tib > 0L
  then Printf.sprintf "%Ld TiB" (div bytes tib)
  else if div bytes gib > 0L
  then Printf.sprintf "%Ld GiB" (div bytes gib)
  else if div bytes mib > 0L
  then Printf.sprintf "%Ld MiB" (div bytes mib)
  else if div bytes kib > 0L
  then Printf.sprintf "%Ld KiB" (div bytes kib)
  else Printf.sprintf "%Ld bytes" bytes

module Progress_bar(T: Floatable) = struct
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

  let eta t =
    let time_so_far = Unix.gettimeofday () -. t.start_time in
    let total_time = T.(to_float t.max_value /. (to_float t.current_value)) *. time_so_far in
    let remaining = int_of_float (total_time -. time_so_far) in
    hms remaining

  let print_bar t =
    let w = bar_width t t.current_value in
    Bytes.set t.line 1 @@ spinner.(t.spin_index);
    t.spin_index <- (t.spin_index + 1) mod (Array.length spinner);
    for i = 0 to w - 1 do
      Bytes.set t.line (prefix + i) @@ (if i = w - 1 then '>' else '#')
    done;
    let percent = Printf.sprintf "%3d" (percent t) in
    String.blit percent 0 t.line (t.width - 19) 3;
    let eta = eta t in
    String.blit eta 0 t.line (t.width - 10) (String.length eta);
    
    Printf.printf "\r%s%!" (Bytes.to_string t.line)

  let update t new_value =
    let new_value = min new_value t.max_value in
    let old_bar = bar_width t t.current_value in
    let new_bar = bar_width t new_value in
    t.current_value <- new_value;
    new_bar <> old_bar

  let average_rate t =
    let time_so_far = Unix.gettimeofday () -. t.start_time in
    T.to_float t.current_value /. time_so_far

  let summarise t =
    if not t.summarised then begin
      t.summarised <- true;
      Printf.printf "Total work done: %s\n" (size (Int64.of_float (T.to_float t.current_value)));
      Printf.printf "Total time: %s\n" (hms (int_of_float (Unix.gettimeofday () -. t.start_time)));
      Printf.printf "Average rate: %s / sec\n" (size (Int64.of_float (average_rate t)))
    end
end

let padto blank n s =
  let result = Bytes.make n blank in
  String.blit s 0 result 0 (min n (String.length s));
  Bytes.unsafe_to_string result

let print_table header rows =
  let nth xs i = try List.nth xs i with Not_found -> "" in
  let width_of_column i =
    let values = nth header i :: (List.map (fun r -> nth r i) rows) in
    let widths = List.map String.length values in
    List.fold_left max 0 widths in
  let widths = List.rev (snd(List.fold_left (fun (i, acc) _ -> (i + 1, (width_of_column i) :: acc)) (0, []) header)) in
  let print_row row =
    List.iter (fun (n, s) -> Printf.printf "%s |" (padto ' ' n s)) (List.combine widths row);
    Printf.printf "\n" in
  print_row header;
  List.iter (fun (n, _) -> Printf.printf "%s-|" (padto '-' n "")) (List.combine widths header);
  Printf.printf "\n";
  List.iter print_row rows

