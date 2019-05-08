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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)
(* Monitor selected processes, periodically logging stats.
 * @group Performance Monitoring
*)

module D = Debug.Make(struct let name = "rrdd_stats" end)
open D

(** Represents a subset of the data in /proc/meminfo *)
type meminfo = {
  total: int; (* KiB *)
  free: int; (* KiB *)
  buffered: int; (* KiB *)
  cached: int; (* KiB *)
  swap_total: int; (* KiB *)
  swap_free: int; (* KiB *)
}

(** Represents a subset of the data in /proc/<pid>/status *)
type process_memory_info = {
  peak: int; (* KiB *)
  size: int; (* KiB *)
  locked: int; (* KiB *)
  hwm: int; (* KiB *)
  rss: int; (* KiB *)
  data: int; (* KiB *)
  stack: int; (* KiB *)
  exe: int; (* KiB *)
  lib: int; (* KiB *)
}

let null_process_memory_info = {
  peak = 0;
  size = 0;
  locked = 0;
  hwm = 0;
  rss = 0;
  data = 0;
  stack = 0;
  exe = 0;
  lib = 0;
}

let plus_process_memory_info pmi1 pmi2 = {
  peak = pmi1.peak + pmi2.peak;
  size = pmi1.size + pmi2.size;
  locked = pmi1.locked + pmi2.locked;
  hwm = pmi1.hwm + pmi2.hwm;
  rss = pmi1.rss + pmi2.rss;
  data = pmi1.data + pmi2.data;
  stack = pmi1.stack + pmi2.stack;
  exe = pmi1.exe + pmi2.exe;
  lib = pmi1.lib + pmi2.lib;
}

(* TODO: Move this function (and its clones) to xen-api-libs. *)
let split_colon line = Astring.String.fields ~empty:false line

let meminfo () =
  let all = Xapi_stdext_unix.Unixext.string_of_file "/proc/meminfo" in
  let total = ref (-1)
  and free = ref (-1)
  and buffered = ref (-1)
  and cached = ref (-1)
  and swap_total = ref (-1)
  and swap_free = ref (-1) in
  List.iter (fun line -> match split_colon line with
      | ["MemTotal:"; x; "kB"] -> total := int_of_string x
      | ["MemFree:"; x; "kB"] -> free := int_of_string x
      | ["Buffers:"; x; "kB"] -> buffered := int_of_string x
      | ["Cached:"; x; "kB"] -> cached := int_of_string x
      | ["SwapTotal:"; x; "kB"] -> swap_total := int_of_string x
      | ["SwapFree:"; x; "kB"] -> swap_free := int_of_string x
      | _ -> ()
    ) Astring.String.(cuts ~sep:"\n" all);
  {total = !total; free = !free; buffered = !buffered;
   cached = !cached; swap_total = !swap_total; swap_free = !swap_free}

let string_of_meminfo (x : meminfo) =
  Printf.sprintf
    "MemTotal: %d KiB; MemFree: %d KiB; Buffered: %d KiB; Cached: %d KiB; SwapTotal: %d KiB; SwapFree: %d KiB"
    x.total x.free x.buffered x.cached x.swap_total x.swap_free

let process_memory_info_of_pid (pid : int) : process_memory_info =
  let all = Xapi_stdext_unix.Unixext.string_of_file (Printf.sprintf "/proc/%d/status" pid) in
  let peak = ref (-1)
  and size = ref (-1)
  and locked = ref (-1)
  and hwm = ref (-1)
  and rss = ref (-1)
  and data = ref (-1)
  and stack = ref (-1)
  and exe = ref (-1)
  and lib = ref (-1) in
  List.iter (fun line -> match split_colon line with
      | ["VmPeak:"; x; "kB"] -> peak := int_of_string x
      | ["VmSize:"; x; "kB"] -> size := int_of_string x
      | ["VmLck:"; x; "kB"] -> locked := int_of_string x
      | ["VmHWM:"; x; "kB"] -> hwm := int_of_string x
      | ["VmRSS:"; x; "kB"] -> rss := int_of_string x
      | ["VmData:"; x; "kB"] -> data := int_of_string x
      | ["VmStk:"; x; "kB"] -> stack := int_of_string x
      | ["VmExe:"; x; "kB"] -> exe := int_of_string x
      | ["VmLib:"; x; "kB"] -> lib := int_of_string x
      | _ -> ()
    ) Astring.String.(cuts ~sep:"\n" all);
  {peak = !peak; size = !size; locked = !locked; hwm = !hwm;
   rss = !rss; data = !data; stack = !stack; exe = !exe; lib = !lib}

let string_of_process_memory_info (x: process_memory_info) =
  Printf.sprintf
    "size: %d KiB; rss: %d KiB; data: %d KiB; stack: %d KiB"
    x.size x.rss x.data x.stack

(* Log the initial offset between our monotonic clock and UTC *)
let initial_offset =
  Unix.gettimeofday () -. (Int64.to_float (Mtime_clock.now_ns ()) /. 1e9)

let print_system_stats () =
  let mi = string_of_meminfo (meminfo ()) in
  debug "system stats: %s" mi;
  let current_offset = Unix.gettimeofday () -. (Int64.to_float (Mtime_clock.now_ns ()) /. 1e9) in
  debug "Clock drift: %.0f" (current_offset -. initial_offset)

(* Obtains process IDs for the specified program.
 * This should probably be moved into xen-api-libs. *)
let pidof ?(pid_dir="/var/run") program =
  try
    let out = Xapi_stdext_unix.Unixext.string_of_file (Printf.sprintf "%s/%s.pid" pid_dir program) in
    let words = Astring.String.fields ~empty:false out in
    let maybe_parse_int acc i = try (int_of_string i) :: acc with Failure _ -> acc in
    List.fold_left maybe_parse_int [] words
  with
  | Unix.Unix_error (Unix.ENOENT, _, _)
  | Unix.Unix_error (Unix.EACCES, _, _) -> []

let print_stats_for ~program =
  let pids = pidof program in
  let n = List.length pids in
  let pmis = List.map process_memory_info_of_pid pids in
  let pmi =
    List.fold_left plus_process_memory_info null_process_memory_info pmis in
  debug "%s stats (n = %d): %s" program n (string_of_process_memory_info pmi)

let last_log = ref 0.
let log_interval = 60.
let programs_to_monitor = ["xcp-rrdd"; "xapi"; "xenopsd-xc"; "xenopsd-xenlight"]

let print_stats () =
  print_system_stats ();
  List.iter (fun program -> print_stats_for ~program) programs_to_monitor

(** Called from the main monitoring loop. *)
let print_snapshot () =
  try
    (* Only run once every minute to avoid spamming the logs *)
    let now = Unix.gettimeofday () in
    if now -. !last_log > log_interval then (
      last_log := now;
      print_stats ()
    )
  with e ->
    debug "Caught: %s" (Printexc.to_string e);
    log_backtrace ()
