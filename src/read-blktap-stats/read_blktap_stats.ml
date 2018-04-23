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

let usage () =
  Printf.printf "Usage:\n";
  Printf.printf "%s <stats-file>\n" Sys.argv.(0);
  exit 1

let () =
  match Sys.argv with
  | [|_; filename|] -> begin
      let stats = Blktap3_stats.of_file filename in
      let s = Blktap3_stats.copy stats in
      let open Blktap3_stats in
      Printf.printf "read_reqs_submitted = %Ld\n" (get_stats_read_reqs_submitted s);
      Printf.printf "read_reqs_completed = %Ld\n" (get_stats_read_reqs_completed s);
      Printf.printf "read_sectors = %Ld\n" (get_stats_read_sectors s);
      Printf.printf "read_total_ticks = %Ld\n" (get_stats_read_total_ticks s);
      Printf.printf "write_reqs_submitted = %Ld\n" (get_stats_write_reqs_submitted s);
      Printf.printf "write_reqs_completed = %Ld\n" (get_stats_write_reqs_completed s);
      Printf.printf "write_sectors = %Ld\n" (get_stats_write_sectors s);
      Printf.printf "write_total_ticks = %Ld\n" (get_stats_write_total_ticks s);
      Printf.printf "io_errors = %Ld\n" (get_stats_io_errors s);
      Printf.printf "flags = %Ld\n" (get_stats_flags s);
    end
  | _ -> usage ()
