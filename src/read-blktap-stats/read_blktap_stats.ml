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
      let stats = Blktap3_stats.get_blktap3_stats filename in
      Printf.printf "read_reqs_submitted = %Ld\n" stats.Blktap3_stats.read_reqs_submitted;
      Printf.printf "read_reqs_completed = %Ld\n" stats.Blktap3_stats.read_reqs_completed;
      Printf.printf "read_sectors = %Ld\n" stats.Blktap3_stats.read_sectors;
      Printf.printf "read_total_ticks = %Ld\n" stats.Blktap3_stats.read_total_ticks;
      Printf.printf "write_reqs_submitted = %Ld\n" stats.Blktap3_stats.write_reqs_submitted;
      Printf.printf "write_reqs_completed = %Ld\n" stats.Blktap3_stats.write_reqs_completed;
      Printf.printf "write_sectors = %Ld\n" stats.Blktap3_stats.write_sectors;
      Printf.printf "write_total_ticks = %Ld\n" stats.Blktap3_stats.write_total_ticks;
      Printf.printf "io_errors = %Ld\n" stats.Blktap3_stats.io_errors;
      Printf.printf "low_mem_mode = %b\n" stats.Blktap3_stats.low_mem_mode
    end
  | _ -> usage ()
