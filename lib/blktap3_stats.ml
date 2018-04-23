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

(*
 * This module extracts the external declaration to make things easier
 * for a library which might be used elsewhere.
*)

(** Define an equivalent blktap3 statistics record *)
(* See https://github.com/xapi-project/blktap/blob/master/drivers/tapdisk-metrics-stats.h for the definition of this struct as used by tapdisk *)

type t = Cstruct.t

let flag_low_mem_mode = 0x1L

[%%cstruct
  type stats = {
    version : uint32_t;
    __pad : uint32_t;
    oo_reqs : uint64_t;
    read_reqs_submitted: uint64_t;
    read_reqs_completed: uint64_t;
    read_sectors: uint64_t;
    read_total_ticks: uint64_t;
    write_reqs_submitted: uint64_t;
    write_reqs_completed: uint64_t;
    write_sectors: uint64_t;
    write_total_ticks: uint64_t;
    io_errors: uint64_t;
    flags: uint64_t;
  } [@@little_endian]]

let of_file f =
  let fd = Unix.(openfile f [O_RDONLY] 0o000) in
  try
    let result = Unix_cstruct.of_fd fd in
    Unix.close fd;
    result
  with e ->
    Unix.close fd;
    raise e

let copy : t -> t =
  fun t ->
    let t' = Cstruct.create_unsafe sizeof_stats in
    Cstruct.blit t 0 t' 0 sizeof_stats;
    t'


