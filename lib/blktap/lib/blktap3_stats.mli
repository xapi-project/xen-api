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
(**
 * This module defines an equivalent blktap3 stats record and the
 * associated API method.
*)

type t

val flag_low_mem_mode : int64

val get_stats_version : t -> Cstruct.uint32
val get_stats_oo_reqs : t -> Cstruct.uint64
val get_stats_read_reqs_submitted : t -> Cstruct.uint64
val get_stats_read_reqs_completed : t -> Cstruct.uint64
val get_stats_read_sectors : t -> Cstruct.uint64
val get_stats_read_total_ticks : t -> Cstruct.uint64
val get_stats_write_reqs_submitted : t -> Cstruct.uint64
val get_stats_write_reqs_completed : t -> Cstruct.uint64
val get_stats_write_sectors : t -> Cstruct.uint64
val get_stats_write_total_ticks : t -> Cstruct.uint64
val get_stats_io_errors : t -> Cstruct.uint64
val get_stats_flags : t -> Cstruct.uint64

val of_file : string -> t
val copy : t -> t
