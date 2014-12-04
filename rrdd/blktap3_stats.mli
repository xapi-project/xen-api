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

(** Attributes associated with the blktap3 stats struct *)
type blktap3_stats = {
	(** BLKIF_OP_DISCARD, not currently supported in blktap3, zero always *)
	st_ds_req : int64;
	(** BLKIF_OP_FLUSH_DISKCACHE, not currently supported in blktap3, zero always*)
	st_f_req  : int64;
	(** Increased each time we fail to allocate memory for a internal 
	 * request descriptor in response to a ring request. *)
	st_oo_req : int64;
	(** Received BLKIF_OP_READ requests. *)
	st_rd_req : int64;
	(** Completed BLKIF_OP_READ requests. *)
	st_rd_cnt : int64;
	(** Read sectors, after we've forwarded the request to actual storage. *)
	st_rd_sect: int64;
	(** Sum of the request response time of all BLKIF_OP_READ *)
	st_rd_sum_usecs : int64;
	(** Absolute maximum BLKIF_OP_READ response time *)
	st_rd_max_usecs : int64;
	(** Received BLKIF_OP_WRITE requests. *)
	st_wr_req : int64;
	(** Completed BLKIF_OP_WRITE requests. *)
	st_wr_cnt : int64;
	(** Write sectors, after we've forwarded the request to actual storage. *)
	st_wr_sect: int64;
	(** Sum of the request response time of all BLKIF_OP_WRITE *)
	st_wr_sum_usecs : int64;
	(** Absolute maximum BLKIF_OP_WRITE response time *)
	st_wr_max_usecs : int64;
}

(** Get a blktap3 statistics record *)
val get_blktap3_stats: filename:string -> blktap3_stats
