/*
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
 *
 */

/*
  This stubs file retrieves the blkback statistics struct created by
  blktap3 under '/dev/shm/<vbd3-domid-devid>/statistics' *)
*/

#include <stdio.h>
#include <errno.h>
#include <blktap/blktap3.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>


CAMLprim value stub_get_blktap3_stats(value filename)
{
	
	CAMLparam1(filename);
	CAMLlocal1(stats);

	FILE *c_fd;
	struct blkback_stats c_stats;

	c_fd = fopen(String_val(filename), "rb");

	if (!c_fd) uerror("fopen", Nothing);
	if (fread(&c_stats, sizeof(struct blkback_stats), 1, c_fd) < 1) uerror("fread", Nothing);
	
	stats = caml_alloc_tuple(13);

	Store_field(stats, 0, caml_copy_int64((int64) c_stats.st_ds_req));
	Store_field(stats, 1, caml_copy_int64((int64) c_stats.st_f_req));
	Store_field(stats, 2, caml_copy_int64((int64) c_stats.st_oo_req));
	Store_field(stats, 3, caml_copy_int64((int64) c_stats.st_rd_req));
	Store_field(stats, 4, caml_copy_int64((int64) c_stats.st_rd_cnt));
	Store_field(stats, 5, caml_copy_int64((int64) c_stats.st_rd_sect));
	Store_field(stats, 6, caml_copy_int64((int64) c_stats.st_rd_sum_usecs));
	Store_field(stats, 7, caml_copy_int64((int64) c_stats.st_rd_max_usecs));
	Store_field(stats, 8, caml_copy_int64((int64) c_stats.st_wr_req));
	Store_field(stats, 9, caml_copy_int64((int64) c_stats.st_wr_cnt));
	Store_field(stats, 10, caml_copy_int64((int64) c_stats.st_wr_sect));
	Store_field(stats, 11, caml_copy_int64((int64) c_stats.st_wr_sum_usecs));
	Store_field(stats, 12, caml_copy_int64((int64) c_stats.st_wr_max_usecs));

	fclose(c_fd);

	CAMLreturn(stats);

}
