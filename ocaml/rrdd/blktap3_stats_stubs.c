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
	
	stats = caml_alloc_tuple(6);

	Store_field(stats, 0, caml_copy_int64((int64) c_stats.st_rd_req));
	Store_field(stats, 1, caml_copy_int64((int64) c_stats.st_rd_cnt));
	Store_field(stats, 2, caml_copy_int64((int64) c_stats.st_rd_sum_usecs));
	Store_field(stats, 3, caml_copy_int64((int64) c_stats.st_wr_req));
	Store_field(stats, 4, caml_copy_int64((int64) c_stats.st_wr_cnt));
	Store_field(stats, 5, caml_copy_int64((int64) c_stats.st_wr_sum_usecs));

	CAMLreturn(stats);

}
