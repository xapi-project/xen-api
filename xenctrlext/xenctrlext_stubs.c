/*
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
 */

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>

CAMLprim value stub_xenctrlext_get_boot_cpufeatures(value xch)
{
	CAMLparam1(xch);
#if defined(XENCTRL_HAS_GET_CPUFEATURES)
	CAMLlocal1(v);
	uint32_t a, b, c, d, e, f, g, h;
	int ret;

	ret = xc_get_boot_cpufeatures(_H(xch), &a, &b, &c, &d, &e, &f, &g, &h);
	if (ret < 0)
	  failwith_xc(_H(xch));
 
	v = caml_alloc_tuple(8);
	Store_field(v, 0, caml_copy_int32(a));
	Store_field(v, 1, caml_copy_int32(b));
	Store_field(v, 2, caml_copy_int32(c));
	Store_field(v, 3, caml_copy_int32(d));
	Store_field(v, 4, caml_copy_int32(e));
	Store_field(v, 5, caml_copy_int32(f));
	Store_field(v, 6, caml_copy_int32(g));
	Store_field(v, 7, caml_copy_int32(h));

	CAMLreturn(v);
#else
	failwith("XENCTRL_HAS_GET_CPUFEATURES not defined");
#endif
}
 
/* 
* Local variables: 
* indent-tabs-mode: t
*/
