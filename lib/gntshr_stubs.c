/*
 * Copyright (C) 2012-2013 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdlib.h>
#include <errno.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include <sys/mman.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define XC_WANT_COMPAT_GNTTAB_API
#include <xenctrl.h>

#if __XEN_INTERFACE_VERSION__ >= 0x00040200
#define HAVE_GNTSHR 1
#endif

CAMLprim value stub_gntshr_allocates(void)
{
        CAMLparam0();
        CAMLreturn(Val_bool(1));
}

static void gntshr_missing()
{
	value *v = caml_named_value("gntshr.missing");
	/* if v is NULL then it's either an error in gntshr.ml or a
	   linking error */ 
	assert (v != NULL);
	caml_raise_constant(*v);
}

#define _G(__g) ((xc_gntshr *)(__g))

#define XC_GNTTAB_BIGARRAY (CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL)

#ifdef HAVE_GNTSHR
#define ERROR_STRLEN 1024
static void failwith_xc(xc_interface *xch)
{
        static char error_str[ERROR_STRLEN];
        if (xch) {
                const xc_error *error = xc_get_last_error(xch);
                if (error->code == XC_ERROR_NONE)
                        snprintf(error_str, ERROR_STRLEN, "%d: %s", errno, strerror(errno));
                else
                        snprintf(error_str, ERROR_STRLEN, "%d: %s: %s",
                                 error->code,
                                 xc_error_code_to_desc(error->code),
                                 error->message);
        } else {
                snprintf(error_str, ERROR_STRLEN, "Unable to open XC interface");
        }
        caml_failwith(error_str);
}
#endif

CAMLprim value stub_gntshr_open(value unit)
{
	CAMLparam1(unit);
	CAMLlocal1(result);
#ifdef HAVE_GNTSHR
	xc_gntshr *xgh;

	xgh = xc_gntshr_open(NULL, 0);
	if (NULL == xgh)
		caml_failwith("Failed to open interface");
	result = (value)xgh;
#else
	gntshr_missing();
#endif
	CAMLreturn(result);
}

CAMLprim value stub_gntshr_close(value xgh)
{
	CAMLparam1(xgh);
#ifdef HAVE_GNTSHR
	xc_gntshr_close(_G(xgh));
#else
	gntshr_missing();
#endif
	CAMLreturn(Val_unit);
}


CAMLprim value stub_gntshr_grant_access(value i, value g, value p, value d, value w)
{
	abort();
}

CAMLprim value stub_gntshr_end_access(value i, value g)
{
	abort();
}

CAMLprim value stub_gntshr_share_pages_batched(value xgh, value domid, value count, value writable) {
	CAMLparam4(xgh, domid, count, writable);
	CAMLlocal4(result, ml_refs, ml_refs_cons, ml_map);
#ifdef HAVE_GNTSHR
	void *map;
	uint32_t *refs;
	int i;
  int c_count = Int_val(count);
	result = caml_alloc(2, 0);
	refs = malloc(c_count * sizeof(uint32_t));

	map = xc_gntshr_share_pages(_G(xgh), Int_val(domid), c_count, refs, Bool_val(writable));

	if(NULL == map) {
		free(refs);
		failwith_xc(_G(xgh));
	}

	// Construct the list of grant references.
	ml_refs = Val_emptylist;
	for(i = c_count - 1; i >= 0; i--) {
		ml_refs_cons = caml_alloc(2, 0);

		Store_field(ml_refs_cons, 0, Val_int(refs[i]));
		Store_field(ml_refs_cons, 1, ml_refs);

		ml_refs = ml_refs_cons;
	}

	ml_map = alloc_bigarray_dims(XC_GNTTAB_BIGARRAY, 1,
                              map, (c_count << XC_PAGE_SHIFT));

	Store_field(result, 0, ml_refs);
	Store_field(result, 1, ml_map);

	free(refs);
#else
	gntshr_missing();
#endif
	CAMLreturn(result);
}

CAMLprim value stub_gntshr_munmap_batched(value xgh, value share) {
	CAMLparam2(xgh, share);
	CAMLlocal1(ml_map);
#ifdef HAVE_GNTSHR
	ml_map = Field(share, 1);

	int size = Bigarray_val(ml_map)->dim[0];
	int pages = size >> XC_PAGE_SHIFT;
#ifdef linux
	/* Bug in xen-4.4 libxc xc_linux_osdep implementation, work-around
	   by using the kernel interface directly. */
	int result = munmap(Data_bigarray_val(ml_map), size);
#else
	int result = xc_gntshr_munmap(_G(xgh), Data_bigarray_val(ml_map), pages);
#endif
	if(result != 0)
		failwith_xc(_G(xgh));
#else
	gntshr_missing();
#endif
	CAMLreturn(Val_unit);
}
