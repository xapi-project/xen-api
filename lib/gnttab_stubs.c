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
#include <stdint.h>
#include <string.h>
#include <errno.h>

/* For PROT_READ | PROT_WRITE */
#include <sys/mman.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#define XC_WANT_COMPAT_GNTTAB_API
#include <xenctrl.h>

#define _G(__g) ((xc_gnttab *)(__g))

CAMLprim value stub_gnttab_interface_open(void)
{
	CAMLparam0();
	xc_gnttab *xgh;
	xgh = xc_gnttab_open(NULL, 0);
	if (xgh == NULL)
		caml_failwith("Failed to open interface");
	CAMLreturn((value)xgh);
}

CAMLprim value stub_gnttab_interface_close(value xgh)
{
	CAMLparam1(xgh);

	xc_gnttab_close(_G(xgh));

	CAMLreturn(Val_unit);
}

CAMLprim value stub_gnttab_allocates(void)
{
	CAMLparam0();
	CAMLreturn(Val_bool(1));
}

/* type grant_handle = Io_page.t = external bigarray */

CAMLprim value stub_gnttab_unmap(value xgh, value array) 
{
	CAMLparam2(xgh, array);

	int size = Caml_ba_array_val(array)->dim[0];
	int pages = size >> XC_PAGE_SHIFT;
	int result = xc_gnttab_munmap(_G(xgh), Caml_ba_data_val(array), pages);
	if(result!=0) {
		caml_failwith("Failed to unmap grant");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value stub_gnttab_map_onto(value xgh, value g, value p, value d, value wr)
{
	/* This should never happen because the OCaml code will have
	   asked us if we allocate or not first */
	caml_failwith("This grant table implementation allocates internally, cannot implement map_onto");
}

#define XC_GNTTAB_BIGARRAY (CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL)

CAMLprim value stub_gnttab_map_fresh(
	value xgh,
	value reference,
	value domid,
	value writable
	)
{
	CAMLparam4(xgh, reference, domid, writable);
	CAMLlocal2(pair, contents);

	void *map =
		xc_gnttab_map_grant_ref(_G(xgh), Int_val(domid), Int_val(reference),
		Bool_val(writable)?PROT_READ | PROT_WRITE:PROT_READ);

	if(map==NULL) {
		caml_failwith("Failed to map grant ref");
	}

	contents = caml_ba_alloc_dims(XC_GNTTAB_BIGARRAY, 1,
		map, 1 << XC_PAGE_SHIFT);
	pair = caml_alloc_tuple(2);
	Store_field(pair, 0, contents); /* grant_handle */
	Store_field(pair, 1, contents); /* Io_page.t */
	CAMLreturn(pair);
}

CAMLprim value stub_gnttab_mapv_batched(
	value xgh,
	value array,
	value writable)
{
	CAMLparam3(xgh, array, writable);
	CAMLlocal4(domid, reference, contents, pair);
	int count = Wosize_val(array) / 2;
	uint32_t domids[count];
	uint32_t refs[count];
	int i;

	for (i = 0; i < count; i++){
		domids[i] = Int_val(Field(array, i * 2 + 0));
		refs[i] = Int_val(Field(array, i * 2 + 1));
	}
	void *map =
		xc_gnttab_map_grant_refs(_G(xgh),
                	count, domids, refs,
			Bool_val(writable)?PROT_READ | PROT_WRITE : PROT_READ);

	if(map==NULL) {
		caml_failwith("Failed to map grant ref");
	}

	contents = caml_ba_alloc_dims(XC_GNTTAB_BIGARRAY, 1,
		map, count << XC_PAGE_SHIFT);
	pair = caml_alloc_tuple(2);
	Store_field(pair, 0, contents); /* grant_handle */
	Store_field(pair, 1, contents); /* Io_page.t */
	CAMLreturn(pair);
}

CAMLprim value stub_gnttab_fini(void)
{
	return 0;
}

CAMLprim value stub_gnttab_init(void)
{
	return 0;
}

CAMLprim value stub_gnttab_reserved(void)
{
	return (Val_int(0));
}

CAMLprim value stub_gnttab_nr_entries(void)
{
	return (Val_int(0));
}
