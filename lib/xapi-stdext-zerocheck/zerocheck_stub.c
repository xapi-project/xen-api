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
 */

#include <caml/memory.h>

/* for better performance in all case, we should process the unalign data at
 * the beginning until we reach a 32 bit align value, however since ocaml
 * allocate the string and we don't use any offset in this string, the string
 * is always correctly aligned.
 */
value is_all_zeros(value string, value length)
{
	CAMLparam2(string, length);
	const char *s = String_val(string);
	unsigned int *p;
	int len = Int_val(length);
	int i;

	p = (unsigned int *) s;
	for (i = len / 4; i > 0; i--)
		if (*p++ != 0)
			goto notallzero;
	s = (unsigned char *) p;
	for (i = 0; i < len % 4; i++)
		if (s[i] != 0)
			goto notallzero;
	CAMLreturn(Val_true);
notallzero:
	CAMLreturn(Val_false);
}
