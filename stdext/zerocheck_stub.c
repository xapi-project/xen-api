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

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

#define OFFSET(s) (((unsigned int)s) & (sizeof(unsigned int) - 1))


/* Return the offset of the next non-zero byte (possibly rounded down a bit).
   The value 'remaining' is returned if there is no non-zero byte found. */
value find_a_nonzero(value string, value offset, value remaining)
{
	CAMLparam3(string, offset, remaining);
	int c_offset = Int_val(offset);
	int c_remaining = Int_val(remaining);
	int c_origremaining = c_remaining;
	char *c_string = String_val(string);
	char *s = c_string + c_offset;

	/* Go character by character until we hit an unsigned int boundary */
	while ((OFFSET(s) != 0) && (c_remaining > 0)){ 
		if (*s != '\000') goto finish;
		s++; c_remaining--;
	}
	/* Go word by word. Note we don't need to determine the exact position
	   of the nonzero, it suffices to return the index of the word containing	
	   the nonzero. */
	unsigned int *p = (unsigned int *)s;
	while (c_remaining > 4){
		if (*p != 0) goto finish;
		p++; c_remaining-=4;
	}
	/* Go character by character until the end of the string */
	s = (char*) p;
	while (c_remaining > 0){
		if (*s != '\000') goto finish;
		s++; c_remaining--;
	}
	/* c_remaining == 0 */
 finish:
	/* If we didn't find a nonzero then we return c_origremaining.
	   If we did then we return the number of chars after the starting
	   offset where the word containing the nonzero was detected. */
	CAMLreturn(Val_int(c_origremaining - c_remaining));
			       
}

/* Return the offset of the next zero byte (possibly rounded up a bit).
   The value 'remaining' is returned if there is no zero byte found. */
value find_a_zero(value string, value offset, value remaining)
{
	CAMLparam3(string, offset, remaining);
	int c_offset = Int_val(offset);
	int c_remaining = Int_val(remaining);
	int c_origremaining = c_remaining;
	char *c_string = String_val(string);
	char *s = c_string + c_offset;

	/* Go character by character until we hit an unsigned int boundary */
	while ((OFFSET(s) != 0) && (c_remaining > 0)){ 
		if (*s == '\000') goto finish;
		s++; c_remaining--;
	}
	/* Go word by word. Note we don't need to determine the exact position
	   of the zero, it suffices to return the index of the word following	
	   the zero. */
	unsigned int *p = (unsigned int *)s;
	while (c_remaining > 4){
		if (*p == 0) goto finish;
		p++; c_remaining-=4;
	}
	/* Go character by character until the end of the string */
	s = (char*) p;
	while (c_remaining > 0){
		if (*s == '\000') goto finish;
		s++; c_remaining--;
	}
	/* c_remaining == 0 */
 finish:
	/* If we didn't find a zero then we return c_origremaining.
	   If we did then we return the number of chars after the starting
	   offset where the word containing the zero was detected. */
	CAMLreturn(Val_int(c_origremaining - c_remaining));			       
}



/* for better performance in all case, we should process the unalign data at
 * the beginning until we reach a 32 bit align value, however since ocaml
 * allocate the string and we don't use any offset in this string, the string
 * is always correctly aligned.
 */
value is_all_zeros(value string, value length)
{
	CAMLparam2(string, length);
	char *s = String_val(string);
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
