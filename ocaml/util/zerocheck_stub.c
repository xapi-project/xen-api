
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

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
