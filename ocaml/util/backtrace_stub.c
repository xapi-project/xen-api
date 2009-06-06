#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdio.h>
#include <string.h>

#if defined(CAML_GOT_GET_BACKTRACE) 
extern size_t caml_get_exception_backtrace(char *buffer, size_t length);
#endif

CAMLprim value stub_getbacktrace()
{
  CAMLparam0();
  CAMLlocal1(result);

#if defined(CAML_GOT_GET_BACKTRACE)
  char buffer[1000];
  int n = caml_get_exception_backtrace(buffer,sizeof(buffer));
  result=caml_alloc_string(n);
  memcpy(String_val(result), buffer, n);
#else
  char *errormsg="backtrace grabbing not compiled into ocaml";
  result=caml_alloc_string(strlen(errormsg));
  memcpy(String_val(result), errormsg, strlen(errormsg));
#endif

  CAMLreturn(result);
}
