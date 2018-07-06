#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <time.h>
#include <stdint.h>

#include "ocaml_utils.h"
#include "config.h"

#include "time_ns_stubs.h"

#define NANOS_PER_SECOND 1000000000

#if defined(JSC_POSIX_TIMERS)

/* Note: this is imported noalloc if (and only if) ARCH_SIXTYFOUR is defined.
 * This is OK because caml_alloc_int63 doesn't actually allocate in that case. */
CAMLprim value core_kernel_time_ns_gettime_or_zero()
{
  struct timespec ts;

  if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
    return caml_alloc_int63(0);
  else
    return caml_alloc_int63(NANOS_PER_SECOND * (uint64_t)ts.tv_sec + (uint64_t)ts.tv_nsec);
}

#else

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value core_kernel_time_ns_gettime_or_zero()
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1)
    return caml_alloc_int63(0);
  else
    return caml_alloc_int63(NANOS_PER_SECOND * (uint64_t)tp.tv_sec + (uint64_t)tp.tv_usec * 1000);
}

#endif

CAMLprim value core_kernel_time_ns_format_tm(struct tm * tm, value v_fmt)
{
  size_t len;
  char* buf;
  int buf_len;
  value v_str;
  /* 100 * length should be large enough to contain the output of strftime. The
     longest expansion we know of is "%c" in the km_KH.utf8 locale, which
     requires 151 bytes. */
  buf_len = 100 * caml_string_length(v_fmt);
  buf = malloc(buf_len);
  if (!buf) caml_failwith("core_kernel_time_ns_format_tm: malloc failed");
  len = strftime(buf, buf_len, String_val(v_fmt), tm);

  if (len == 0) {
    /* From the man page:
         "Note that the return value 0 does not necessarily indicate an error;
          for example, in many locales %p yields an empty string."
       Given how large our buffer is we just assume that 0 always indicates
       an empty string. */
    v_str = caml_copy_string("");
    free(buf);
    return v_str;
  }

  v_str = caml_copy_string(buf);  /* [strftime] always null terminates the string */
  free(buf);
  return v_str;
}


CAMLprim value core_kernel_time_ns_format(value t, value v_fmt)
{
  time_t clock;
  struct tm * tm;
  clock = (time_t) Double_val(t);
  /* This [tm] must not be freed. It refers to statically allocated
     memory and its contents change every time [localtime] is
     called. */
  tm = localtime(&clock);
  if (tm == NULL) caml_failwith("core_kernel_time_ns_format: localtime failed");
  return core_kernel_time_ns_format_tm(tm, v_fmt);
}
