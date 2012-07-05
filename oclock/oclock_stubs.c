/*
Copyright (c) 2011, Mickaël Delahaye <mickael.delahaye@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

CAMLprim value oclock_getclocks(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);
  res = caml_alloc_tuple(5);
  Store_field(res, 0, Val_int(CLOCK_REALTIME));
  Store_field(res, 1, Val_int(CLOCK_MONOTONIC));
  Store_field(res, 2, Val_int(CLOCK_PROCESS_CPUTIME_ID));
  Store_field(res, 3, Val_int(CLOCK_THREAD_CPUTIME_ID));
  Store_field(res, 4, Val_int(
#ifdef CLOCK_MONOTONIC_RAW
    CLOCK_MONOTONIC_RAW
#else
    CLOCK_MONOTONIC
#endif
  ));
  CAMLreturn(res);
}

CAMLprim value oclock_getres(value vclockid) {
  CAMLparam1(vclockid);
  CAMLlocal1(res);

  clockid_t clockid = Int_val(vclockid);
  struct timespec ts;
  if (clock_getres(clockid, &ts) != 0) {
    switch (errno) {
    case EINVAL:
      /* not supported clkid*/
      caml_invalid_argument ("unsupported clock");
    case EFAULT:
      /* invalid ts, SHOULD NOT HAPPEN*/
    default:
      caml_failwith ("unknown failure");
    }
  }

  res = copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);

  CAMLreturn(res);
}

CAMLprim value oclock_gettime(value vclockid) {
  CAMLparam1(vclockid);
  CAMLlocal1(res);

  clockid_t clockid = Int_val(vclockid);
  struct timespec ts;
  if (clock_gettime(clockid, &ts) != 0) {
    switch (errno) {
    case EINVAL:
      /* not supported clkid*/
      caml_invalid_argument ("unsupported clock");
    case EFAULT:
      /* invalid ts, SHOULD NOT HAPPEN*/
    default:
      caml_failwith ("unknown failure");
    }
  }

  res = copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);

  CAMLreturn(res);
}

CAMLprim value oclock_settime(value vclockid, value vvalue) {
  CAMLparam2(vclockid,vvalue);
  
  clockid_t clockid = Int_val(vclockid);
  long long ll = Int64_val(vvalue);
  struct timespec ts;
  ts.tv_sec = ll / 1000000000LL;
  ts.tv_nsec = ll % 1000000000LL; // maybe add a check for negative
  
  if (clock_settime(clockid, &ts) != 0) {
    switch (errno) {
    case EINVAL:
      /* not supported clkid */
      caml_invalid_argument ("unsupported clock");
    case EPERM: /* dont have the perm */
      caml_failwith ("settime permission");
    case EFAULT:
      /* invalid ts, SHOULD NOT HAPPEN*/
    default:
      caml_failwith ("unknown failure");
    }
  }
  CAMLreturn(Val_unit);
}

CAMLprim value oclock_getcpuclockid(value vpid) {
  CAMLparam1(vpid);
  
  int pid = Int_val(vpid);
  
  clockid_t clkid;
  if (clock_getcpuclockid(pid, &clkid) != 0)  {
    switch (errno) {
    case ENOSYS:
      /* "The  kernel  does not support obtaining the per-process CPU-time
          clock of another process, and pid does not specify  the  calling
          process" */
      caml_failwith ("unsupported feature");
    case EPERM: /* "The caller does not have the permission (...)" */
      caml_failwith ("invalid permission");
    case ESRCH: /* should be this one */
    case EINVAL:
    case ESPIPE: /* my linux send this one */
      caml_invalid_argument ("invalid pid");
    default:
      caml_failwith ("unknown failure");
    }
  }
  
  CAMLreturn(Val_int(clkid));
}

CAMLprim value oclock_pthread_getcpuclockid(value vpid) {
  CAMLparam1(vpid);
  
  int pid = Int_val(vpid);
  
  clockid_t clkid;
  if (pthread_getcpuclockid(pid, &clkid) != 0)  {
    perror(NULL);
    switch (errno) {
    case ENOENT:
      /* "Per-thread CPU time clocks are not supported by the system." */
      caml_failwith ("unsupported feature");
    case EPERM: /* dont have the perm */
      caml_failwith ("invalid permission");
    case ESRCH: /* should be this one */
    case EINVAL:
    case ESPIPE:
      /* No thread with the ID thread could be found. */
      caml_invalid_argument ("invalid pid");
    default:
      caml_failwith ("unknown failure");
    }
  }
  
  CAMLreturn(Val_int(clkid));
}
