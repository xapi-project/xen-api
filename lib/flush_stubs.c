/*
 * Copyright (c) 2016 Docker Inc
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

#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#ifdef WIN32
#include <winsock2.h>
#include <wtypes.h>
#include <winbase.h>
#include <tchar.h>
#else
#define HANDLE int
#define DWORD int
#define Handle_val(x) Int_val(x)
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/callback.h>

#include "lwt_unix.h"

struct job_flush {
  struct lwt_unix_job job;
  HANDLE fd;
  int ask_drive_to_flush; /* only available on APPLE */
  DWORD errno_copy;
};

static void worker_flush(struct job_flush *job)
{
  int result = 0;
#ifdef WIN32
  if (!FlushFileBuffers(job->fd)) {
    job->errno_copy = GetLastError();
  }
#else
  #if defined(__APPLE__)
    if (job->ask_drive_to_flush) {
      result = fcntl(job->fd, F_FULLFSYNC);
    } else {
      result = fsync(job->fd);
    }
  #else
    result = fsync(job->fd);
  #endif
    if (result == -1) {
      job->errno_copy = errno;
    }
#endif
}

static value result_flush(struct job_flush *job)
{
  CAMLparam0 ();
  int errno_copy = job->errno_copy;
  lwt_unix_free_job(&job->job);
  if (errno_copy != 0) {
#ifdef WIN32
  win32_maperr(errno_copy);
  unix_error(errno_copy, "FlushFileBuffers", Nothing);
#else
  #if defined(__APPLE__)
      unix_error(errno_copy, "fcntl", Nothing);
  #else
      unix_error(errno_copy, "fsync", Nothing);
  #endif
#endif
  }
  CAMLreturn(Val_unit);
}

CAMLprim
value mirage_block_unix_flush_job(value handle, value ask_drive_to_flush)
{
  CAMLparam2(handle, ask_drive_to_flush);
  LWT_UNIX_INIT_JOB(job, flush, 0);
  job->fd = (HANDLE)Handle_val(handle);
  job->ask_drive_to_flush = Bool_val(ask_drive_to_flush);
  job->errno_copy = 0;
  CAMLreturn(lwt_unix_alloc_job(&(job->job)));
}
