/*
 * Copyright (C) 2018 Docker Inc
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

#include <errno.h>
#include <stdint.h>
#ifndef _WIN32
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

#include "lwt_unix.h"

struct job_chsize {
  struct lwt_unix_job job;
  int fd;
  uint64_t size;
  int errno_copy;
};

#ifdef _WIN32

#include <io.h>

#else

#define errno_t int

#define Handle_val(x) Int_val(x)

errno_t _chsize_s(int fd, int64_t size) {
  return ENOTSUP;
}

#endif
#include <stdio.h>
static void worker_chsize(struct job_chsize *job)
{
fprintf(stderr, "fd = %d\n", job->fd);
  job->errno_copy = _chsize_s(job->fd, job->size);
}

static value result_chsize(struct job_chsize *job)
{
  CAMLparam0 ();
  int errno_copy = job->errno_copy;
  lwt_unix_free_job(&job->job);
  if (errno_copy != 0) {
    unix_error(errno_copy, "chsize_s", Nothing);
  }
  CAMLreturn(Val_int(0));
}

CAMLprim
value mirage_block_unix_chsize_job(value fd, value size)
{
  CAMLparam2(fd, size);

  LWT_UNIX_INIT_JOB(job, chsize, 0);
#ifdef _WIN32
  job->fd = _open_osfhandle((intptr_t)Handle_val(fd), 0);
#else
  job->fd = Int_val(fd);
#endif
  job->size = Int64_val(size);
  job->errno_copy = 0;

  CAMLreturn(lwt_unix_alloc_job(&(job->job)));
}

