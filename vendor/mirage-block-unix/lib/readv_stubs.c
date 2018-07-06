/*
 * Copyright (C) 2016 Docker Inc
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
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <limits.h>
#endif
#ifndef IOV_MAX
#define IOV_MAX 16 /* never used */
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/callback.h>

#include "lwt_unix.h"

struct job_readv {
  struct lwt_unix_job job;
  int fd;
#ifndef _WIN32
  struct iovec iovec[IOV_MAX];
#endif
  int length;
  int ret;
  int errno_copy;
};

static void worker_readv(struct job_readv *job)
{
#ifndef _WIN32
  job->ret = readv(job->fd, job->iovec, job->length);
  job->errno_copy = errno;
#else
  job->ret = -1;
  job->errno_copy = ENOTSUP;
#endif
}

static value result_readv(struct job_readv *job)
{
  CAMLparam0 ();
  int errno_copy = job->errno_copy;
  int ret = job->ret;
  lwt_unix_free_job(&job->job);
  if (ret == -1) {
    unix_error(errno_copy, "readv", Nothing);
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim
value mirage_block_unix_readv_job(value fd, value val_list)
{
  CAMLparam2(fd, val_list);
  CAMLlocal5(next, head, val_buf, val_ofs, val_len);
  int i;
  LWT_UNIX_INIT_JOB(job, readv, 0);
#ifdef _WIN32
  caml_failwith("readv is not supported on Win32");
#else
  job->fd = Int_val(fd);
  job->errno_copy = 0;
  job->ret = 0;
  next = val_list;
  /* Calculate the length of the val_list */
  job->length = 0;
  for (next = val_list; next != Val_emptylist; next = Field(next, 1))
    job->length++;

  /* Only copy up to the iovec array size */
  if (job->length > IOV_MAX)
    job->length = IOV_MAX;

  next = val_list;
  for (i = 0; i < job->length; i ++) {
    head = Field(next, 0);
    val_buf = Field(head, 0);
    val_ofs = Field(head, 1);
    val_len = Field(head, 2);
    job->iovec[i].iov_base = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
    job->iovec[i].iov_len = Long_val(val_len);
    next = Field(next, 1);
  }
#endif
  CAMLreturn(lwt_unix_alloc_job(&(job->job)));
}
