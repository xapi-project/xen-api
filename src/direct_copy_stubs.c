/*
 * Copyright (C) 2012-2013 Citrix Inc
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

#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);
#define Nothing ((value) 0)

enum direct_copy_rc {
  OK                   = 0,
  TRIED_AND_FAILED     = 1,
  READ_FAILED          = 2,
  WRITE_FAILED         = 3,
  WRITE_UNEXPECTED_EOF = 4
};

#define XFER_BUFSIZ (2*1024*1024)

struct direct_copy_handle {
  int  in_fd;
  int  out_fd;
  char *buffer;
};

CAMLprim value stub_init(value in_fd, value out_fd)
{
  CAMLparam2(in_fd, out_fd);
  CAMLlocal1(result);
  int c_in_fd = Int_val(in_fd);
  int c_out_fd = Int_val(out_fd);
  struct direct_copy_handle *cpinfo = NULL;
  int flags;

  /* This is where we will keep the handle on return to OCaml. The
   * Abstract tag teaches OCaml's garbage collector not to mess with
   * it */
  result = alloc(1, Abstract_tag);

  /* initialise handle */
  cpinfo = malloc(sizeof(struct direct_copy_handle));
  if (!cpinfo) caml_raise_out_of_memory();
  cpinfo->buffer = NULL;
  if (posix_memalign((void **)&cpinfo->buffer, sysconf(_SC_PAGESIZE), XFER_BUFSIZ)) {
      free(cpinfo);
      caml_raise_out_of_memory();
  }
  cpinfo->in_fd = c_in_fd;
  cpinfo->out_fd = c_out_fd;

#ifdef __linux__
  /* Force the output to have O_DIRECT if possible.
     Because it may not be possible, ignore any error
     we might get on setting the flag.
  */
  flags = fcntl(c_out_fd, F_GETFL, NULL);
  if (flags > 0 && !(flags & O_DIRECT))
    fcntl(c_out_fd, F_SETFL, flags | O_DIRECT);
#endif

  Field(result, 0) = (uintptr_t)cpinfo;
  CAMLreturn(result);

}

CAMLprim value stub_cleanup(value handle)
{
  CAMLparam1(handle);
  struct direct_copy_handle *cpinfo = NULL;

  assert(Is_block(handle) && Tag_val(handle) == Abstract_tag);
  cpinfo = (struct direct_copy_handle *)Field(handle, 0);

  free(cpinfo->buffer);
  free(cpinfo);
  Field(handle, 0) = (uintptr_t)NULL;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_direct_copy(value handle, value len){
  CAMLparam2(handle, len);
  CAMLlocal1(result);
  size_t c_len = Int64_val(len);
  struct direct_copy_handle *cpinfo = NULL;
  size_t bytes;
  size_t remaining;
  enum direct_copy_rc rc;

  assert(Is_block(handle) && Tag_val(handle) == Abstract_tag);
  cpinfo = (struct direct_copy_handle *)Field(handle, 0);
  if (!cpinfo) caml_failwith("direct_copy: NULL handle");

  /* Calling enter_blocking_section() actually releases the OCaml
   * runtime lock, so no OCaml exceptions may be thrown, and no OCaml
   * values may be accessed, until it is reacquired. Also this
   * means other OCaml threads may do things while this is going
   * on so the caller must be careful. */
  enter_blocking_section();

  rc = TRIED_AND_FAILED;
  bytes = 0;
  
  remaining = c_len;
  while (remaining > 0) {
    ssize_t bread;
    ssize_t bwritten = 0;

    bread = read(cpinfo->in_fd, cpinfo->buffer, (remaining < XFER_BUFSIZ)?remaining:XFER_BUFSIZ) ;
    if (bread < 0) {
        rc = READ_FAILED;
        goto fail;
    }
    while (bwritten < bread) {
      ssize_t ret;

      ret = write(cpinfo->out_fd, cpinfo->buffer + bwritten, bread - bwritten);
      if (ret == 0) {
          rc = WRITE_UNEXPECTED_EOF;
          goto fail;
      }
      if (ret < 0) {
          rc = WRITE_FAILED;
          goto fail;
      }
      bytes += ret;
      bwritten += ret;
      remaining -= ret;
    }
  }
  rc = OK;
fail:

  leave_blocking_section();
  /* Now that the OCaml runtime lock is reacquired, it is safe to
   * raise OCaml exceptions */

  switch (rc) {
    case TRIED_AND_FAILED:
      caml_failwith("direct_copy: General error");
      break;
    case WRITE_FAILED:
      uerror("write", Nothing);
      break;
    case READ_FAILED:
      uerror("read", Nothing);
      break;
    case WRITE_UNEXPECTED_EOF:
      caml_failwith("direct_copy: Unexpected EOF on write");
      break;
    case OK:
      break;
  }
  result = caml_copy_int64(bytes);
  CAMLreturn(result);
}
