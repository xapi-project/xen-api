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

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#ifdef __linux__
# include <linux/fs.h>
# include <sys/sendfile.h>
#endif

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);
#define Nothing ((value) 0)

#define NOT_IMPLEMENTED (-1)
#define TRIED_AND_FAILED (1)
#define OK 0

CAMLprim value stub_sendfile64(value in_fd, value out_fd, value len){
  CAMLparam3(in_fd, out_fd, len);
  CAMLlocal1(result);
  size_t c_len = Int64_val(len);
  size_t bytes;
  int c_in_fd = Int_val(in_fd);
  int c_out_fd = Int_val(out_fd);

  int rc = NOT_IMPLEMENTED;

  enter_blocking_section();

#ifdef __linux__
  rc = TRIED_AND_FAILED;
  bytes = sendfile(c_out_fd, c_in_fd, NULL, c_len);
  if (bytes != -1) {
      rc = OK;
  } else if (errno == EINVAL) {
      int flags;
      int result = 0;

      flags = fcntl(c_in_fd, F_GETFL, NULL);
      if (flags > 0) {
          result = fcntl(c_in_fd, F_SETFL, flags & ~O_DIRECT);

          if (result == 0) {
              bytes = sendfile(c_out_fd, c_in_fd, NULL, c_len);
              if (bytes != -1) {
                  rc = OK;
              }
              fcntl(c_in_fd, F_SETFL, flags);
          }
      }
  }
#endif

  leave_blocking_section();

  switch (rc) {
    case NOT_IMPLEMENTED:
      caml_failwith("This platform does not support sendfile()");
      break;
    case TRIED_AND_FAILED:
      uerror("sendfile", Nothing);
      break;
    default: break;
  }
  result = caml_copy_int64(bytes);
  CAMLreturn(result);
}
