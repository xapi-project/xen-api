/*
 * Copyright (C) 2015 Citrix Inc
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
#endif

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);
#define Nothing ((value) 0)

CAMLprim value stub_lseek64_data(value fd, value ofs) {
  CAMLparam2(fd, ofs);
  CAMLlocal1(result);
  int c_fd = Int_val(fd);
  off_t c_ofs = Int64_val(ofs);
  off_t c_ret;

  caml_enter_blocking_section();
#if defined(SEEK_DATA)
  c_ret = lseek(c_fd, c_ofs, SEEK_DATA);
  /* retry, if SEEK_DATA not supported on this file system */
  if (c_ret == -1 && errno == EINVAL)
    c_ret = lseek(c_fd, c_ofs, SEEK_SET);
#else
  /* Set the file pointer to ofs; pretend there is data */
  c_ret = lseek(c_fd, c_ofs, SEEK_SET);
#endif
  caml_leave_blocking_section();
  if (c_ret == -1) uerror("lseek", Nothing);

  result = caml_copy_int64(c_ret);
  CAMLreturn(result);
}

CAMLprim value stub_lseek64_hole(value fd, value ofs) {
  CAMLparam2(fd, ofs);
  CAMLlocal1(result);
  int c_fd = Int_val(fd);
  off_t c_ofs = Int64_val(ofs);
  off_t c_ret;

  caml_enter_blocking_section();
#if defined(SEEK_HOLE)
  c_ret = lseek(c_fd, c_ofs, SEEK_HOLE);
  /* retry, if SEEK_HOLE not supported on this file system */
  if (c_ret == -1 && errno == EINVAL)
    c_ret = lseek(c_fd, 0, SEEK_END);
#else
  /* Set the file pointer to the end of the file; pretend
     there is no hole */
  c_ret = lseek(c_fd, 0, SEEK_END);
#endif
  caml_leave_blocking_section();
  if (c_ret == -1) uerror("lseek", Nothing);
  result = caml_copy_int64(c_ret);
  CAMLreturn(result);
}
