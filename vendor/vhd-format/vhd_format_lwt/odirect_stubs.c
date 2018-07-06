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

#define _GNU_SOURCE /* needed for O_DIRECT */

#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);
#define Nothing ((value) 0)

CAMLprim value stub_openfile_direct(value filename, value rw, value perm){
  CAMLparam3(filename, rw, perm);
  CAMLlocal1(result);
  int fd;

  const char *filename_c = strdup(String_val(filename));

  enter_blocking_section();
  int flags = 0;
#if defined(O_DIRECT)
  flags |= O_DIRECT;
#endif
  if (Bool_val(rw)) {
    flags |= O_RDWR;
  } else {
    flags |= O_RDONLY;
  }
  fd = open(filename_c, flags, Int_val(perm));
  leave_blocking_section();

  free((void*)filename_c);

  if (fd == -1) uerror("open", filename);

  CAMLreturn(Val_int(fd));
}

CAMLprim value stub_fsync (value fd)
{
  CAMLparam1(fd);
  int c_fd = Int_val(fd);
  if (fsync(c_fd) != 0) uerror("fsync", Nothing);
  CAMLreturn(Val_unit);
}

