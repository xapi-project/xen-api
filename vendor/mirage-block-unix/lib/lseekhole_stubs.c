/*
 * Copyright (C) 2012-2013 Citrix Inc
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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

#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);
#define Nothing ((value) 0)

CAMLprim value stub_lseek_data_64(value fd, value ofs){
  CAMLparam2(fd, ofs);
  CAMLlocal1(result);
  int c_fd = Int_val(fd);
  off_t c_ofs = Int64_val(ofs);
  off_t ret = 0;
#if defined(SEEK_DATA)
  caml_enter_blocking_section();
  ret = lseek(c_fd, c_ofs, SEEK_DATA);
  caml_leave_blocking_section();
  if (ret == -1) uerror("lseek SEEK_DATA", Nothing);
#else
  /* If the filesystem doesn't support sparseness then we would return
     the requested start offset as the next block of data, since we don't
     know for sure but this is safe. */
  caml_enter_blocking_section();
  ret = lseek(c_fd, c_ofs, SEEK_SET);
  caml_leave_blocking_section();
#endif
  result = caml_copy_int64(ret);
  CAMLreturn(result);
}

CAMLprim value stub_lseek_hole_64(value fd, value ofs){
  CAMLparam2(fd, ofs);
  CAMLlocal1(result);
  int c_fd = Int_val(fd);
  off_t ret = 0;
#if defined(SEEK_HOLE)
  off_t c_ofs = Int64_val(ofs);
  caml_enter_blocking_section();
  ret = lseek(c_fd, c_ofs, SEEK_HOLE);
  caml_leave_blocking_section();
  if (ret == -1) uerror("lseek SEEK_HOLE", Nothing);
#else
  /* If the filesystem doesn't support sparseness then we return
     the next hole as the end of the file, since this is safe. */
  caml_enter_blocking_section();
  ret = lseek(c_fd, 0, SEEK_END);
  caml_leave_blocking_section();
#endif
  result = caml_copy_int64(ret);
  CAMLreturn(result);
}
