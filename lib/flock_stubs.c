/*
 * Copyright (C) 2017 Docker Inc
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

#include <sys/file.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);

CAMLprim value stub_flock(value fd, value ex, value nb){
  CAMLparam3(fd, ex, nb);
  int result;
#ifdef _WIN32
  caml_failwith("flock is not supported on Win32");
#else
  const int c_fd = Int_val(fd);
  int flags = 0;
  if (Bool_val(ex)) {
    flags |= LOCK_EX;
  } else {
    flags |= LOCK_SH;
  }
  if (Bool_val(nb)) {
    flags |= LOCK_NB;
  }
  enter_blocking_section();
  result = flock(c_fd, flags);
  leave_blocking_section();

  if (result != 0) {
    uerror("flock", fd);
  }
  CAMLreturn(Val_unit);
#endif /* _WIN32 */
}
