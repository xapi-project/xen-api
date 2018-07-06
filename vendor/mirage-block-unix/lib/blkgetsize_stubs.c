/*
 * Copyright (C) 2012-2013 Citrix Inc
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

#include <sys/types.h>
#include <sys/stat.h>
#ifndef _WIN32
#include <sys/ioctl.h>
#endif

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
#include <linux/fs.h>

int blkgetsize(int fd, uint64_t *psize)
{
#ifdef BLKGETSIZE64
  int ret = ioctl(fd, BLKGETSIZE64, psize);
#elif BLKGETSIZE
  unsigned long sectors = 0;
  int ret = ioctl(fd, BLKGETSIZE, &sectors);
  *psize = sectors * 512ULL;
#else
# error "Linux configuration error (blkgetsize)"
#endif
  return ret;
}

int blkgetsectorsize(int fd, int *size)
{
#ifdef BLKSSZGET
  int ret = ioctl(fd, BLKSSZGET, size);
#else
# error "Linux configuration error (BLKSSZGET)"
#endif
  return ret;
}

#elif defined(__APPLE__)
#include <sys/disk.h>

int blkgetsize(int fd, uint64_t *psize)
{
  unsigned long blocksize = 0;
  int ret = ioctl(fd, DKIOCGETBLOCKSIZE, &blocksize);
  if (!ret) {
    unsigned long nblocks;
    ret = ioctl(fd, DKIOCGETBLOCKCOUNT, &nblocks);
    if (!ret)
      *psize = (uint64_t)nblocks * blocksize;
  }
  return ret;
}

int blkgetsectorsize(int fd, int *size)
{
  int ret = ioctl(fd, DKIOCGETBLOCKSIZE, &size);
  return ret;
}

#elif defined(__FreeBSD__) || defined(__NetBSD__)
#include <sys/disk.h>

int blkgetsize(int fd, uint64_t *psize)
{
  int ret = ioctl(fd, DIOCGMEDIASIZE, psize);
  return ret;
}

int blkgetsectorsize(int fd, int *size)
{
  int ret = ioctl(fd, DIOCGSECTORSIZE, &size);
  return ret;
}

#elif _WIN32

int blkgetsize(int fd, uint64_t *psize)
{
  return 0; /* Will never be called because there are no block device files */
}

int blkgetsectorsize(int fd, int *size)
{
  return 512; /* Will never be called because there are no block device files */
}

#else
# error "Unable to query block device size: unsupported platform, please report."
#endif

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);
#define Nothing ((value) 0)

CAMLprim value stub_blkgetsize(value fd){
  CAMLparam1(fd);
  CAMLlocal1(result);
  uint64_t size_in_bytes;
  int c_fd = Int_val(fd); /* safe only because Unix.file_descr = int */
  int success = -1;

  enter_blocking_section();
  if (blkgetsize(c_fd, &size_in_bytes) == 0)
    success = 0;
  leave_blocking_section();

  if (success == -1) uerror("BLKGETSIZE", Nothing);

  result = caml_copy_int64(size_in_bytes);
  CAMLreturn(result);
}

CAMLprim value stub_blkgetsectorsize(value fd){
  CAMLparam1(fd);
  CAMLlocal1(result);
  int size;
  int c_fd = Int_val(fd); /* safe only because Unix.file_descr = int */
  int success = -1;

  enter_blocking_section();
  if (blkgetsectorsize(c_fd, &size) == 0)
    success = 0;
  leave_blocking_section();

  if (success == -1) uerror("blkgetsectorsize", Nothing);

  CAMLreturn(Val_int(size));
}
