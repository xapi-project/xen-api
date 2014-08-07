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
#include <sys/ioctl.h>

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

#elif defined(__FreeBSD__)
#include <sys/disk.h>

int blkgetsize(int fd, uint64_t *psize)
{
  int ret = ioctl(fd, DIOCGMEDIASIZE, psize);
  return ret;
}

#else
# error "Unable to query block device size: unsupported platform, please report."
#endif

/* ocaml/ocaml/unixsupport.c */
extern void uerror(char *cmdname, value cmdarg);
#define Nothing ((value) 0)

CAMLprim value stub_blkgetsize(value filename){
  CAMLparam1(filename);
  CAMLlocal1(result);
  uint64_t size_in_bytes;
  int fd;
  int success = -1;

  const char *filename_c = strdup(String_val(filename));

  enter_blocking_section();
  fd = open(filename_c, O_RDONLY, 0);
  if (blkgetsize(fd, &size_in_bytes) == 0)
    success = 0;
  close(fd);
  leave_blocking_section();

  free((void*)filename_c);

  if (fd == -1) uerror("open", filename);
  if (success == -1) uerror("BLKGETSIZE", filename);

  result = caml_copy_int64(size_in_bytes);
  CAMLreturn(result);
}
