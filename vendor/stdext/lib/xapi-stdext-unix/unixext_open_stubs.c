/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: open.c 9547 2010-01-22 12:48:24Z doligez $ */

#define _GNU_SOURCE /* O_DIRECT */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <string.h>
#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif

static int open_flag_table[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_NOCTTY, O_DSYNC, O_SYNC, O_RSYNC
};

CAMLprim value stub_stdext_unix_open_direct(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  int fd, cv_flags;
#ifndef O_DIRECT
  int ret;
#endif
  char * p;

  cv_flags = convert_flag_list(flags, open_flag_table);
 
#ifdef O_DIRECT
  cv_flags |= O_DIRECT;
#endif
  p = stat_alloc(string_length(path) + 1);
  strcpy(p, String_val(path));
  /* open on a named FIFO can block (PR#1533) */
  enter_blocking_section();
  fd = open(p, cv_flags, Int_val(perm));
#ifndef O_DIRECT
  if (fd != -1)
    ret = fcntl(fd, F_NOCACHE);
#endif
  leave_blocking_section();
  stat_free(p);
  if (fd == -1) uerror("open", path);
#ifndef O_DIRECT
  if (ret == -1) uerror("fcntl", path);
#endif

  CAMLreturn (Val_int(fd));
}
