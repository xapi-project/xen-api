/*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <string.h>
#include <unistd.h> /* needed for _SC_OPEN_MAX */
#include <stdio.h> /* snprintf */
#include <sys/ioctl.h>
#include <sys/statvfs.h>
#if defined(__linux__)
# include <linux/fs.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

#if defined(__linux__)
# define TCP_LEVEL SOL_TCP
#elif defined(__APPLE__)
# define TCP_LEVEL IPPROTO_TCP
#else
# error "Don't know how to use setsockopt on this platform"
#endif

CAMLprim value stub_sockopt_set_sock_keepalives(value fd, value count, value idle, value interval)
{
    CAMLparam4(fd, count, idle, interval);

	int c_fd = Int_val(fd);
	int optval;
	socklen_t optlen=sizeof(optval);

	optval = Int_val(count);
	if(setsockopt(c_fd, TCP_LEVEL, TCP_KEEPCNT, &optval, optlen) < 0) {
	  uerror("setsockopt(TCP_KEEPCNT)", Nothing);
	}
#if defined(__linux__)
	optval = Int_val(idle);
	if(setsockopt(c_fd, TCP_LEVEL, TCP_KEEPIDLE, &optval, optlen) < 0) {
	  uerror("setsockopt(TCP_KEEPIDLE)", Nothing);
	}
#endif
	optval = Int_val(interval);
	if(setsockopt(c_fd, TCP_LEVEL, TCP_KEEPINTVL, &optval, optlen) < 0) {
	  uerror("setsockopt(TCP_KEEPINTVL)", Nothing);
	}

	CAMLreturn(Val_unit);
}

