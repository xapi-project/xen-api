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
#include <caml/threads.h>

/* Set the TCP_NODELAY flag on a Unix.file_descr */
CAMLprim value stub_unixext_set_tcp_nodelay (value fd, value bool)
{
	CAMLparam2 (fd, bool);
	int c_fd = Int_val(fd);
	int opt = (Bool_val(bool)) ? 1 : 0;
	if (setsockopt(c_fd, IPPROTO_TCP, TCP_NODELAY, (void *)&opt, sizeof(opt)) != 0){
		uerror("setsockopt", Nothing);
	}
	CAMLreturn(Val_unit);
}

CAMLprim value stub_unixext_fsync (value fd)
{
	CAMLparam1(fd);
	int c_fd = Int_val(fd);
	int rc;

	caml_release_runtime_system();
	rc = fsync(c_fd);
	caml_acquire_runtime_system();
	if (rc != 0) uerror("fsync", Nothing);
	CAMLreturn(Val_unit);
}
	
extern uint64_t stdext_blkgetsize(int fd, uint64_t *psize);

CAMLprim value stub_unixext_blkgetsize64(value fd)
{
  CAMLparam1(fd);
  uint64_t size;
  int c_fd = Int_val(fd);
  int rc;

  caml_release_runtime_system();
  /* mirage-block-unix binding: */
  rc = stdext_blkgetsize(c_fd, &size);
  caml_acquire_runtime_system();

  if (rc) {
    uerror("ioctl(BLKGETSIZE64)", Nothing);
  }
  CAMLreturn(caml_copy_int64(size));
}

CAMLprim value stub_unixext_get_max_fd (value unit)
{
	CAMLparam1 (unit);
	long maxfd;
	maxfd = sysconf(_SC_OPEN_MAX);
	CAMLreturn(Val_int(maxfd));
}

#if defined(__linux__)
# define TCP_LEVEL SOL_TCP
#elif defined(__APPLE__)
# define TCP_LEVEL IPPROTO_TCP
#else
# error "Don't know how to use setsockopt on this platform"
#endif

CAMLprim value stub_unixext_set_sock_keepalives(value fd, value count, value idle, value interval)
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

#define FDSET_OF_VALUE(v) (&(((struct fdset_t *) v)->fds))
#define MAXFD_OF_VALUE(v) (((struct fdset_t *) v)->max)
struct fdset_t { fd_set fds; int max; };

CAMLprim value stub_fdset_of_list(value l)
{
	CAMLparam1(l);
	CAMLlocal1(set);

	set = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	FD_ZERO(FDSET_OF_VALUE(set));
	MAXFD_OF_VALUE(set) = -1;
	while (l != Val_int(0)) {
		int fd;
		fd = Int_val(Field(l, 0));
		FD_SET(fd, FDSET_OF_VALUE(set));
		if (fd > MAXFD_OF_VALUE(set))
			MAXFD_OF_VALUE(set) = fd;
		l = Field(l, 1);
	}
	CAMLreturn(set);
}

CAMLprim value stub_fdset_is_set(value set, value fd)
{
	CAMLparam2(set, fd);
	CAMLreturn(Val_bool(FD_ISSET(Int_val(fd), FDSET_OF_VALUE(set))));
}

CAMLprim value stub_fdset_set(value set, value fd)
{
	CAMLparam2(set, fd);
	FD_SET(Int_val(fd), FDSET_OF_VALUE(set));
	CAMLreturn(Val_unit);
}

CAMLprim value stub_fdset_clear(value set, value fd)
{
	CAMLparam2(set, fd);
	FD_CLR(Int_val(fd), FDSET_OF_VALUE(set));
	CAMLreturn(Val_unit);
}

CAMLprim value stub_fdset_is_set_and_clear(value set, value fd)
{
	CAMLparam2(set, fd);
	int r, c_fd;
	fd_set *c_set;	

	c_fd = Int_val(fd);
	c_set = FDSET_OF_VALUE(set);
	r = FD_ISSET(c_fd, c_set);
	if (r)
		FD_CLR(c_fd, c_set);
	CAMLreturn(Val_bool(r));
}

void unixext_error(int code)
{
	static value *exn = NULL;

	if (!exn) {
		exn = caml_named_value("unixext.unix_error");
		if (!exn)
			caml_invalid_argument("unixext.unix_error not initialiazed");
	}
	caml_raise_with_arg(*exn, Val_int(code));
}

CAMLprim value stub_fdset_select(value rset, value wset, value eset, value t)
{
	CAMLparam4(rset, wset, eset, t);
	CAMLlocal4(ret, nrset, nwset, neset);
	fd_set r, w, e;
	int maxfd;
	double tm;
	struct timeval tv;
	struct timeval *tvp;
	int v;

	memcpy(&r, FDSET_OF_VALUE(rset), sizeof(fd_set));
	memcpy(&w, FDSET_OF_VALUE(wset), sizeof(fd_set));
	memcpy(&e, FDSET_OF_VALUE(eset), sizeof(fd_set));

	maxfd = (MAXFD_OF_VALUE(rset) > MAXFD_OF_VALUE(wset))
		? MAXFD_OF_VALUE(rset)
		: MAXFD_OF_VALUE(wset);
	maxfd = (maxfd > MAXFD_OF_VALUE(eset)) ? maxfd : MAXFD_OF_VALUE(eset);

	tm = Double_val(t);
	if (tm < 0.0)
		tvp = NULL;
	else {
		tv.tv_sec = (int) tm;
		tv.tv_usec = (int) (1e6 * (tm - tv.tv_sec));
		tvp = &tv;
	}

	caml_enter_blocking_section();
	v = select(maxfd + 1, &r, &w, &e, tvp);
	caml_leave_blocking_section();
	if (v == -1)
		unixext_error(errno);

	nrset = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	nwset = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	neset = caml_alloc(sizeof(struct fdset_t), Abstract_tag);

	memcpy(FDSET_OF_VALUE(nrset), &r, sizeof(fd_set));
	memcpy(FDSET_OF_VALUE(nwset), &w, sizeof(fd_set));
	memcpy(FDSET_OF_VALUE(neset), &e, sizeof(fd_set));
	
	ret = caml_alloc_small(3, 0);
	Field(ret, 0) = nrset;
	Field(ret, 1) = nwset;
	Field(ret, 2) = neset;

	CAMLreturn(ret);
}

CAMLprim value stub_fdset_select_ro(value rset, value t)
{
	CAMLparam2(rset, t);
	CAMLlocal1(ret);
	fd_set r;
	int maxfd;
	double tm;
	struct timeval tv;
	struct timeval *tvp;
	int v;

	memcpy(&r, FDSET_OF_VALUE(rset), sizeof(fd_set));
	maxfd = MAXFD_OF_VALUE(rset);

	tm = Double_val(t);
	if (tm < 0.0)
		tvp = NULL;
	else {
		tv.tv_sec = (int) tm;
		tv.tv_usec = (int) (1e6 * (tm - tv.tv_sec));
		tvp = &tv;
	}

	caml_enter_blocking_section();
	v = select(maxfd + 1, &r, NULL, NULL, tvp);
	caml_leave_blocking_section();
	if (v == -1)
		unixext_error(errno);

	ret = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	memcpy(FDSET_OF_VALUE(ret), &r, sizeof(fd_set));
	
	CAMLreturn(ret);
}

CAMLprim value stub_fdset_select_wo(value wset, value t)
{
	CAMLparam2(wset, t);
	CAMLlocal1(ret);
	fd_set w;
	int maxfd;
	double tm;
	struct timeval tv;
	struct timeval *tvp;
	int v;

	memcpy(&w, FDSET_OF_VALUE(wset), sizeof(fd_set));
	maxfd = MAXFD_OF_VALUE(wset);

	tm = Double_val(t);
	if (tm < 0.0)
		tvp = NULL;
	else {
		tv.tv_sec = (int) tm;
		tv.tv_usec = (int) (1e6 * (tm - tv.tv_sec));
		tvp = &tv;
	}

	caml_enter_blocking_section();
	v = select(maxfd + 1, NULL, &w, NULL, tvp);
	caml_leave_blocking_section();
	if (v == -1)
		unixext_error(errno);

	ret = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	memcpy(FDSET_OF_VALUE(ret), &w, sizeof(fd_set));
	
	CAMLreturn(ret);
}

CAMLprim value stub_fdset_is_empty(value set)
{
	CAMLparam1(set);
	fd_set x;
	int ret;
	FD_ZERO(&x);
	ret = memcmp(&x, FDSET_OF_VALUE(set), sizeof(fd_set));
	
	CAMLreturn(Bool_val(ret == 0));
}

CAMLprim value stub_statvfs(value filename) 
{
  CAMLparam1(filename);
  CAMLlocal1(v);
  int ret;
  struct statvfs buf;

  /* We want to release the runtime lock, so we must copy
   * all OCaml arguments.
   * See the manual section 20.12.2 Parallel execution of long running C code */
  char *name = caml_stat_strdup(String_val(filename));

  caml_release_runtime_system();
  ret = statvfs(name, &buf);
  caml_stat_free(name);
  caml_acquire_runtime_system();

  if(ret == -1) uerror("statvfs", Nothing);

  v=caml_alloc(11,0);
  Store_field(v, 0, caml_copy_int64(buf.f_bsize));
  Store_field(v, 1, caml_copy_int64(buf.f_frsize));
  Store_field(v, 2, caml_copy_int64(buf.f_blocks));
  Store_field(v, 3, caml_copy_int64(buf.f_bfree));
  Store_field(v, 4, caml_copy_int64(buf.f_bavail));
  Store_field(v, 5, caml_copy_int64(buf.f_files));
  Store_field(v, 6, caml_copy_int64(buf.f_ffree));
  Store_field(v, 7, caml_copy_int64(buf.f_favail));
  Store_field(v, 8, caml_copy_int64(buf.f_fsid));
  Store_field(v, 9, caml_copy_int64(buf.f_flag));
  Store_field(v,10, caml_copy_int64(buf.f_namemax));

  CAMLreturn(v);
}
