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
#include <linux/fs.h> 

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

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
	if (fsync(c_fd) != 0) uerror("fsync", Nothing);
	CAMLreturn(Val_unit);
}
	
CAMLprim value stub_unixext_blkgetsize64(value fd)
{
  CAMLparam1(fd);
  uint64_t size;
  int c_fd = Int_val(fd);
  if(ioctl(c_fd,BLKGETSIZE64,&size)) {
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

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

#define UNIX_BUFFER_SIZE 16384

CAMLprim value stub_unix_send_fd(value sock, value buff, value ofs, value len, value flags, value fd)
{
  CAMLparam5(sock,buff,ofs,len,flags);
  CAMLxparam1(fd);
  int ret,  cv_flags, cfd;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  char buf[CMSG_SPACE(sizeof(cfd))];

  cfd = Int_val(fd);

  cv_flags = convert_flag_list(flags,msg_flag_table);

  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove(iobuf, &Byte(buff, Long_val(ofs)), numbytes);

  /* Set up sockaddr */

  struct msghdr msg;
  struct iovec vec;
  struct cmsghdr *cmsg;
  
  msg.msg_name = NULL;
  msg.msg_namelen = 0; 
  vec.iov_base=iobuf;
  vec.iov_len=numbytes;
  msg.msg_iov=&vec;
  msg.msg_iovlen=1;

  msg.msg_control = buf;
  msg.msg_controllen = sizeof(buf);
  cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = SOL_SOCKET;
  cmsg->cmsg_type = SCM_RIGHTS;
  cmsg->cmsg_len = CMSG_LEN(sizeof(cfd));
  *(int*)CMSG_DATA(cmsg) = cfd;
  msg.msg_controllen = cmsg->cmsg_len;

  msg.msg_flags = 0;

  caml_enter_blocking_section();  
  ret=sendmsg(Int_val(sock), &msg, cv_flags);
  caml_leave_blocking_section();

  if(ret == -1)
    unixext_error(errno);

  CAMLreturn(Val_int(ret));
}

CAMLprim value stub_unix_send_fd_bytecode(value *argv, int argn) 
{
  return stub_unix_send_fd(argv[0],argv[1],argv[2],argv[3],
                        argv[4], argv[5]);
}

CAMLprim value stub_unix_recv_fd(value sock, value buff, value ofs, value len, value flags) 
{
  CAMLparam5(sock,buff,ofs,len,flags);
  CAMLlocal2(res,addr);
  int ret,  cv_flags, fd;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  char buf[CMSG_SPACE(sizeof(fd))];
  struct sockaddr_un unix_socket_name;

  cv_flags = convert_flag_list(flags,msg_flag_table);

  struct msghdr msg;
  struct iovec vec;
  struct cmsghdr *cmsg;

  numbytes = Long_val(len);
  if(numbytes > UNIX_BUFFER_SIZE)
    numbytes = UNIX_BUFFER_SIZE;

  msg.msg_name=&unix_socket_name;
  msg.msg_namelen=sizeof(unix_socket_name);
  vec.iov_base=iobuf;
  vec.iov_len=numbytes;
  msg.msg_iov=&vec;

  msg.msg_iovlen=1;

  msg.msg_control = buf;
  msg.msg_controllen = sizeof(buf);

  caml_enter_blocking_section();  
  ret=recvmsg(Int_val(sock), &msg, cv_flags);
  caml_leave_blocking_section();

  if(ret == -1) 
    unixext_error(errno);

  if(ret>0 && msg.msg_controllen>0) {
    cmsg = CMSG_FIRSTHDR(&msg);
    if(cmsg->cmsg_level == SOL_SOCKET && (cmsg->cmsg_type == SCM_RIGHTS)) {
      fd=Val_int(*(int*)CMSG_DATA(cmsg));
    } else {
      failwith("Failed to receive an fd!");
    }
  } else {
    fd=Val_int(-1);
  }
  
  if(ret<numbytes)
    numbytes = ret;

  memmove(&Byte(buff, Long_val(ofs)), iobuf, numbytes);

  addr=alloc_small(1,0); /* Unix.sockaddr; must be an ADDR_UNIX of string */
  Field(addr, 0) = Val_unit; /* must set all fields before next allocation */

  if(ret>0) {
    Field(addr,0) = copy_string(unix_socket_name.sun_path);
  } else {
    Field(addr,0) = copy_string("nothing");
  }

  res=alloc_small(3,0);
  Field(res,0) = Val_int(ret);
  Field(res,1) = addr;
  Field(res,2) = fd;

  CAMLreturn(res);
}
