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
#ifndef _WIN32
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <sys/statvfs.h>
#endif
#include <errno.h>
#include <string.h>
#include <unistd.h> /* needed for _SC_OPEN_MAX */
#include <stdio.h> /* snprintf */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

void raise_error(int code)
{
	static value *exn = NULL;

	if (!exn) {
		exn = caml_named_value("fd_send_recv.unix_error");
		if (!exn)
			caml_invalid_argument("fd_send_recv.unix_error not initialiazed");
	}
	caml_raise_with_arg(*exn, Val_int(code));
}

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

#define UNIX_BUFSIZ 16384

CAMLprim value stub_unix_send_fd(value sock, value buff, value ofs, value len, value flags, value fd)
{
  CAMLparam5(sock,buff,ofs,len,flags);
  CAMLxparam1(fd);
  int ret,  cv_flags, cfd;
#ifndef _WIN32
  long numbytes;
  char iobuf[UNIX_BUFSIZ];
  char buf[CMSG_SPACE(sizeof(cfd))];

  cfd = Int_val(fd);

  cv_flags = convert_flag_list(flags,msg_flag_table);

  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFSIZ) numbytes = UNIX_BUFSIZ;
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

  if(ret == -1) {
    perror("sendmsg");
    raise_error(errno);
  }
#else
  caml_failwith("stub_unix_send_fd not implementable on Win32");
#endif
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
#ifndef _WIN32
  int ret,  cv_flags, fd;
  long numbytes;
  char iobuf[UNIX_BUFSIZ];
  char buf[CMSG_SPACE(sizeof(fd))];
  struct sockaddr_un unix_socket_name;

  cv_flags = convert_flag_list(flags,msg_flag_table);

  struct msghdr msg;
  struct iovec vec;
  struct cmsghdr *cmsg;

  numbytes = Long_val(len);
  if(numbytes > UNIX_BUFSIZ)
    numbytes = UNIX_BUFSIZ;

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

  if(ret == -1) {
    perror("recvmsg");
    raise_error(errno);
  }

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
#else
  caml_failwith("stub_unix_recv_fd not implementable on Win32");
#endif
  CAMLreturn(res);
}
