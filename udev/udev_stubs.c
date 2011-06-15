/*
 * Copyright (C) 2011    Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
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

#define _GNU_SOURCE
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <bits/socket.h>
#include <linux/netlink.h>
#include <arpa/inet.h>
#include <linux/filter.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>

CAMLprim value stub_socket_netlink_udev(value unit)
{
	CAMLparam1(unit);
	int fd;

	fd = socket(AF_NETLINK, SOCK_RAW, NETLINK_KOBJECT_UEVENT);
	if (fd == -1)
		caml_failwith("cannot open netlink socket");
	if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) {
		close(fd);
		caml_failwith("cannot set cloexec netlink socket");
	}
	CAMLreturn(Val_int(fd));
}

CAMLprim value stub_bind_netlink_udev(value fd)
{
	CAMLparam1(fd);
	int c_fd = Int_val(fd);
	int err;
	struct sockaddr_nl snl;
	const int on = 1;

	snl.nl_family = AF_NETLINK;
	snl.nl_groups = 1;

	err = bind(c_fd, (struct sockaddr *) &snl, sizeof(struct sockaddr_nl));
	if (err != 0) {
		caml_failwith("bind netlink socket");
	}

	setsockopt(c_fd, SOL_SOCKET, SO_PASSCRED, &on, sizeof(on));
	CAMLreturn(Val_unit);
}

CAMLprim value stub_receive_events_udev(value fd)
{
	CAMLparam1(fd);
	CAMLlocal1(event);
	struct iovec iov;
	struct msghdr msg;
	ssize_t buflen;
	char buf[8192];
	char cred_msg[CMSG_SPACE(sizeof(struct ucred))];
	struct ucred *cred;
	struct sockaddr_nl snl;
	socklen_t addrlen;
	int c_fd = Int_val(fd);

	addrlen = sizeof(struct sockaddr_nl);
	getsockname(c_fd, (struct sockaddr *) &snl, &addrlen); 
	snl.nl_family = AF_NETLINK;
	snl.nl_groups = 1;

	memset(buf, 0x0, sizeof(buf));
	iov.iov_base = &buf;
	iov.iov_len = sizeof(buf);
	memset(&msg, 0x0, sizeof(struct msghdr));
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;
	msg.msg_control = cred_msg;
	msg.msg_controllen = sizeof(cred_msg);
	msg.msg_name = &snl;
	msg.msg_namelen = sizeof(snl);

	buflen = recvmsg(c_fd, &msg, 0);
	if (buflen < 0) {
		caml_failwith("buflen is negative");
	}

	if (buflen < 32 || (size_t) buflen >= sizeof(buf)) {
		caml_failwith("buflen is not correct");
	}

	cred = (struct ucred *) CMSG_FIRSTHDR(&msg);
	if (cred->uid != 0) {
		caml_failwith("udev event with uid != 0 ignored");
	}

	event = caml_copy_string(buf);

	CAMLreturn(event);
}

/*
 * Local variables:
 *  indent-tabs-mode: t
 *  c-basic-offset: 8
 *  tab-width: 8
 */
