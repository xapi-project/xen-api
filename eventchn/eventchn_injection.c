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
#ifdef WITH_INJECTION_CAPABILITY
#include "../fake/marshall.h"
#include "../fake/using.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

static int fake_eventchn_open(void)
{
	struct sockaddr_un remote;
	char *s;
	int fd, len;

	s = getenv("XIU");
	if (!s)
		return -1;
	snprintf(remote.sun_path, 256, "%s-ev", s);
	remote.sun_family = AF_UNIX;
	len = strlen(remote.sun_path) + sizeof(remote.sun_family);

	fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (fd == -1)
		return -1;
	if (connect(fd, (struct sockaddr *)&remote, len) != 0)
		return -1;

	return fd;
}

static int fake_eventchn_ioctl(int handle, int cmd, void *arg)
{
	switch (cmd) {
	case IOCTL_EVTCHN_NOTIFY:
		marshall_command(handle, "ioctl,notify,%d\n",
		                 ((struct ioctl_evtchn_notify *) arg)->port);
		return unmarshall_return(handle);
	case IOCTL_EVTCHN_BIND_INTERDOMAIN:
		marshall_command(handle, "ioctl,bind_interdomain,%d,%d\n",
		                 ((struct ioctl_evtchn_bind_interdomain *) arg)->remote_domain,
		                 ((struct ioctl_evtchn_bind_interdomain *) arg)->remote_port);
		return unmarshall_return(handle);
	case IOCTL_EVTCHN_BIND_VIRQ:
		marshall_command(handle, "ioctl,bind_virq,%d\n",
		                 ((struct ioctl_evtchn_bind_virq *) arg)->virq);
		return unmarshall_return(handle);
	case IOCTL_EVTCHN_UNBIND:
		marshall_command(handle, "ioctl,unbind,%d\n",
		                 ((struct ioctl_evtchn_unbind *) arg)->port);
		return unmarshall_return(handle);
	default:
		return -EINVAL;
	}
}

static int fake_eventchn_read_port(int handle, evtchn_port_t *port)
{
	int ret;

	marshall_command(handle, "read\n");
	ret = unmarshall_int(handle);
	*port = ret;
	return unmarshall_return(handle);
}

static int fake_eventchn_write_port(int handle, evtchn_port_t port)
{
	marshall_command(handle, "write,%d\n", port);
	return unmarshall_return(handle);
}

#define pre_eventchn_open()          if (using_injection()) return fake_eventchn_open();
#define pre_eventchn_ioctl(h,c,a)    if (using_injection()) return fake_eventchn_ioctl(h,c,a);
#define pre_eventchn_read_port(h,p)  if (using_injection()) return fake_eventchn_read_port(h,p);
#define pre_eventchn_write_port(h,p) if (using_injection()) return fake_eventchn_write_port(h,p);
#else
#define pre_eventchn_open()          do {} while(0);
#define pre_eventchn_ioctl(h,c,a)    do {} while(0);
#define pre_eventchn_read_port(h,p)  do {} while(0);
#define pre_eventchn_write_port(h,p) do {} while(0);
#endif
