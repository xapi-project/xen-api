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
/*
 */

#include <ifaddrs.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>

static value alloc_addr(struct sockaddr *sock)
{
	CAMLparam0();
	CAMLlocal1(result);
	char output[40];
	int ret = 0;

	switch (sock->sa_family) {
		case AF_INET: {
			struct sockaddr_in *in = (struct sockaddr_in *) sock;
			int v = ntohl(in->sin_addr.s_addr);
			ret = snprintf(output, sizeof(output), "%u.%u.%u.%u",
				(v >> 24) & 0xff, (v >> 16) & 0xff,
				(v >> 8) & 0xff, v & 0xff);
			break;
		}
		case AF_INET6: {
			struct sockaddr_in6 *in6 = (struct sockaddr_in6 *) sock;
			ret = snprintf(output, sizeof(output),
			        "%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x",
				in6->sin6_addr.s6_addr[0],
				in6->sin6_addr.s6_addr[1],
				in6->sin6_addr.s6_addr[2],
				in6->sin6_addr.s6_addr[3],
				in6->sin6_addr.s6_addr[4],
				in6->sin6_addr.s6_addr[5],
				in6->sin6_addr.s6_addr[6],
				in6->sin6_addr.s6_addr[7],
				in6->sin6_addr.s6_addr[8],
				in6->sin6_addr.s6_addr[9],
				in6->sin6_addr.s6_addr[10],
				in6->sin6_addr.s6_addr[11],
				in6->sin6_addr.s6_addr[12],
				in6->sin6_addr.s6_addr[13],
				in6->sin6_addr.s6_addr[14],
				in6->sin6_addr.s6_addr[15]);
			break;
		}
		default:
			/* just ignore */
			;
	}
	result = caml_alloc_string(ret);
	memcpy(String_val(result), output, ret);
	CAMLreturn(result);
}

value stub_if_getaddr(value unit)
{
	CAMLparam0();
	CAMLlocal5(result, temp, name, addrstr, netmaskstr);
	CAMLlocal1(tuple);
	int ret;
	struct ifaddrs *ifaddrs, *tmp;
	struct sockaddr *sock, *netmask;

	result = temp = Val_emptylist;
	name = addrstr = Val_int(0);

	ret = getifaddrs(&ifaddrs);
	if (ret < 0)
		caml_failwith("cannot get interface address");

	for (tmp = ifaddrs; tmp; tmp = tmp->ifa_next) {
		sock = tmp->ifa_addr;
		netmask = tmp->ifa_netmask;

		if ((sock && netmask) &&
		    (sock->sa_family == AF_INET || sock->sa_family == AF_INET6)) {
			name = caml_copy_string(tmp->ifa_name);
			addrstr = alloc_addr(sock);
			netmaskstr = alloc_addr(netmask);

			tuple = caml_alloc_tuple(4);
			Store_field(tuple, 0, name);
			Store_field(tuple, 1, addrstr);
			Store_field(tuple, 2, netmaskstr);
			Store_field(tuple, 3, Val_bool(sock->sa_family == AF_INET6));

			result = caml_alloc_small(2, Tag_cons);
			Field(result, 0) = tuple;
			Field(result, 1) = temp;

			temp = result;
		}
	}

	freeifaddrs(ifaddrs);

	CAMLreturn(result);
}
