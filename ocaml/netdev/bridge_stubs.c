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

#include "netdev.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>

value stub_bridge_add(value fd, value name)
{
	CAMLparam2(fd, name);
	int err;

	err = ioctl(Int_val(fd), SIOCBRADDBR, String_val(name));
	CHECK_IOCTL(err, "bridge add");
	CAMLreturn(Val_unit);
}

value stub_bridge_del(value fd, value name)
{
	CAMLparam2(fd, name);
	int err;

	err = ioctl(Int_val(fd), SIOCBRDELBR, String_val(name));
	CHECK_IOCTL(err, "bridge del");
	CAMLreturn(Val_unit);
}

value stub_bridge_intf_add(value fd, value name, value intf)
{
	CAMLparam3(fd, name, intf);
	int err;
	struct ifreq ifr;
	int ifindex;

	ifindex = if_nametoindex(String_val(intf));
	if (ifindex == 0)
		caml_failwith("Device_not_found");

	memset(ifr.ifr_name, '\000', IFNAMSIZ);
	strncpy(ifr.ifr_name, String_val(name), IFNAMSIZ);
	ifr.ifr_ifindex = ifindex;

	err = ioctl(Int_val(fd), SIOCBRADDIF, &ifr);
	CHECK_IOCTL(err, "bridge intf add");
	CAMLreturn(Val_unit);
}

value stub_bridge_intf_del(value fd, value name, value intf)
{
	CAMLparam3(fd, name, intf);
	int err;
	struct ifreq ifr;
	int ifindex;

	ifindex = if_nametoindex(String_val(intf));
	if (ifindex == 0)
		caml_failwith("Device_not_found");

	memset(ifr.ifr_name, '\000', IFNAMSIZ);
	strncpy(ifr.ifr_name, String_val(name), IFNAMSIZ);
	ifr.ifr_ifindex = ifindex;

	err = ioctl(Int_val(fd), SIOCBRDELIF, &ifr);
	CHECK_IOCTL(err, "bridge intf del");

	CAMLreturn(Val_unit);
}
