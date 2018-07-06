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

#define SET_IFREQ(ifreq, devname)		\
	strncpy(ifreq.ifr_name, devname, IFNAMSIZ)

static int link_change_flags(int fd, char *name,
                             unsigned int flags, unsigned int mask)
{
	struct ifreq ifr;
	int ret;

	SET_IFREQ(ifr, name);
	ret = ioctl(fd, SIOCGIFFLAGS, &ifr);
	if (ret < 0)
		return ret;
	if ((ifr.ifr_flags ^ flags) & mask) {
		ifr.ifr_flags &= ~mask;
		ifr.ifr_flags |= mask & flags;
		ret = ioctl(fd, SIOCSIFFLAGS, &ifr);
	}
	return ret;
}

static int link_change_name(int fd, char *name, char *newname)
{
	struct ifreq ifr;
	int ret;

	SET_IFREQ(ifr, name);
	strncpy(ifr.ifr_newname, newname, IFNAMSIZ);
	ret = ioctl(fd, SIOCSIFNAME, &ifr);
	return ret;
}

value stub_link_up(value fd, value dev)
{
	CAMLparam2(fd, dev);
	int err;
	err = link_change_flags(Int_val(fd), String_val(dev), IFF_UP, IFF_UP);
	CHECK_IOCTL(err, "link up");
	CAMLreturn(Val_unit);
}

value stub_link_is_up(value fd, value dev)
{
	CAMLparam2(fd, dev);
	struct ifreq ifr;
	int err;

	SET_IFREQ(ifr, String_val(dev));
	err = ioctl(Int_val(fd), SIOCGIFFLAGS, &ifr);
	CHECK_IOCTL(err, "link_is_up");
	CAMLreturn(Val_bool (ifr.ifr_flags & IFF_UP));
}

value stub_link_down(value fd, value dev)
{
	CAMLparam2(fd, dev);
	int err;
	err = link_change_flags(Int_val(fd), String_val(dev), 0, IFF_UP);
	CHECK_IOCTL(err, "link down");

	CAMLreturn(Val_unit);
}

value stub_link_change_name(value fd, value dev, value newname)
{
	CAMLparam3(fd, dev, newname);
	int err;

	err = link_change_name(Int_val(fd),
	                       String_val(dev), String_val(newname));
	CHECK_IOCTL(err, "link change name");
	CAMLreturn(Val_unit);
}

value stub_link_multicast(value fd, value dev, value v)
{
	CAMLparam3(fd, dev, v);
	int err;
	err = link_change_flags(Int_val(fd), String_val(dev),
	           ((Bool_val(v)) ? IFF_MULTICAST : 0), IFF_MULTICAST);
	CHECK_IOCTL(err, "link multicast");
	CAMLreturn(Val_unit);
}

value stub_link_arp(value fd, value dev, value v)
{
	CAMLparam3(fd, dev, v);
	int err;
	err = link_change_flags(Int_val(fd), String_val(dev),
	           ((Bool_val(v)) ? 0 : IFF_NOARP), IFF_NOARP);
	CHECK_IOCTL(err, "link arp");
	CAMLreturn(Val_unit);
}

#ifdef SIOCETHTOOL
#define ETHTOOL_GSET             0x00000001 /* Get settings. */

#include <stdint.h>
/* copied from linux/ethtool.h and made compilable with userspace types */
struct ethtool_cmd {
	uint32_t cmd;
	uint32_t supported;	/* Features this interface supports */
	uint32_t advertising;	/* Features this interface advertises */
	uint16_t speed;		/* The forced speed, 10Mb, 100Mb, gigabit */
	uint8_t	duplex;		/* Duplex, half or full */
	uint8_t	port;		/* Which connector port */
	uint8_t	phy_address;
	uint8_t	transceiver;	/* Which transceiver to use */
	uint8_t	autoneg;	/* Enable or disable autonegotiation */
	uint32_t maxtxpkt;	/* Tx pkts before generating tx int */
	uint32_t maxrxpkt;	/* Rx pkts before generating rx int */
	uint32_t reserved[4];
};

value stub_link_get_status(value fd, value dev)
{
	CAMLparam2(fd, dev);
	CAMLlocal1(ret);
	struct ifreq ifr;
	struct ethtool_cmd ecmd;
	int err, speed, duplex;

	SET_IFREQ(ifr, String_val(dev));
	ecmd.cmd = ETHTOOL_GSET;
	ifr.ifr_data = (caddr_t) &ecmd;
	err = ioctl(Int_val(fd), SIOCETHTOOL, &ifr);
	CHECK_IOCTL(err, "get ethtool");

	/* CA-24610: apparently speeds can be other values eg 2500 */
	speed = ecmd.speed;

	switch (ecmd.duplex) {
	case 0: duplex = 1; break;
	case 1: duplex = 2; break;
	default: duplex = 0;
	}

	ret = caml_alloc_tuple(2);
	Store_field(ret, 0, Val_int(speed));
	Store_field(ret, 1, Val_int(duplex));

	CAMLreturn(ret);
}
#else
value stub_link_get_status(value fd, value dev)
{
	CAMLparam2(fd, dev);
	CAMLlocal1(ret);
	ret = caml_alloc_tuple(2);
	Store_field(ret, 0, Val_int(0)); /* unknown speed */
	Store_field(ret, 1, Val_int(0)); /* unknown duplex */
	CAMLreturn(ret);
}
#endif
