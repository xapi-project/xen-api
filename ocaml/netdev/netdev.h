/*
 * Copyright (c) 2006 XenSource Inc.
 * Author: Vincent Hanquez <vincent@xensource.com>
 */

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/if.h>
#include <linux/sockios.h>

#ifndef SIOCBRADDBR
#include "sockios_compat.h"
#endif

#define CHECK_IOCTL(err, S)	\
	if (err < 0) {		\
		caml_failwith(S ": ioctl failed");	\
	}
