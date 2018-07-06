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
/* Query CDROM info */
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <linux/cdrom.h>

static int CDROM_DRIVE_STATUS_list[] = {
	CDS_NO_INFO,
	CDS_NO_DISC,
	CDS_TRAY_OPEN,
	CDS_DRIVE_NOT_READY,
	CDS_DISC_OK,
};

static int CDROM_DISC_STATUS_list[] = {
	CDS_NO_INFO,
	CDS_NO_DISC,

	CDS_AUDIO,
	CDS_DATA_1,
	CDS_DATA_2,
	CDS_XA_2_1,
	CDS_XA_2_2,
	CDS_MIXED,
};

#define make_ioctl_stub(X)                                                              \
CAMLprim value stub_##X (value fd)                                                      \
{                                                                                       \
	CAMLparam1 (fd);                                                                \
	int result = -1;                                                                \
	int i;                                                                          \
	int c_fd = Int_val(fd);                                                         \
	int c_res = ioctl(c_fd, X);                                                     \
                                                                                        \
	if (c_res < 0) failwith_errno();                                                \
                                                                                        \
	for (i = 0; i < sizeof(X##_list); i++){                                         \
		if (X##_list[i] == c_res){                                              \
			result = i;                                                     \
			break;                                                          \
		}                                                                       \
	}                                                                               \
	if (result < 0) caml_failwith("Failed to understand result of " #X " ioctl");   \
	CAMLreturn(Val_int(result));                                                    \
}

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

static void failwith_errno(void)
{
        char buf[256];
	char buf2[280];
	memset(buf, '\0', sizeof(buf));
	strerror_r(errno, buf, sizeof(buf));
	snprintf(buf2, sizeof(buf2), "errno: %d msg: %s", errno, buf);
	caml_failwith(buf2);
}

make_ioctl_stub(CDROM_DRIVE_STATUS);
make_ioctl_stub(CDROM_DISC_STATUS);

CAMLprim value stub_CDROM_GET_MCN (value fd)                                            
{
	CAMLparam1 (fd);
	CAMLlocal1 (result);
	struct cdrom_mcn mcn;
	char *mcnptr;

	int c_fd = Int_val(fd);                                                         
	int c_res = ioctl(c_fd, CDROM_GET_MCN, &mcn);                                                     
	if (c_res < 0) failwith_errno();                                                

	mcnptr = (char *) mcn.medium_catalog_number;
	result = caml_copy_string(mcnptr);

	CAMLreturn(result);
}
