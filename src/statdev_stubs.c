/*
 * Copyright (C) 2007 Xensource Ltd.
 * Author Vincent Hanquez <vincent@xensource.com>
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

#define MINORBITS	20
#define MINORMASK	((1U << MINORBITS) - 1)

#define MAJOR(dev)	((unsigned int) ((dev) >> MINORBITS))
#define MINOR(dev)	((unsigned int) ((dev) & MINORMASK))

value stub_statdev_get_major_minor(value dpath)
{
	CAMLparam1(dpath);
	CAMLlocal2(majmin, errno_value);
	struct stat statbuf;
	unsigned major, minor;
	int ret;

	errno_value = Val_int(0);

	ret = stat(String_val(dpath), &statbuf);
	if (ret == -1) 
		errno_value = Val_int(errno);

	major = (statbuf.st_rdev & 0xfff00) >> 8;
	minor = (statbuf.st_rdev & 0xff) | ((statbuf.st_rdev >> 12) & 0xfff00);

	majmin = caml_alloc_tuple(3);
	Store_field(majmin, 0, errno_value);
	Store_field(majmin, 1, Val_int(major));
	Store_field(majmin, 2, Val_int(minor));
	CAMLreturn(majmin);
}
