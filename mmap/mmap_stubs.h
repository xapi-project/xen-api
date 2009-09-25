
#ifndef C_MMAP_H
#define C_MMAP_H

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

struct mmap_interface
{
	void *addr;
	int len;
};

#endif
