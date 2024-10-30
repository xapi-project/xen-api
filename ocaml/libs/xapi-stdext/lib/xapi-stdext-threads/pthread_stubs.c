/*
 * Copyright (C) Cloud Software Group
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

#include <pthread.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include "pthread_helpers.h"

#define NAMELEN 16

CAMLprim value stub_set_name(value name){
  CAMLparam1(name);
  CAMLlocal1(ret);
  ret = Val_unit;
  int rc;

  char *c_name = strdup(String_val(name));

  caml_enter_blocking_section();
  rc = set_name(c_name);
  free(c_name);
  caml_leave_blocking_section();

  if (rc != 0)
    unix_error(rc, "pthread_setname_np", Nothing);

  CAMLreturn(ret);
}

CAMLprim value stub_get_name(value unit){
  CAMLparam1(unit);
  CAMLlocal1(result);
  char thread_name[NAMELEN];

  caml_enter_blocking_section();
  int rc = get_name(thread_name, NAMELEN);
  caml_leave_blocking_section();

  if (rc != 0)
    unix_error(rc, "pthread_getname_np", Nothing);

  result = caml_copy_string(thread_name);
  CAMLreturn(result);
}
