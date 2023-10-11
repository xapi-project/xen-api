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

/* must be at the beginning, it affects defines in other headers that cannot be reenabled later */
#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>

#include "xa_auth.h"

CAMLprim value stub_XA_mh_authorize(value username, value password){
    CAMLparam2(username, password);
    CAMLlocal1(ret);
    ret = Val_unit;

    char *c_username = strdup(String_val(username));
    char *c_password = strdup(String_val(password));
    const char *error = NULL;
    int rc;

    caml_enter_blocking_section();
    rc = XA_mh_authorize(c_username, c_password, &error);
    free(c_username);
    free(c_password);
    caml_leave_blocking_section();

    if (rc != XA_SUCCESS)
        caml_failwith(error ? error : "Unknown error");
    CAMLreturn(ret);
}

CAMLprim value stub_XA_mh_chpasswd(value username, value new_password){
    CAMLparam2(username, new_password);
    CAMLlocal1(ret);
    ret = Val_unit;

    char *c_username = strdup(String_val(username));
    char *c_new_password = strdup(String_val(new_password));
    const char *error = NULL;
    int rc;

    caml_enter_blocking_section();
    rc = XA_mh_chpasswd (c_username, c_new_password, &error);
    free(c_username);
    free(c_new_password);
    caml_leave_blocking_section();

    if (rc != XA_SUCCESS)
        caml_failwith(error ? error : "Unknown error");
    CAMLreturn(ret);
}

CAMLprim value stub_XA_mh_authorize_start(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(result);
  pam_handle_t *handle;
  const char *error = NULL;

  result = caml_alloc(1, Abstract_tag);

  caml_enter_blocking_section();
  handle = XA_mh_authorize_start(&error);
  caml_leave_blocking_section();

  if (!handle)
    caml_failwith(error ? error : "Unknown error");

  *((pam_handle_t** ) Data_abstract_val(result)) = handle;
  CAMLreturn(result);
}

static pam_handle_t *pam_handle_of_val(value v)
{
 return *((pam_handle_t **) Data_abstract_val(v)); 
}

CAMLprim value stub_XA_mh_authorize_stop(value handle)
{
  CAMLparam1(handle);
  const char* error = NULL;
  int rc;
  pam_handle_t *pamh = pam_handle_of_val(handle);

  if (!pamh)
    CAMLreturn(Val_unit);/* already cleaned up, e.g. in error handler in _run */

  /* guard against use after free and double free */
  *((pam_handle_t** ) Data_abstract_val(handle)) = NULL;

  caml_enter_blocking_section();
  rc = XA_mh_authorize_stop(pamh, &error);
  caml_leave_blocking_section();

  if (XA_SUCCESS != rc)
    caml_failwith(error ? error : "Unknown error");

  CAMLreturn(Val_unit);
}

CAMLprim value stub_XA_mh_authorize_run(value ml_handle, value username, value password){
    CAMLparam2(username, password);
    CAMLlocal1(ret);
    pam_handle_t *handle = pam_handle_of_val(ml_handle);
    ret = Val_unit;

    char *c_username = strdup(String_val(username));
    char *c_password = strdup(String_val(password));
    const char *error = NULL;
    int rc;

    caml_enter_blocking_section();
    rc = XA_mh_authorize_run(&handle, c_username, c_password, &error);
    free(c_username);
    free(c_password);
    caml_leave_blocking_section();

    /* must only be done with the runtime lock held again */
    *((pam_handle_t** ) Data_abstract_val(ml_handle)) = handle;

    if (rc != XA_SUCCESS)
        caml_failwith(error ? error : "Unknown error");
    CAMLreturn(ret);
}

#include <crypt.h>
CAMLprim value stub_XA_workaround(value u)
{
  CAMLparam1(u);
  struct crypt_data data;
  memset(&data, 0, sizeof(data));

  /* When called with '$6$' it will call sha512_crypt_r which will call NSSLOW_Init, which initializes the library,
     and avoids the sleep() call that would otherwise happen when the library is initialized in parallel.
     We don't want to link with libfreebl3 directly, because in the future we might switch to using libxcrypt.
   */
  crypt_r("", "$6$", &data);
  
  CAMLreturn(Val_unit);
}

/*
 * Local variables:
 * mode: C
 * c-set-style: "BSD"
 * c-basic-offset: 4
 * tab-width: 4
 * indent-tabs-mode: nil
 * End:
 */

