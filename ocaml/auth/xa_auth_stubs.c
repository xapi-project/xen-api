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
#include <caml/threads.h>

#include "xa_auth.h"

CAMLprim value stub_XA_mh_authorize(value username, value password){
    CAMLparam2(username, password);
    CAMLlocal1(ret);
    ret = Val_unit;

    char *c_username = strdup(String_val(username));
    char *c_password = strdup(String_val(password));
    const char *error = NULL;
    int rc;

    caml_release_runtime_system();
    rc = XA_mh_authorize(c_username, c_password, &error);
    free(c_username);
    free(c_password);
    caml_acquire_runtime_system();

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

    caml_release_runtime_system();
    rc = XA_mh_chpasswd (c_username, c_new_password, &error);
    free(c_username);
    free(c_new_password);
    caml_acquire_runtime_system();

    if (rc != XA_SUCCESS)
        caml_failwith(error ? error : "Unknown error");
    CAMLreturn(ret);
}

#include <crypt.h>
/* 'constructor' attribute will ensure this function gets call early during program startup. */
void __attribute__((constructor)) stub_XA_workaround(void)
{
  struct crypt_data data;
  memset(&data, 0, sizeof(data));

  /* Initialize and load crypt library used for password hashing.
     This library is loaded and initialized at [pam_authenticate] time and not at [pam_start].
     If it detects a race condition (multiple threads with their own PAM contexts trying to call 'crypt_r'),
     then it does [sleep 1] as can be seen in this call trace:
     [pam_authenticate -> crypt_r -> __sha512_crypt_r -> freebl_InitVector -> freebl_RunLoaderOnce -> sleep].

     As a workaround link with 'libcrypt' and call 'crypt_r' with a setting that will make it take tha sha512 route
     to ensure that the library gets initialized while we're still single-threaded and stays loaded and initialized.

     '$6$' is the setting for sha512 according to crypt(5).

      Upstream has switched to using libxcrypt instead which doesn't have these problems, when we switch then
      this workaround can be dropped.
   */
  crypt_r("", "$6$", &data);
}

/* key:string -> setting:string -> string option */
CAMLprim value stub_XA_crypt_r(value key, value setting) {
    CAMLparam2(key, setting);
    CAMLlocal1(result);

    struct crypt_data cd = {0};

    caml_release_runtime_system();
    const char* const hashed =
        crypt_r(String_val(key), String_val(setting), &cd);
    caml_acquire_runtime_system();

    if (!hashed || *hashed == '*')
      CAMLreturn(Val_none);

    result = caml_copy_string(hashed);
    CAMLreturn(caml_alloc_some(result));
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

