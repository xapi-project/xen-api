/*
 * Copyright (C) Citrix Systems Inc.
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

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/threads.h>

#include "unixpwd.h"

static value caml_unixpwd_get_(value caml_user, const char *fname, char*(*f)(const char*))
{
    CAMLparam1(caml_user);
    char     *user;
    char     *passwd;
    int       saved_errno;
    CAMLlocal1(pw);

    user = caml_stat_strdup(String_val(caml_user));
    caml_release_runtime_system();
    errno = 0;
    passwd = f(user);
    saved_errno = errno;
    caml_acquire_runtime_system();
    caml_stat_free(user); user = NULL;
    errno = saved_errno;

    if (passwd == NULL) {
        char msg[128];

        snprintf(msg, sizeof(msg), "unspecified error in %s()", fname);
        caml_failwith(saved_errno ? strerror(saved_errno) : msg);
    }

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}

CAMLprim        value
caml_unixpwd_getpwd(value caml_user)
{
    return caml_unixpwd_get_(caml_user, "unixpwd_getpwd", unixpwd_getpwd);
}

CAMLprim        value
caml_unixpwd_getspw(value caml_user)
{
    return caml_unixpwd_get_(caml_user, "unixpwd_getspw", unixpwd_getspw);
}

CAMLprim        value
caml_unixpwd_get(value caml_user)
{
    return caml_unixpwd_get_(caml_user, "unixpwd_get", unixpwd_get);
}

static value caml_unixpwd_set_(value caml_user, value caml_password, const char *fname, int(*f)(const char*, char*))
{
    CAMLparam2(caml_user, caml_password);
    char     *user;
    char     *password;
    int       rc;

    user = caml_stat_strdup(String_val(caml_user));
    password = caml_stat_strdup(String_val(caml_password));
    caml_release_runtime_system();
    rc = f(user, password);
    caml_acquire_runtime_system();
    caml_stat_free(user);
    caml_stat_free(password);

    if (rc != 0) {
        char msg[128];

        snprintf(msg, sizeof(msg), "%s: %s", fname, strerror(rc));
        caml_failwith(msg);
    }
    CAMLreturn(Val_unit);
}

CAMLprim        value
caml_unixpwd_setpwd(value caml_user, value caml_password)
{
    return caml_unixpwd_set_(caml_user, caml_password, "unixpwd_setpwd",
                             unixpwd_setpwd);
}

CAMLprim        value
caml_unixpwd_setspw(value caml_user, value caml_password)
{
    return caml_unixpwd_set_(caml_user, caml_password, "unixpwd_setspw",
                             unixpwd_setspw);
}
