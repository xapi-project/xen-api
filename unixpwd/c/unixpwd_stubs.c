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
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include "unixpwd.h"

static CAMLprim value caml_unixpwd_get_(value caml_user, const char *fname,
                                        char *(*f)(const char *))
{
    CAMLparam1(caml_user);
    char *user;
    char *passwd;
    int saved_errno;
    CAMLlocal1(pw);

    user = caml_stat_strdup(String_val(caml_user));
    caml_enter_blocking_section();
    errno = 0;
    passwd = f(user);
    saved_errno = errno;
    caml_stat_free(user);
    user = NULL;
    caml_leave_blocking_section();
    errno = saved_errno;

    if ( passwd == NULL ) /* errno of 0 will be mapped to `EUNKNOWNERR of 0` */
        uerror(fname, caml_user);

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}

CAMLprim value caml_unixpwd_getpwd(value caml_user)
{
    return caml_unixpwd_get_(caml_user, "unixpwd_getpwd", unixpwd_getpwd);
}

CAMLprim value caml_unixpwd_getspw(value caml_user)
{
    return caml_unixpwd_get_(caml_user, "unixpwd_getspw", unixpwd_getspw);
}

CAMLprim value caml_unixpwd_get(value caml_user)
{
    return caml_unixpwd_get_(caml_user, "unixpwd_get", unixpwd_get);
}

static CAMLprim value caml_unixpwd_set_(value caml_user, value caml_password,
                                        const char *fname,
                                        int (*f)(const char *, char *))
{
    CAMLparam2(caml_user, caml_password);
    char *user;
    char *password;
    int saved_errno;
    int rc;

    user = caml_stat_strdup(String_val(caml_user));
    password = caml_stat_strdup(String_val(caml_password));
    caml_enter_blocking_section();
    errno = 0;
    rc = f(user, password);
    saved_errno = errno;
    caml_stat_free(user);
    caml_stat_free(password);
    caml_leave_blocking_section();
    errno = saved_errno;

    if ( rc != 0 )
        uerror(fname, caml_user); /* only raise with user not pass */
    CAMLreturn(Val_unit);
}

CAMLprim value caml_unixpwd_setpwd(value caml_user, value caml_password)
{
    return caml_unixpwd_set_(caml_user, caml_password, "unix_setpwd",
                             unixpwd_setpwd);
}

CAMLprim value caml_unixpwd_setspw(value caml_user, value caml_password)
{
    return caml_unixpwd_set_(caml_user, caml_password, "unix_setpwd",
                             unixpwd_setspw);
}

CAMLprim value caml_unixpwd_unshadow(value _unused)
{
    CAMLparam0();
    char *passwords;
    CAMLlocal1(str);

    /* NOT thread safe, retain runtime lock for now, it uses setpwent/endpwent,
     * this should be replaced by fopen/fpwgetent_r/etc. */
    passwords = unixpwd_unshadow();
    if ( passwords == NULL )
        uerror("unixpwd_unshadow", Nothing);

    str = caml_copy_string(passwords);
    free(passwords);
    CAMLreturn(str);
}
