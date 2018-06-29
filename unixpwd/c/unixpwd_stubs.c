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
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>

#include "unixpwd.h"


CAMLprim        value
caml_unixpwd_getpwd(value caml_user)
{
    CAMLparam1(caml_user);
    char           *user;
    char           *passwd;
    CAMLlocal1(pw);

    user = String_val(caml_user);
    passwd = unixpwd_getpwd(user);
    if (passwd == NULL && errno != 0)
        caml_failwith(strerror(errno));
    if (passwd == NULL)
        caml_failwith("unspecified error in caml_unixpwd_getpwd()");

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}

CAMLprim        value
caml_unixpwd_getspw(value caml_user)
{
    CAMLparam1(caml_user);
    char           *user;
    char           *passwd;
    CAMLlocal1(pw);

    user = String_val(caml_user);
    passwd = unixpwd_getspw(user);
    if (passwd == NULL && errno != 0)
        caml_failwith(strerror(errno));
    if (passwd == NULL)
        caml_failwith("unspecified error in caml_unixpwd_getspw()");

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}



CAMLprim        value
caml_unixpwd_get(value caml_user)
{
    CAMLparam1(caml_user);
    char           *user;
    char           *passwd;
    CAMLlocal1(pw);

    user = String_val(caml_user);
    passwd = unixpwd_get(user);
    if (passwd == NULL && errno != 0)
        caml_failwith(strerror(errno));
    if (passwd == NULL)
        caml_failwith("unspecified error in caml_unixpwd_get()");

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}

CAMLprim        value
caml_unixpwd_setpwd(value caml_user, value caml_password)
{
    CAMLparam2(caml_user, caml_password);
    char           *user,
                   *password;
    int             rc;

    user = String_val(caml_user);
    password = String_val(caml_password);
    rc = unixpwd_setpwd(user, password);
    if (rc != 0)
        caml_failwith(strerror(rc));
    CAMLreturn(Val_unit);
}

CAMLprim        value
caml_unixpwd_setspw(value caml_user, value caml_password)
{
    CAMLparam2(caml_user, caml_password);
    char           *user,
                   *password;
    int             rc;

    user = String_val(caml_user);
    password = String_val(caml_password);
    rc = unixpwd_setspw(user, password);
    if (rc != 0)
        caml_failwith(strerror(rc));
    CAMLreturn(Val_unit);
}

CAMLprim        value
caml_unixpwd_unshadow(void)
{
    CAMLparam0();
    char           *passwords;
    CAMLlocal1(str);

    passwords = unixpwd_unshadow();
    if (passwords == NULL && errno != 0)
        caml_failwith(strerror(errno));
    if (passwords == NULL)
        caml_failwith("unspecified error in caml_unixpwd_unshadow()");

    str = caml_copy_string(passwords);
    free(passwords);
    CAMLreturn(str);
}
