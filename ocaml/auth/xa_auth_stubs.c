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
/*
 */

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
    caml_leave_blocking_section();
    
    free(c_username);
    free(c_password);
    
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
    caml_leave_blocking_section();
    
    free(c_username);
    free(c_new_password);
    
    if (rc != XA_SUCCESS)
        caml_failwith(error ? error : "Unknown error");
    CAMLreturn(ret);
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

