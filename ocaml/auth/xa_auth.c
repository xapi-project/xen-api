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
#include <stdarg.h>

#include "xa_auth.h"
#include <security/pam_appl.h>
#include <security/pam_misc.h>

#define SERVICE_NAME "xapi"

#define XA_LOG_AUTH "authhelper"

/* Adapted from xenagentd.hg:src/xa_auth.c */
struct xa_auth_info
{
    const char *username;
    const char *password;
};

static int xa_auth_conv(int num_msg, const struct pam_message **msg,
                        struct pam_response **resp, void *app_data)
{
    struct xa_auth_info *auth_info = app_data;
    struct pam_response *response;
    int i, j;

    if ( msg == NULL || resp == NULL || app_data == NULL )
        return PAM_CONV_ERR;

    response = calloc(num_msg, sizeof(struct pam_response));
    if ( response == NULL )
        return PAM_CONV_ERR;

    for ( i = 0; i < num_msg; i++ )
    {
        switch ( msg[i]->msg_style )
        {
        case PAM_PROMPT_ECHO_ON:
            response[i].resp = strdup(auth_info->username);
            if ( response[i].resp == NULL )
                goto resperr;
            break;
        case PAM_PROMPT_ECHO_OFF:
            response[i].resp = strdup(auth_info->password);
            if ( response[i].resp == NULL )
                goto resperr;
            break;
        default:
            goto resperr;
        }
    }

    *resp = response;
    return PAM_SUCCESS;

resperr:
    for ( j = 0; j < i; j++ )
        free(response[j].resp);
    free(response);
    return PAM_CONV_ERR;
}

int XA_mh_authorize(const char *username, const char *password,
                    const char **error)
{
    struct xa_auth_info auth_info = { username, password };
    struct pam_conv xa_conv = { xa_auth_conv, &auth_info };
    pam_handle_t *pamh;
    int rc = XA_SUCCESS;

    if ( (rc = pam_start(SERVICE_NAME, username, &xa_conv, &pamh))
         != PAM_SUCCESS )
    {
        goto exit;
    }
    if ( (rc = pam_authenticate(pamh, PAM_DISALLOW_NULL_AUTHTOK))
         != PAM_SUCCESS )
    {
        goto exit;
    }

    rc = pam_acct_mgmt(pamh, PAM_DISALLOW_NULL_AUTHTOK);

exit:
    pam_end(pamh, rc);
    if ( rc != PAM_SUCCESS )
    {
        if ( error )
            *error = pam_strerror(pamh, rc);
        rc = XA_ERR_EXTERNAL;
    }
    else
    {
        rc = XA_SUCCESS;
    }
    return rc;
}

int XA_mh_chpasswd(const char *username, const char *new_passwd,
                   const char **error)
{
    struct xa_auth_info auth_info = { username, new_passwd };
    struct pam_conv xa_conv = { xa_auth_conv, &auth_info };
    pam_handle_t *pamh;
    int rc = XA_SUCCESS;

    if ( (rc = pam_start(SERVICE_NAME, username, &xa_conv, &pamh))
         != PAM_SUCCESS )
    {
        goto exit;
    }
    rc = pam_chauthtok(pamh, 0);

exit:
    if ( rc != PAM_SUCCESS )
    {
        if ( error )
            *error = pam_strerror(pamh, rc);
        pam_end(pamh, rc);
        rc = XA_ERR_EXTERNAL;
    }
    else
    {
        pam_end(pamh, rc);
        rc = XA_SUCCESS;
    }
    return rc;
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
