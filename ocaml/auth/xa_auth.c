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
struct xa_auth_info {
    const char *username;
    const char *password;
};

static int xa_auth_conv(int num_msg, const struct pam_message **msg,
                        struct pam_response **resp, void *app_data)
{
    struct xa_auth_info *auth_info = app_data;
    struct pam_response *response;
    int i, j;

    if (msg == NULL || resp == NULL || app_data == NULL) 
        return PAM_CONV_ERR;
    
    response = calloc (num_msg, sizeof (struct pam_response));
    if (response == NULL)
        return PAM_CONV_ERR;
    
    for (i = 0; i < num_msg; i++) {
        switch(msg[i]->msg_style) {
        case PAM_PROMPT_ECHO_ON:
            response[i].resp = strdup(auth_info->username);
            if (response[i].resp == NULL)
              goto resperr;
            break;
        case PAM_PROMPT_ECHO_OFF:
            response[i].resp = strdup(auth_info->password);
            if (response[i].resp == NULL)
              goto resperr;
            break;
        default:
            goto resperr;
        }
    }
   
    *resp = response;
    return PAM_SUCCESS;

resperr:
    for (j = 0; j < i; j++)
        free(response[j].resp);
    free(response);
    return PAM_CONV_ERR;
}

int XA_mh_authorize (const char *username, const char *password,
                     const char **error)
{
    struct xa_auth_info auth_info = {username, password};
    struct pam_conv xa_conv = {xa_auth_conv, &auth_info};
    pam_handle_t *pamh;
    int rc = XA_SUCCESS;

    if ((rc = pam_start(SERVICE_NAME, username, &xa_conv, &pamh))
        != PAM_SUCCESS) {
        goto exit;
    }
    if ((rc = pam_authenticate(pamh, PAM_DISALLOW_NULL_AUTHTOK))
        != PAM_SUCCESS) {
        goto exit;
    }

    rc = pam_acct_mgmt(pamh, PAM_DISALLOW_NULL_AUTHTOK);

 exit:
    pam_end(pamh, rc);
    if (rc != PAM_SUCCESS) {
        if (error) *error = pam_strerror(pamh, rc);
        rc = XA_ERR_EXTERNAL;
    }
    else {
        rc = XA_SUCCESS;
    }
    return rc;
}

int XA_mh_chpasswd (const char *username, const char *new_passwd, const char **error)
{
    struct xa_auth_info auth_info = {username, new_passwd};
    struct pam_conv xa_conv = {xa_auth_conv, &auth_info};
    pam_handle_t *pamh;
    int rc = XA_SUCCESS;

    if ((rc = pam_start(SERVICE_NAME, username, &xa_conv, &pamh))
        != PAM_SUCCESS) {
        goto exit;
    }
    rc = pam_chauthtok(pamh, 0);

 exit:
    if (rc != PAM_SUCCESS) {
        if (error) *error = pam_strerror(pamh, rc);
        pam_end(pamh, rc);
        rc = XA_ERR_EXTERNAL;
    }
    else {
        pam_end(pamh, rc);
        rc = XA_SUCCESS;
    }
    return rc;
}


static struct pam_conv default_conv = {xa_auth_conv, NULL};
/*
    pam(3)  says "The libpam interfaces are only thread-safe if each thread within the multithreaded application uses its own PAM handle."

    This is ambigous, but a safe interpretation is that PAM handles must be created, used and destroyed within the same thread:
    sharing a PAM handle between threads is not safe even if only one thread at a time would access it.

    pam_start(3) says "it is not possible to use the same handle for different transactions, a new one is needed for every new context."
        "The PAM handle cannot be used for multiple authentications at the same time as long as pam_end was not called on it before."

    We need a pool of ready-to-use PAM handles to handle authentication in a dedicated worker thread-pool
    (API threads are created and destroyed dynamically they wouldn't be useful for caching a handle)
*/
pam_handle_t *XA_mh_authorize_start (const char **error)
{
    pam_handle_t *pamh;
    int rc = pam_start(SERVICE_NAME, NULL, &default_conv, &pamh);
    if (PAM_SUCCESS == rc)
        return pamh;
    /* pamh is undefined here, explicitly set to NULL to avoid using it in strerror! */
    pamh = NULL;
    if (error) *error = pam_strerror(pamh, rc);
    return NULL;
}

int XA_mh_authorize_stop (pam_handle_t *pamh, const char **error)
{
    int rc = pam_end(pamh, PAM_SUCCESS);
    if (PAM_SUCCESS == rc)
        return XA_SUCCESS;
    /* pamh is undefined here, explicitly set to NULL to prevent accidentally using it */
    pamh = NULL;
    if (error) *error = pam_strerror(pamh, rc);
    return XA_ERR_EXTERNAL;
}

int XA_mh_authorize_run (pam_handle_t **pamhp, const char *username, const char *password,
                         const char **error)
{
    struct xa_auth_info auth_info = {username, password};
    struct pam_conv xa_conv = {xa_auth_conv, &auth_info};
    pam_handle_t *pamh;
    int rc = XA_SUCCESS;

    if (!pamhp || !username || !password) {
        if (error) *error = "Internal error: null arguments";
        return XA_ERR_EXTERNAL;
    }

    pamh = *pamhp;
    if (!pamh) {
        if (error) *error = "Use after free detected";
        return XA_ERR_EXTERNAL;
    }

    do {
        if ((rc = pam_set_item(pamh, PAM_USER, username)) != PAM_SUCCESS)
            break;

        if ((rc = pam_set_item(pamh, PAM_CONV, &xa_conv)) != PAM_SUCCESS)
            break;

        if ((rc = pam_authenticate(pamh, PAM_DISALLOW_NULL_AUTHTOK)) != PAM_SUCCESS)
            break;

        if ((rc = pam_acct_mgmt(pamh, PAM_DISALLOW_NULL_AUTHTOK)) != PAM_SUCCESS)
            break;

        /* do not leave dangling pointers around in the PAM handle */
        if ((rc = pam_set_item(pamh, PAM_USER, NULL)) != PAM_SUCCESS)
            break;

        if ((rc = pam_set_item(pamh, PAM_CONV, &default_conv)) != PAM_SUCCESS)
            break;
    } while(0);

    if (PAM_SUCCESS == rc)
        return XA_SUCCESS;

    /* mark pam handle unusable, prevent use after free */
    *pamhp = NULL;

    if (error) *error = pam_strerror(pamh, rc);

    /* always clean up after a failure, we've likely already incurred the cost of fail delay */
    (void)pam_end(pamh, rc);

    return XA_ERR_EXTERNAL;
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

