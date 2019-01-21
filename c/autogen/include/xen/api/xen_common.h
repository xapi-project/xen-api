/*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 * 
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef XEN_COMMON_H
#define XEN_COMMON_H


#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>

#ifndef XEN_CLAZZ
#ifdef __cplusplus
#define XEN_CLAZZ clazz
#else
#define XEN_CLAZZ class
#endif
#endif

#include "xen_api_version.h"
#include "xen/api/xen_host_decl.h"
#include "xen/api/xen_task_decl.h"
#include "xen/api/xen_string_set.h"


typedef bool (*xen_result_func)(const void *data, size_t len,
                                void *result_handle);


/**
 * len does not include a terminating \0.
 */
typedef int (*xen_call_func)(const void *, size_t len, void *user_handle,
                             void *result_handle,
                             xen_result_func result_func);

typedef struct
{
    xen_call_func call_func;
    void *handle;
    const char *session_id;
    bool ok;
    char **error_description;
    int error_description_count;
    xen_api_version api_version;
} xen_session;


typedef struct xen_session_record
{
    char *uuid;
    struct xen_host_record_opt *this_host;
    char *this_user;
    time_t last_active;
} xen_session_record;


/**
 * Allocate a xen_session_record.
 */
extern xen_session_record *
xen_session_record_alloc(void);


/**
 * Free the given xen_session_record, and all referenced values.  The
 * given record must have been allocated by this library.
 */
extern void
xen_session_record_free(xen_session_record *record);


struct xen_task_;
typedef struct xen_task_ * xen_task_id;


typedef struct
{
    int progress;
    long eta;
    /* !!! RESULT */
}  xen_task_status;


typedef struct
{
    int major;
    int minor;
    int patch;
    char *extraversion;
} xen_version;


/**
 * Free the given xen_version, and all referenced values.
 */
extern void xen_version_free(xen_version *version);


/**
 * Return the version of this client-side library.  This will be the major,
 * minor, and extraversion of the Xen release with which it was released,
 * plus the library's own version as the patch.
 */
extern xen_version *xen_get_client_side_version();


extern bool
xen_uuid_string_to_bytes(char *uuid, char **bytes);


extern bool
xen_uuid_bytes_to_string(char *bytes, char **uuid);


extern void
xen_uuid_free(char *uuid);


extern void
xen_uuid_bytes_free(char *bytes);


/**
 * Initialise this library.  Call this before starting to use this library.
 * Note that since this library depends upon libxml2, you should also call
 * xmlInitParser as appropriate for your program.
 */
extern
void xen_init(void);


/**
 * Clear up this library.  Call when you have finished using this library.
 * Note that since this library depends upon libxml2, you should also call
 * xmlCleanupParser as appropriate for your program.
 */
extern
void xen_fini(void);


/**
 * Log in at the server, and allocate a xen_session to represent this session.
 */
extern xen_session *
xen_session_login_with_password(xen_call_func call_func, void *handle,
                                const char *uname, const char *pwd,
                                xen_api_version version);


/**
 * Log in at the server, and allocate a xen_session to represent this session.
 */
extern xen_session *
xen_session_slave_local_login_with_password(xen_call_func call_func, void *handle,
                                            const char *uname, const char *pwd);


/**
 * Log out at the server, and free the xen_session.
 */
extern void
xen_session_logout(xen_session *session);


/**
 * Log out at the server, and free the local xen_session.
 */
extern void
xen_session_local_logout(xen_session *session);


 /**
 * Log out all sessions associated to a user subject-identifier, except the session associated with the context calling this function
 */
extern bool
xen_session_logout_subject_identifier(xen_session *session, const char *subject_identifier);


 /**
 * Log out all sessions associated to a user subject-identifier, except the session associated with the context calling this function
 */
extern bool
xen_session_logout_subject_identifier_async(xen_session *session, xen_task *result, const char *subject_identifier);


/**
 * Return a list of all the user subject-identifiers of all existing sessions
 */
extern bool
xen_session_get_all_subject_identifiers(xen_session *session, struct xen_string_set **result);


/**
 * Return a list of all the user subject-identifiers of all existing sessions
 */
extern bool
xen_session_get_all_subject_identifiers_async(xen_session *session, xen_task *result);


/**
 * Clear any error condition recorded on this session.
 */
void
xen_session_clear_error(xen_session *session);


/**
 * Get the UUID of the second given session.  Set *result to point at a
 * string, yours to free.
 */
extern bool
xen_session_get_uuid(xen_session *session, char **result,
                     xen_session *self_session);


/**
 * Get the this_host field of the second given session.  Set *result to be a
 * handle to that host.
 */
extern bool
xen_session_get_this_host(xen_session *session, xen_host *result,
                          xen_session *self_session);


/**
 * Get the this_user field of the second given session.  Set *result to point
 * at a string, yours to free.
 */
extern bool
xen_session_get_this_user(xen_session *session, char **result,
                          xen_session *self_session);


/**
 * Get the last_active field of the given session, and place it in *result.
 */
extern bool
xen_session_get_last_active(xen_session *session, time_t *result,
                            xen_session *self_session);

/**
 * Get a record containing the current state of the second given session.
 */
extern bool
xen_session_get_record(xen_session *session, xen_session_record **result,
                       xen_session *self_session);


#endif
