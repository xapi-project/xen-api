/*
 * Copyright (c) Citrix Systems, Inc
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef CERTCHECK_H
#define CERTCHECK_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Certificate expiration status codes
 */
typedef enum {
    CERT_CHECK_OK = 0,              /* All certificates are valid */
    CERT_CHECK_EXPIRED = 1,         /* At least one certificate is expired */
    CERT_CHECK_NOT_YET_VALID = 2,   /* At least one certificate is not yet valid */
    CERT_CHECK_PARSE_ERROR = -1,    /* Error parsing NVRAM data */
    CERT_CHECK_NO_CERTS = -2,       /* No certificates found */
    CERT_CHECK_INTERNAL_ERROR = -3, /* Internal error (e.g., memory allocation) */
} cert_check_status_t;

/**
 * Information about a certificate
 */
typedef struct cert_info {
    char *subject;          /* Certificate subject (allocated, caller must free) */
    char *issuer;           /* Certificate issuer (allocated, caller must free) */
    time_t not_before;      /* Certificate validity start time */
    time_t not_after;       /* Certificate validity end time */
    bool is_expired;        /* True if certificate is expired */
    bool is_not_yet_valid;  /* True if certificate is not yet valid */
    char *variable_name;    /* Name of EFI variable containing this cert (PK, KEK, db) */
    struct cert_info *next; /* Pointer to next certificate in list */
} cert_info_t;

/**
 * Check if any certificate in the NVRAM is expired.
 *
 * @param nvram_data    Pointer to the raw NVRAM data (serialized variable store)
 * @param nvram_len     Length of the NVRAM data in bytes
 *
 * @return CERT_CHECK_OK if all certificates are valid,
 *         CERT_CHECK_EXPIRED if at least one certificate is expired,
 *         CERT_CHECK_NOT_YET_VALID if at least one certificate is not yet valid,
 *         or a negative error code on failure.
 */
cert_check_status_t varstored_check_cert_expiration(const uint8_t *nvram_data,
                                                    size_t nvram_len);

/**
 * Check certificate expiration at a specific time.
 *
 * @param nvram_data    Pointer to the raw NVRAM data (serialized variable store)
 * @param nvram_len     Length of the NVRAM data in bytes
 * @param check_time    The time to check against (use time(NULL) for current time)
 *
 * @return CERT_CHECK_OK if all certificates are valid at the specified time,
 *         CERT_CHECK_EXPIRED if at least one certificate is expired,
 *         CERT_CHECK_NOT_YET_VALID if at least one certificate is not yet valid,
 *         or a negative error code on failure.
 */
cert_check_status_t varstored_check_cert_expiration_at(const uint8_t *nvram_data,
                                                       size_t nvram_len,
                                                       time_t check_time);

/**
 * Get detailed information about all certificates in the NVRAM.
 *
 * @param nvram_data    Pointer to the raw NVRAM data (serialized variable store)
 * @param nvram_len     Length of the NVRAM data in bytes
 * @param out_certs     Output pointer to receive the list of certificate info structures.
 *                      Caller must free with varstored_free_cert_info().
 *
 * @return CERT_CHECK_OK on success, or a negative error code on failure.
 */
cert_check_status_t varstored_get_cert_info(const uint8_t *nvram_data,
                                            size_t nvram_len,
                                            cert_info_t **out_certs);

/**
 * Free certificate information list returned by varstored_get_cert_info().
 *
 * @param certs    Pointer to the first certificate info structure in the list.
 */
void varstored_free_cert_info(cert_info_t *certs);

/**
 * Get a human-readable string for a certificate check status code.
 *
 * @param status    The status code to convert.
 *
 * @return A static string describing the status.
 */
const char *varstored_cert_status_string(cert_check_status_t status);

#ifdef __cplusplus
}
#endif

#endif /* CERTCHECK_H */
