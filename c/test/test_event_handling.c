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

#define _GNU_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <libxml/parser.h>
#include <curl/curl.h>
#include <xen/api/xen_all.h>


static void usage()
{
    fprintf(stderr,
"Usage:\n"
"\n"
"    test_event_handling <url> <username> <password>\n"
"\n"
"where\n"
"        <url>      is the server's URL, e.g. https://server.example.com\n"
"        <username> is the username to use at the server; and\n"
"        <password> is the password.\n");

    exit(EXIT_FAILURE);
}


static char *url;


typedef struct
{
    xen_result_func func;
    void *handle;
} xen_comms;


static size_t
write_func(void *ptr, size_t size, size_t nmemb, xen_comms *comms)
{
    size_t n = size * nmemb;
#ifdef PRINT_XML
    printf("\n\n---Result from server -----------------------\n");
    printf("%s\n",((char*) ptr));
    fflush(stdout);
#endif
    return comms->func(ptr, n, comms->handle) ? n : 0;
}


static int
call_func(const void *data, size_t len, void *user_handle,
          void *result_handle, xen_result_func result_func)
{
    (void)user_handle;

#ifdef PRINT_XML
    printf("\n\n---Data to server: -----------------------\n");
    printf("%s\n",((char*) data));
    fflush(stdout);
#endif

    CURL *curl = curl_easy_init();
    if (!curl) {
        return -1;
    }

    xen_comms comms = {
        .func = result_func,
        .handle = result_handle
    };

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1);
#ifdef CURLOPT_MUTE
    curl_easy_setopt(curl, CURLOPT_MUTE, 1);
#endif
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, &write_func);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &comms);
    curl_easy_setopt(curl, CURLOPT_POST, 1);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, data);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, len);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0);

    CURLcode result = curl_easy_perform(curl);

    curl_easy_cleanup(curl);

    return result;
}


static void print_error(xen_session *session)
{
    fprintf(stderr, "Error: %d\n", session->error_description_count);
    for (int i = 0; i < session->error_description_count; i++)
    {
        fprintf(stderr, "%s ", session->error_description[i]);
    }
    fprintf(stderr, "\n");
}


/**
 * Workaround for whinging GCCs, as suggested by strftime(3).
 */
static size_t my_strftime(char *s, size_t max, const char *fmt,
                          const struct tm *tm)
{
    return strftime(s, max, fmt, tm);
}


int main(int argc, char **argv)
{
    if (argc != 4)
    {
        usage();
    }

    url = argv[1];
    char *username = argv[2];
    char *password = argv[3];

    xmlInitParser();
    xen_init();
    curl_global_init(CURL_GLOBAL_ALL);

#define CLEANUP                                 \
    do {                                        \
        xen_session_logout(session);            \
        curl_global_cleanup();                  \
        xen_fini();                             \
        xmlCleanupParser();                     \
    } while(0)                                  \


    xen_session *session =
        xen_session_login_with_password(call_func, NULL, username, password,
                                        xen_api_latest_version);

    //get events for all classes
    char *all_classes = calloc(1, sizeof (all_classes));
    strncpy(all_classes, "*", sizeof (all_classes) - 1);

    struct xen_string_set *classes = xen_string_set_alloc(1);
    classes->contents[0] = all_classes;

    if (!session->ok)
    {
        print_error(session);
        CLEANUP;
        return 1;
    }

    // interval in seconds, after which the xen_event_from call should time out
    const double timeout = 30;

    // the output of xen_event_from includes a token, which can be passed into a
    // subsequent xen_event_from call to retrieve only the events that have occurred
    // since the last call; if an empty string is passed, xen_event_from will return
    // all events (this is normally done for the very first call)
    char token[512];
    token[0] = '\0';

    while (true)
    {
        printf("Polling for events...\n");
        struct xen_event_batch *event_batch;
        if (!xen_event_from(session, &event_batch, classes, token, timeout))
        {
            print_error(session);
            CLEANUP;
            return 1;
        }

        strncpy(token, event_batch->token, sizeof (token) - 1);
        token[sizeof (token) - 1] = '\0';

        for (size_t i = 0; i < event_batch->events->size; i++)
        {
            xen_event_record *ev = event_batch->events->contents[i];
            char time[256];
            struct tm *tm = localtime(&ev->timestamp);
            my_strftime(time, 256, "%c, local time", tm);
            printf("Event received: ID = %"PRId64", %s.\n", ev->id, time);
            switch (ev->operation)
            {
            case XEN_EVENT_OPERATION_ADD:
                printf("%s created with reference %s.\n", ev->class, ev->ref);
                break;

            case XEN_EVENT_OPERATION_DEL:
                printf("%s with reference %s deleted.\n", ev->class, ev->ref);
                break;

            case XEN_EVENT_OPERATION_MOD:
                printf("%s with reference %s modified.\n", ev->class, ev->ref);
                break;
            default:
                assert(false);
            }
        }

        xen_event_batch_free(event_batch);
    }

    xen_string_set_free(classes);

    CLEANUP;

    return 0;
}
