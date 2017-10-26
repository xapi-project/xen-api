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
"    test_get_records <url> <username> <password>\n"
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


int main(int argc, char **argv)
{
    if (argc != 4)
        usage();

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

    
    xen_session *session = xen_session_login_with_password(call_func, NULL,
                               username, password, xen_api_latest_version);

    
    /* Print some info for hosts */
    
    xen_host_xen_host_record_map *hostRecords;
    if (!xen_host_get_all_records(session, &hostRecords))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }
    
    for (size_t i = 0; i < hostRecords->size; i++)
    {
        xen_host_record *rec = hostRecords->contents[i].val;        
        printf("Host: %s, edition: %s\n", rec->name_label, rec->edition);
    }
    
    xen_host_xen_host_record_map_free(hostRecords);

    /* Print some info for templates */
    /* Also choose a template to use further down */

    char chosenUuid[256];
    chosenUuid[0] = '\0';
    
    xen_vm_xen_vm_record_map *vmRecords;    
    if (!xen_vm_get_all_records(session, &vmRecords))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }
    
    for (size_t i = 0; i < vmRecords->size; i++)
    {
        xen_vm_record *rec = vmRecords->contents[i].val;        
        if (!rec->is_a_template)
            continue;

        if (chosenUuid[0] == '\0')
        {
            strncpy(chosenUuid, rec->uuid, sizeof (chosenUuid) - 1);
            chosenUuid[sizeof (chosenUuid) - 1] = '\0';
        }
        
        printf("VM: %s, vCPUs max: %" PRId64 "\n", rec->name_label, rec->vcpus_max);
    }
    
    xen_vm_xen_vm_record_map_free(vmRecords);
    
    /* clone the first vm, add a blocked operation to the clone
     * and then print out its allowed and blocked oeprations */

    xen_vm orig;
    if (!xen_vm_get_by_uuid(session, &orig, chosenUuid))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }

    xen_vm clone;
    xen_vm_clone(session, &clone, orig, "clonedVM");

    xen_vm_add_to_blocked_operations(session, clone, XEN_VM_OPERATIONS_POOL_MIGRATE, "123");

    xen_vm_record *vm_rec;
    xen_vm_get_record(session, &vm_rec, clone);

    for (size_t j = 0; j < vm_rec->allowed_operations->size; j++)
    {
        printf("VM: %s, Allowed operation: %s\n",
                vm_rec->name_label,
                xen_vm_operations_to_string(vm_rec->allowed_operations->contents[j]));
    }

    for (size_t k = 0; k < vm_rec->blocked_operations->size; k++)
    {
        printf("VM: %s, Blocked operation: %s, Error code: %s\n",
                vm_rec->name_label,
                xen_vm_operations_to_string(vm_rec->blocked_operations->contents[k].key),
                vm_rec->blocked_operations->contents[k].val);
    }

    xen_vm_record_free(vm_rec);
    xen_vm_destroy(session, clone);

    /* Print some info for storage repositories */
    
    xen_sr_xen_sr_record_map *srRecords;    
    if (!xen_sr_get_all_records(session, &srRecords))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }
    
    for (size_t i = 0; i < srRecords->size; i++)
    {
        xen_sr_record *rec = srRecords->contents[i].val;        
        printf("SR: %s -> Free space: %" PRId64 "\n",
                rec->name_label,
                rec->physical_size - rec->physical_utilisation);
    }
    
    xen_sr_xen_sr_record_map_free(srRecords);

    CLEANUP;
    return 0;
}
