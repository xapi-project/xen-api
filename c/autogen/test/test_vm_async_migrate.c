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

/* PURPOSE:
 * ========
 *
 * This sample program demonstrates the availability of APIs that
 * allow API calls to be made asynchronously without the need for explicit
 * thread construction.
 *
 * Many APIs have an equivalent asynchronous version that will return a
 * xen_task object and release control back to the main program. The xen_task
 * object can later be queried as to whether the task has finished. In this
 * way the user can start multiple tasks in parallel rather than concurrently.
 *
 *
 * PRE-REQUISITES:
 * ===============
 * This is a very specific example to demonstrate the availability of
 * asynchronous API functions and pools containing more than one host.
 *
 * This example assumes you have a test server available with a pool containing
 * two hosts and that the hosts are such that it is possible to migrate VMs
 * from one to the other.
 *
 */

#define _GNU_SOURCE
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <libxml/parser.h>
#include <curl/curl.h>

#include <xen/api/xen_all.h>

static char *url;


typedef struct
{
    xen_result_func func;
    void *handle;
} xen_comms;


static void usage()
{
    fprintf(stderr,
            "Usage:\n"
            "\n"
            "    test_vm_async_migrate <url> <username> <password> <source-host> <target-host>\n"
            "\n"
            "where\n"
            "    <url>          is the master server's URL, e.g. server.example.com\n"
            "    <username>     is the username to use at the server <url>\n"
            "    <password>     is the password to <url>.\n"
            "    <source-host>  is the name of the server to migrate all the VMs from\n"
            "    <target-host>  is the name server to migrate all the VMs to\n");

    exit(EXIT_FAILURE);
}

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

    if (argc != 6)
    {
        usage();
    }

    url = argv[1];
    char *username = argv[2];
    char *password = argv[3];

    char* sourceHost = argv[4];
    char* targetHost = argv[5];

    xmlInitParser();
    xmlKeepBlanksDefault(0);
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

    /* Ensure the source and target hosts exist before attepting migration */
    xen_host_set *source_hosts = 0;
    if (!xen_host_get_by_name_label(session, &source_hosts, sourceHost) || source_hosts->size < 1)
    {
        fprintf(stderr, "Source host lookup failed.\n");
        print_error(session);
        if (source_hosts)
            xen_host_set_free(source_hosts);
        return 1;
    }

    xen_host_set *target_hosts = 0;
    if (!xen_host_get_by_name_label(session, &target_hosts, targetHost) || target_hosts->size < 1)
    {
        fprintf(stderr, "Target host lookup failed.\n");
        print_error(session);
        if (target_hosts)
            xen_host_set_free(target_hosts);
        return 1;
    }

    xen_vm_xen_vm_record_map *all_vms_in_pool;
    if (!xen_vm_get_all_records(session, &all_vms_in_pool))
    {
        xen_host_set_free(source_hosts);
        xen_host_set_free(target_hosts);
        xen_vm_xen_vm_record_map_free(all_vms_in_pool);
        CLEANUP;
        return 1;
    }

    xen_task* task_list = calloc(all_vms_in_pool->size, sizeof (xen_task));
    xen_string_string_map* options = xen_string_string_map_alloc(0);

    for (size_t i = 0; i < all_vms_in_pool->size; i++)
    {
        xen_vm a_vm = all_vms_in_pool->contents[i].key;
        xen_vm_record *rec = all_vms_in_pool->contents[i].val;

        /*
         * We can only migrate running VMs that are not control domains.
         * Resident_on is used to identify the eligible VMs on the user
         * requested source_host
         */

        if (!rec->is_a_template
                && !rec->is_control_domain
                && (rec->power_state == XEN_VM_POWER_STATE_RUNNING)
                && (strcmp(rec->resident_on->u.handle,
                (char*) source_hosts->contents[0]) == 0))
        {
            printf(" Migrating VM %s \n", rec->name_label);
            xen_vm_pool_migrate_async(session, &task_list[i], a_vm,
                    target_hosts->contents[0], options);
        }
        else
        {
            task_list[i] = NULL;
        }
    }

    xen_string_set *error_msgs = NULL;

    // time out after certain number of iterations
    int max_iter = 50;
    int iter = 0;
    int pause_interval_secs = 4;

    /* Poll how many of the migration tasks have completed.*/

    do
    {
        int tasks_running = 0;

        for (size_t j = 0; j < all_vms_in_pool->size; j++)
        {
            xen_task a_task = task_list[j];
            if (a_task == NULL)
                continue;

            enum xen_task_status_type task_status;
            xen_task_get_status(session, &task_status, a_task);

            if (task_status == XEN_TASK_STATUS_TYPE_PENDING)
            {
                tasks_running++;
            }
            else
            {
                /* This code logs only a failed task. It is not interested in
                 * a task that has succeeded, has been cancelled, is in the
                 * process of being cancelled etc.
                 */

                if (task_status == XEN_TASK_STATUS_TYPE_FAILURE)
                {
                    if (xen_task_get_error_info(session, &error_msgs, task_list[j] ))
                    {
                        /* VMs may need to meet certain criteria for migration to be
                         * possible between hosts; such as shared storage between
                         * hosts. It is advisable to check the criteria needed for
                         * migration on the particular version of XenServer.
                         * The error messages output below should give information
                         * that allows the identification of an unsupported
                         * operation
                         */
                        printf("-----------------------------------\n");
                        printf("Failed while trying to migrate VM: \n");
                        for (size_t k = 0; k < error_msgs->size; k++)
                        {
                            printf("error_msg %zu : %s \n",  k, error_msgs->contents[k]);
                        }
                    }
                }

                /* Avoid querying it in the next iterations */
                task_list[j] = NULL;
            }
        }

        printf("**************************************\n");
        printf("VM migration progress, poll number %d \n", iter);
        printf("--------------------------------------\n");
        printf(" Tasks pending : %d \n", tasks_running);
        printf("**************************************\n");

        if (tasks_running == 0)
            break;

        iter++;
        sleep(pause_interval_secs);
    }
    while (iter < max_iter);

    xen_string_set_free(error_msgs);
    xen_string_string_map_free(options);
    xen_host_set_free(source_hosts);
    xen_host_set_free(target_hosts);
    free(task_list);
    xen_vm_xen_vm_record_map_free(all_vms_in_pool);

    CLEANUP;
}
