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
 * This example assume you have a test server available with a pool containing 
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

static char *url, *source_url, *target_url;


typedef struct
{
    xen_result_func func;
    void *handle;
} xen_comms;


struct task_node
{
    xen_task* a_task;
    struct task_node *next; 
};

static void usage()
{
    fprintf(stderr,
"Usage:\n"
"\n"
"    test_vm_async_migrate <url> <username> <password>\n"
"                               <source-host> <target-host>\n"
"\n"
"where\n"
"        <url>      is the server's URL, e.g. server.example.com\n"
"                   i.e. the pool master"   
"        <username> is the username to use at the server <url>; and\n"
"        <password> is the password to <url>.\n"
"        <source-host>  the server to migrate all the VMs from \n"
"        <target-host>  the server to migrate all the VMs to \n"

);

/* Command line argument format:
 * "${OUTPUT_PATH}" "host_a.xensource.com" "root" "xenroot" "host_b" "host_a"
 * 
 * would attempt to move all the suitable VMs off the slave "host_b"
 * onto the master ("host_a"); note the url is needed for the master
 * but the source-host and target-host can be supplied using their local name
 */
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
    fprintf(stderr, "Error: %d", session->error_description_count);
    for (int i = 0; i < session->error_description_count; i++)
    {
        fprintf(stderr, "%s ", session->error_description[i]);
    }
    fprintf(stderr, "\n");
}

static int get_host_names(xen_session *session, 
        xen_host_set **hosts_address, char* source_url )
{
    int rc = -1; // failed but nothing to free
    if (xen_host_get_by_name_label(session, hosts_address, source_url))
    {
      if (((*hosts_address)->size) > 1)
      {
          rc = 1;  // greater than 1; an error but will free
          xen_host_set_free(*hosts_address);
      }
      else rc = 0; // size must be = 1
    }
    return rc;
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
    
    source_url = argv[4];
    target_url = argv[5];

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

    printf("\n\nQuerying host...\n");
    xen_host host;
    if (!xen_session_get_this_host(session, &host, session))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }
    
    /* Read in the source host and target host using their name labels
     */
    xen_host_set *source_hosts;
    
    int rc = 0;
 
    rc = get_host_names(session, &source_hosts, source_url );
    if (rc !=0 )
    {
        fprintf(stderr, "source host lookup failed.\n");
	print_error(session);
        return 1;
    }
    
    
    xen_host_set *target_hosts;
    rc = 0;
    rc = get_host_names(session, &target_hosts, target_url );
    if (rc !=0 )
    {
        fprintf(stderr, "target host lookup failed.\n");
	print_error(session);
        return 1;
    }
    
            
    struct xen_vm_set *all_vms_in_pool;
    xen_vm_get_all(session, &all_vms_in_pool);
    
    
    bool *vm_to_be_migrated = calloc(all_vms_in_pool->size, sizeof(bool));
    int num_vms_to_migrate = 0;
    enum xen_task_status_type task_status;
   
    xen_vm_record* result;
    
    for (size_t i = 0; i < all_vms_in_pool->size; i++ )
    {
        xen_vm a_vm = all_vms_in_pool->contents[i];
        xen_vm_get_record(session, &result, a_vm);
        
        /*
         * we can only migrate VMs that are
         * -not templates 
         * -not control domains
         * -and are running 
         * 
         * resident_on is used to identify the eligible VMs on the user
         * requested source_host 
         */
        
        if ( !result->is_a_template 
              && !result->is_control_domain
              && (result->power_state ==  XEN_VM_POWER_STATE_RUNNING)
              && (strcmp(result->resident_on->u.handle, 
                         (char*)source_hosts->contents[0]) == 0) )
        {
            // flag this VM as one suitable to migrate
            vm_to_be_migrated[i] = true;
            num_vms_to_migrate++; 
        }
        else 
        {
            vm_to_be_migrated[i] = false;
        }
    }
   
    
    if (!session->ok)
    {
        /* Error has been logged, just clean up. */
        xen_host_set_free(source_hosts);
        xen_host_set_free(target_hosts);
        xen_vm_set_free(all_vms_in_pool);       
        xen_vm_record_free(result);
        free(vm_to_be_migrated);
        CLEANUP;
        return 1;
    }

    xen_task* task_list = calloc(num_vms_to_migrate, sizeof(xen_task));
    xen_string_string_map* options = xen_string_string_map_alloc(0);    
    xen_string_set *error_msgs = NULL;
    
    int idx = 0;
    for (size_t i = 0; i < all_vms_in_pool->size; i++ )
    {
     
        if (vm_to_be_migrated[i] == true)
        {
            xen_vm_pool_migrate_async(session, &task_list[idx], all_vms_in_pool->contents[i], 
                                        target_hosts->contents[0], options );
            
            idx++;
            printf(" Migrating VM %d \n", i);
        }
    
    }
    
    // time out after certain number of iterations
    int max_iter = 50; 
    int iter = 0;
    int pause_interval_secs = 4;
    int tasks_running = 0;
    int tasks_completed = 0;
    xen_task a_task;
    bool tasks_still_pending = true;

    /* Poll how many of the migration tasks have completed. 
     * 
     * The task querying below isn't is intended to provide a sample of useful 
     * syntax. In practice a user would probably consider using functions such 
     * as xen_task_cancel or indeed the asynchronous equivalent 
     * xen_task_cancel_async.
     * These functions and other task handling ones are defined in xen_task.c
     */
    
    while ( iter < max_iter && tasks_still_pending )
    {
        tasks_running = 0;
        tasks_completed = 0;
        for (int j = 0; j < num_vms_to_migrate; j++)
        {
            a_task = task_list[j];
           
            xen_task_get_status(session, &task_status, a_task);
            
            if (task_status == XEN_TASK_STATUS_TYPE_PENDING)
            {
                tasks_running++;
            }
            else
            {
                /* See the xen_task_status_type enum definitions
                 * defined in xen_task_status_type.h a task can have
                 * failed, or be cancelled or in the process of being cancelled
                 * amongst others.
                 * The definition of tasks_completed in this context is tasks
                 * not pending.
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
                        printf("-------------------------------------\n");
                        printf("Failed while trying to migrate VM: \n");
                        for(size_t k=0; k<error_msgs->size; k++)
                        {
                            printf("error_msg %u : %s \n",  k, error_msgs->contents[k]); 
                        }
                    }
                }
                
                tasks_completed++;
            }

        }

        if (tasks_running == 0) 
        {
            tasks_still_pending = false; // stop the iteration early
            printf("All tasks completed \n");
        }
        printf("*********************************************\n");
        printf("VM migration progress, poll number %d \n", iter);
        printf("----------------------------------------\n");
        printf(" Tasks pending : %d \n", tasks_running);
        printf("       ended   : %d \n", tasks_completed);
        printf("*********************************************\n");    
        
        iter++;
        sleep(pause_interval_secs); 
        
    }
    

    xen_string_set_free(error_msgs);
    xen_string_string_map_free(options);
    xen_host_set_free(source_hosts);
    xen_host_set_free(target_hosts);
    free(task_list);
    free(vm_to_be_migrated);
    xen_vm_set_free(all_vms_in_pool);
    
    CLEANUP;
}
