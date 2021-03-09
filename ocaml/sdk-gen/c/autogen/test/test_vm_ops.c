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
 * This sample program demonstrates how to create a VM, create a blank
 * disk image in a named Storage Repository, and attach the disk to the
 * VM.
 *
 * Before creating the VM, the host's capabilities are queried and
 * displayed.
 *
 * After creating the VM, some powercycle operations are demonstrated:
 * starting the VM, suspending it, resuming it and shutting it down.
 * After each powercycle operation the server is queried and the powerstate
 * displayed.
 *
 */

#define _GNU_SOURCE
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <libxml/parser.h>
#include <curl/curl.h>

#include <xen/api/xen_all.h>

static char *url;


typedef struct
{
    xen_result_func func;
    void *handle;
} xen_comms;


static xen_vm create_new_vm(xen_session *session, char* template_name, char* sr_name, bool pv);
static void print_vm_power_state(xen_session *session, xen_vm vm);
static char *replace_str(char *str, char *orig, char *rep);
static int cycle_vm(xen_session *session, xen_vm vm);


static void usage()
{
    fprintf(stderr,
"Usage:\n"
"\n"
"    test_vm_ops <url> <sr-name> <username> <password>\n"
"\n"
"where\n"
"        <url>      is the server's URL, e.g. https://server.example.com\n"
"        <sr-name>  the name of the SR in which to create a disk\n"
"        <username> is the username to use at the server; and\n"
"        <password> is the password.\n");

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

    if (argc != 5)
    {
        usage();
    }

    url = argv[1];
    char *sr_name = argv[2];
    char *username = argv[3];
    char *password = argv[4];

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

    /* ---------------------------------------------------------------------
       Read host, capabilities and API vsn
       --------------------------------------------------------------------- */

    printf("\n\nQuerying host...\n");

    xen_host host;
    if (!xen_session_get_this_host(session, &host, session))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }

    xen_string_string_map *versions;
    if (!xen_host_get_software_version(session, &versions, host))
    {
        print_error(session);
        xen_host_free(host);
        CLEANUP;
        return 1;
    }

    xen_string_set *supported_bootloaders;
    if (!xen_host_get_supported_bootloaders(session, &supported_bootloaders,
                                            host))
    {
        print_error(session);
        xen_string_string_map_free(versions);
        xen_host_free(host);
        CLEANUP;
        return 1;
    }

    xen_string_set *capabilities;
    if (!xen_host_get_capabilities(session, &capabilities, host))
    {
        print_error(session);
        xen_string_set_free(supported_bootloaders);
        xen_string_string_map_free(versions);
        xen_host_free(host);
        CLEANUP;
        return 1;
    }


    for (size_t i = 0; i < versions->size; i++)
    {
        printf("%s -> %s.\n", versions->contents[i].key,
               versions->contents[i].val);
    }

    printf("Host supports the following bootloaders:");
    for (size_t i = 0; i < supported_bootloaders->size; i++)
    {
        printf(" %s", supported_bootloaders->contents[i]);
    }
    printf("\n");

    printf("Host has the following capabilities:");
    for (size_t i = 0; i < capabilities->size; i++)
    {
        printf(" %s", capabilities->contents[i]);
    }
    printf("\n");

    xen_host_free(host);
    xen_string_string_map_free(versions);
    xen_string_set_free(supported_bootloaders);
    xen_string_set_free(capabilities);

    /* ---------------------------------------------------------------------
       Create a new VM with a blank disk:
       --------------------------------------------------------------------- */

    printf("\n\nCreating new HVM VM...\n");
    xen_vm hvm_vm = create_new_vm(session, "Other install media", sr_name, false);
    if (!session->ok)
    {
        /* Error has been logged, just clean up. */
        CLEANUP;
        return 1;
    }

    print_vm_power_state(session, hvm_vm);

    /* Blob handling used to crash when we tried to free the VM record:
     * CA-38872. */
    xen_blob blob;
    xen_blob_create(session, &blob, "hello", false);
    xen_vm_create_new_blob(session, &blob, hvm_vm, "test", "test", false);
    printf("\nBlob created.\n");
    xen_blob_record *blob_record;
    xen_blob_get_record(session, &blob_record, blob);
    printf("Blob record retrieved.\n");

    printf("\nGetting VM record...\n");
    xen_vm_record *vm_record;
    xen_vm_get_record(session, &vm_record, hvm_vm);
    printf("VM record: %s blob: %s\n",
           (char *)vm_record->handle,
           (char *)blob_record->handle);
    printf("Freeing VM record...\n");
    xen_vm_record_free(vm_record);
    printf("VM record freed.\n");
    printf("Freeing blob record...\n");
    xen_blob_record_free(blob_record);
    printf("Blob record freed.\n");
    
    if (!session->ok)
    {
        /* Error has been logged, just clean up. */
        xen_vm_free(hvm_vm);
        CLEANUP;
        return 1;
    }

    /*
       Test Enum parsing by setting actions after shutdown
     */

    xen_vm_set_actions_after_shutdown(session, hvm_vm, XEN_ON_NORMAL_EXIT_RESTART);

    /*
      Test getting a map and having a play
     */
    xen_string_string_map *hvm_boot_params;

    if (!xen_vm_get_hvm_boot_params(session, &hvm_boot_params, hvm_vm))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }

    printf("HVM_boot_params contains:\n");

    for (size_t i = 0; i < hvm_boot_params->size; i++)
    {
        printf("%s -> %s.\n", hvm_boot_params->contents[i].key,
               hvm_boot_params->contents[i].val);
    }

    xen_string_string_map_free(hvm_boot_params);

    cycle_vm(session, hvm_vm);

    xen_vm_free(hvm_vm);

    /* TODO uncomment this when we test against real
            hosts, as SDK doesn't have debian template
    
    printf("\n\nCreating new PV VM...\n");
    xen_vm pv_vm = create_new_vm(session, "Debian Etch 4.0", sr_name, true);
    if (!session->ok)
    {
        CLEANUP;
        return 1;
    }

    if (!xen_vm_get_hvm_boot_params(session, &hvm_boot_params, pv_vm))
    {
        print_error(session);
        CLEANUP;
        return 1;
    }

    for (size_t i = 0; i < hvm_boot_params->size; i++)
    {
        printf("%s -> %s.\n", hvm_boot_params->contents[i].key,
               hvm_boot_params->contents[i].val);
    }

    cycle_vm(session, pv_vm);

    xen_vm_free(pv_vm);
    */

    CLEANUP;
}

static int cycle_vm(xen_session *session, xen_vm vm)
{
    /* ---------------------------------------------------------------------
       Demonstrate some powercycle operations on VM:
       --------------------------------------------------------------------- */

    printf("Starting new VM..\n");
    xen_vm_start(session, vm, false, false);
    print_vm_power_state(session, vm);

    /* printf("Suspending new VM..\n");
    xen_vm_suspend(session, vm);
    print_vm_power_state(session, vm);

    printf("Resuming new VM..\n");
    xen_vm_resume(session, vm, false, false);
    print_vm_power_state(session, vm);*/

    printf("Shutting down VM..\n");
    xen_vm_hard_shutdown(session, vm);
    print_vm_power_state(session, vm);

    return 0;
}


/**
 * Creation of a new VM by cloning from an existing template (looked up by name)
 */
static xen_vm create_new_vm(xen_session *session, char* template_name, char* sr_name, bool PV)
{

    /*
     * Lookup template by name
     */
    xen_vm_set *vms;
    if (!xen_vm_get_by_name_label(session, &vms, template_name) ||
	vms->size < 1)
      {
	fprintf(stderr, "VM lookup failed.\n");
	print_error(session);
	return NULL;
      }
    
    /*
     * Create VM by cloning from template
     */
    xen_vm vm;
    char *name_before = "NewVM <&>"; //using xml sensitive characters
    int name_length = 9;
    xen_vm_clone(session, &vm, vms->contents[0], name_before);
    char *name_after;
    xen_vm_get_name_label(session, &name_after, vm);
    int result = strncmp(name_before, name_after, name_length);

    if (result != 0){
      fprintf(stderr, "Error: The VM name failed to be encoded/decoded correctly\n");
      fprintf(stderr, "Before:%s\nAfter:%s\n", name_before, name_after);
      return NULL;
    }
    
	xen_vm_set_free(vms);
   
    if (!session->ok) {
      fprintf(stderr, "VM clone failed.\n");
      print_error(session);
      return NULL;
    }

    if (PV) {

      xen_string_string_map *other_config;
      if (!xen_vm_get_other_config(session, &other_config, vm)) {
 
        fprintf(stderr, "VM get other_config failed.\n");
        print_error(session);
        return NULL;
      }
      
      char *disks = NULL;
      
      for (size_t i=0; i < other_config->size; i++) {
        
        printf("%s -> %s.\n", other_config->contents[i].key,
               other_config->contents[i].val);
        
        if (strcmp(other_config->contents[i].key, "disks") == 0) {          
          disks = other_config->contents[i].val;
          break;
        }
      }
      
      if (disks == NULL) {
        fprintf(stderr, "Did not find provision XML in other_config.\n");
        xen_string_string_map_free(other_config);
        return NULL;
      }
      
      xen_sr_set *srs;
      if (!xen_sr_get_by_name_label(session, &srs, sr_name) ||
          srs->size < 1) {
        
        fprintf(stderr, "SR lookup failed.\n");
        print_error(session);
        xen_vm_free(vm);
        return NULL;
      }

      xen_sr sr = srs->contents[0];

      char *sr_uuid;

      if(!xen_sr_get_uuid(session, &sr_uuid, sr)){
        //TODO free...?

        return NULL;
      }

      char *new_str;
      if(asprintf(&new_str, "sr=\"%s\"", sr_uuid) < 0) {
        return NULL;
      }

      char *new_disks = replace_str(disks, "sr=\"\"", new_str);

      free(new_str);

      xen_string_string_map_free(other_config);

      if (new_disks == NULL) {
        fprintf(stderr, "Error replacing SR in provision XML.\n");
        return NULL;
      }

      fprintf(stdout, "New provisions XML: %s\n", new_disks);

      if (!xen_vm_remove_from_other_config(session, vm, "disks")) {
        fprintf(stderr, "Error removing old value from other_config.\n");
        print_error(session);
        free(new_disks);
        return NULL;
      }
      
      if (!xen_vm_add_to_other_config(session, vm, "disks", new_disks)) {
        fprintf(stderr, "Error adding new value to other_config.\n");
        print_error(session);
        free(new_disks);
        return NULL;
      }

      free(new_disks);
    }
    
    xen_vm_set_name_description(session, vm, "An example VM created via C bindings");
    if (!session->ok)
      {
	fprintf(stderr, "Failed to set VM description.\n");
	print_error(session);
	return NULL;
      }
    
    xen_vm_provision(session, vm);
    if (!session->ok)
      {
	fprintf(stderr, "Failed to provision VM.\n");
	print_error(session);
	return NULL;
      }
 
    if (PV)
      return vm;

    /*
     * Create a new disk for the new VM.
     */
    printf("Creating new (blank) disk image in 'Shared SR'\n");
    xen_sr_set *srs;
    if (!xen_sr_get_by_name_label(session, &srs, sr_name) ||
        srs->size < 1)
    {
        fprintf(stderr, "SR lookup failed.\n");
        print_error(session);
        xen_vm_free(vm);
        return NULL;
    }

    xen_sr_record_opt sr_record =
        {
            .u.handle = srs->contents[0]
        };

    xen_string_string_map* other_config = xen_string_string_map_alloc(0);    
    xen_vdi_record vdi0_record =
        {
            .name_label = "MyRootFS",
            .name_description = "MyRootFS description",
            .sr = &sr_record,
            .virtual_size = (1024 * 1024 * 1024), /* 1 GiB in bytes */
            .type = XEN_VDI_TYPE_SYSTEM,
            .sharable = false,
            .read_only = false,
	    .other_config = other_config
        };

    xen_vdi vdi0;
    if (!xen_vdi_create(session, &vdi0, &vdi0_record))
    {
        fprintf(stderr, "VDI creation failed.\n");
        print_error(session);

        xen_sr_set_free(srs);
    
        xen_vm_free(vm);
        return NULL;
    }


    xen_vm_record_opt vm_record_opt =
        {
            .u.handle = vm
        };
    xen_vdi_record_opt vdi0_record_opt =
        {
            .u.handle = vdi0
        };
    xen_string_string_map* qos_algorithm_params = xen_string_string_map_alloc(0);    
    xen_string_string_map* vbd_other_config = xen_string_string_map_alloc(0);    

    enum xen_vbd_type vbd_type_disk = xen_vbd_type_from_string(session, "Disk");

    printf("Attaching disk image to newly created VM\n");
    xen_vbd_record vbd0_record =
        {
            .vm = &vm_record_opt,
            .vdi = &vdi0_record_opt,
            .userdevice = "xvda",
	    .type = vbd_type_disk,
            .mode = XEN_VBD_MODE_RW,
            .bootable = 1,
	    .qos_algorithm_params = qos_algorithm_params,
	    .other_config = vbd_other_config
        };

    xen_vbd vbd0;
    if (!xen_vbd_create(session, &vbd0, &vbd0_record))
    {
        fprintf(stderr, "VBD creation failed.\n");
        print_error(session);

        xen_vdi_free(vdi0);    
        xen_sr_set_free(srs);
        xen_vm_free(vm);
        return NULL;
    }

    char *vm_uuid;
    char *vdi0_uuid;
    char *vbd0_uuid;

    xen_vm_get_uuid(session,  &vm_uuid,   vm);
    xen_vdi_get_uuid(session, &vdi0_uuid, vdi0);
    xen_vbd_get_uuid(session, &vbd0_uuid, vbd0); 

    if (!session->ok)
    {
        fprintf(stderr, "get_uuid call failed.\n");
        print_error(session);
     
        xen_uuid_free(vm_uuid);
        xen_uuid_free(vdi0_uuid);
        xen_uuid_free(vbd0_uuid);
        xen_vbd_free(vbd0);
        xen_vdi_free(vdi0);
        xen_sr_set_free(srs);
        xen_vm_free(vm);
        return NULL;
    }

    fprintf(stderr,
	    "Created a new VM, with UUID %s, VDI UUID %s, VBD "
	    "UUID %s.\n",
	    vm_uuid, vdi0_uuid, vbd0_uuid);
 
    xen_uuid_free(vm_uuid);
    xen_uuid_free(vdi0_uuid);
    xen_uuid_free(vbd0_uuid);
    xen_vbd_free(vbd0);
    xen_vdi_free(vdi0);
    xen_sr_set_free(srs);

    return vm;
}


/**
 * Print the power state for the given VM.
 */
static void print_vm_power_state(xen_session *session, xen_vm vm)
{
    char *vm_uuid;
    enum xen_vm_power_state power_state;

    if (!xen_vm_get_uuid(session, &vm_uuid, vm))
    {
        print_error(session);
        return;
    }

    if (!xen_vm_get_power_state(session, &power_state, vm))
    {
        xen_uuid_free(vm_uuid);
        print_error(session);
        return;
    }

    printf("VM %s power state is %s.\n", vm_uuid,
           xen_vm_power_state_to_string(power_state));

    xen_uuid_free(vm_uuid);
}

/*
 * Replace all occurrences of orig in str with rep
 *
 * @returns newly malloc'd string - you must free it
 */
static char *replace_str(char *str, char *orig, char *rep)
{
  int occurrences = 0;
  int i = 0, k = 0;
  char *p = str;

  while ((p = strstr(p, orig)) != NULL) {
    
    ++occurrences;

    p += strlen(orig);
  }

  char *buffer = malloc(strlen(str) + 1 - (occurrences * (strlen(orig) - strlen(rep)))); 
  if(buffer == NULL)
    return NULL;  

  p = str;
  
  while ((p = strstr(p, orig)) != NULL) {

    int j = p - str - k;

    strncpy(buffer + i, str + k, j);
    
    i += j;
    
    strcpy(buffer + i, rep);

    i += strlen(rep);
    
    p += strlen(orig);
    k += j + strlen(orig);
  }

  strncpy(buffer + i, str + k, strlen(str + k));

  buffer[i + strlen(str + k)] = '\0';

  return buffer;
}
