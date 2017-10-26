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
 * This sample program demonstrates how to traverse the xenapi objects of
 * various types. It gets all the VMs on a host and then prints out
 * info on each VM. The disks of each VM are also enumerated and printed,
 * following VBD and VDI references.
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

/*------------------ defines and typedefs -----------------------------*/

#define INDENTATION_LEVEL 2
#define RIGHT_JUST 30

#define PRINT_INDENT(indent, string, format, ... )                 \
    {                                                              \
        int j;                                                     \
        for (j=0;j<(int)indent;j++)                                \
        {                                                          \
            printf(" ");                                           \
        }                                                          \
        printf("%s", string);                                      \
        for (j=0;j<(int)(RIGHT_JUST-indent-strlen(string)); j++)   \
        {                                                          \
            printf(" ");                                           \
        }                                                          \
        printf(format, __VA_ARGS__);                               \
        printf("\n");                                              \
    }

typedef struct
{
    xen_result_func func;
    void *handle;
} xen_comms;

/*-------------------------------------------------------------------------*/

static char *url;

static void
usage()
{
    fprintf(stderr,
            "Usage:\n"
            "\n"
            "    test_enumerate <url> <username> <password>\n"
            "\n"
            "where\n"
            "        <url>      is the server's URL, e.g. https://server.example.com\n"
            "        <username> is the username to use at the server; and\n"
            "        <password> is the password.\n");

    exit(EXIT_FAILURE);
}

static size_t
write_func(void *ptr, size_t size, size_t nmemb, xen_comms *comms)
{
    size_t n = size * nmemb;
#ifdef PRINT_XML
    printf("Data from server:\n%s\n", ((char*) ptr));
#endif
    return comms->func(ptr, n, comms->handle) ? n : 0;
}

static int
call_func(const void *data, size_t len, void *user_handle,
        void *result_handle, xen_result_func result_func)
{
    (void) user_handle;

#ifdef PRINT_XML
    printf("Data to server:\n%s\n", ((char*) data));
#endif

    CURL *curl = curl_easy_init();
    if (!curl)
        return -1;

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

static void
dump_string_map(xen_string_string_map *smap, char *string, int indent)
{
    unsigned int i;
    char buf[512];

    if (!smap)
        return;

    PRINT_INDENT(indent, string, "%s", "");

    indent += INDENTATION_LEVEL;

    for (i = 0; i < smap->size; i++)
    {
        sprintf(buf, "[%s, %s]", smap->contents[i].key, smap->contents[i].val);
        PRINT_INDENT(indent, buf, "%s", "");
    }
}

static void
dump_string_set(xen_string_set *sset, char *string, int indent)
{
    unsigned int i;

    if (!sset)
        return;

    PRINT_INDENT(indent, string, "%s", "");

    indent += INDENTATION_LEVEL;

    for (i = 0; i < sset->size; i++)
        PRINT_INDENT(indent, "", "%s", sset->contents[i]);
}


//dump a virtual disk interface (VDI) record

static void
dump_vdi_record(xen_vdi_record *vrec, int indent)
{
    indent += INDENTATION_LEVEL;

    PRINT_INDENT(indent, "uuid:", "%s", vrec->uuid);
    PRINT_INDENT(indent, "name_label:", "%s", vrec->name_label);
    PRINT_INDENT(indent, "name_description:", "%s", vrec->name_description);
    PRINT_INDENT(indent, "virtual_size:", "%"PRId64, vrec->virtual_size);
    PRINT_INDENT(indent, "physical_utilisation:", "%"PRId64, vrec->physical_utilisation);
    PRINT_INDENT(indent, "type:", "%s", xen_vdi_type_to_string(vrec->type));
    PRINT_INDENT(indent, "sharable:", "%d", vrec->sharable);
    PRINT_INDENT(indent, "read_only:", "%d", vrec->read_only);
}

static void
dump_vdi_opt_record(xen_session *session, xen_vdi_record_opt *voptr, int indent)
{
    xen_vdi_record *vrec;

    if (voptr->is_record)
    {
        dump_vdi_record(voptr->u.record, indent);
    }
    else
    {
        if (xen_vdi_get_record(session, &vrec, voptr->u.handle))
        {
            dump_vdi_record(vrec, indent);
            xen_vdi_record_free(vrec);
        }
    }
}


// dump a virtual block device record

static void
dump_vbd_record(xen_session *session, xen_vbd_record *vrec, int indent)
{
    indent += INDENTATION_LEVEL;

    PRINT_INDENT(indent, "uuid:", "%s", vrec->uuid);

    //struct xen_vm_record_opt *vm; -- no need to follow to vm's ... we are dumping vdi's
    //struct xen_vdi_record_opt *vdi; -- no need to follow to vdi ... we are dumping vdi's

    PRINT_INDENT(indent, "device:", "%s", vrec->device);
    PRINT_INDENT(indent, "bootable:", "%d", vrec->bootable);
    PRINT_INDENT(indent, "mode:", "%d", vrec->mode);
    PRINT_INDENT(indent, "type:", "%d", vrec->type);
    PRINT_INDENT(indent, "currently_attached:", "%d", vrec->currently_attached);
    PRINT_INDENT(indent, "status_code:", "%"PRId64, vrec->status_code);
    PRINT_INDENT(indent, "status_detail:", "%s", vrec->status_detail);
    PRINT_INDENT(indent, "qos_algorithm_type:", "%s", vrec->qos_algorithm_type);

    dump_string_set(vrec->qos_supported_algorithms, "qos_supported_algorithms:", indent);

    if (vrec->vdi)
    {
        PRINT_INDENT(indent, "VDI:", "%s", "");
        dump_vdi_opt_record(session, vrec->vdi, indent);
    }

    //struct xen_vbd_metrics_record_opt *metrics;
}

static void
dump_vbd_opt_record(xen_session *session, xen_vbd_record_opt *voptr, int indent)
{
    xen_vbd_record *vrec;

    if (voptr->is_record)
    {
        dump_vbd_record(session, voptr->u.record, indent);
    }
    else
    {
        if (xen_vbd_get_record(session, &vrec, voptr->u.handle))
        {
            dump_vbd_record(session, vrec, indent);
            xen_vbd_record_free(vrec);
        }
    }
}

static void
dump_console_record(xen_console_record *crec, int indent)
{
    indent += INDENTATION_LEVEL;

    PRINT_INDENT(indent, "uuid:", "%s", crec->uuid);
    PRINT_INDENT(indent, "protocol:", "%s", xen_console_protocol_to_string(crec->protocol));
    PRINT_INDENT(indent, "location:", "%s", crec->location);

    //struct xen_vm_record_opt *vm; -- no need to follow to vm ... we are dumping vm's

    dump_string_map(crec->other_config, "other_config:", indent);
}

static void
dump_consoles(xen_session *session, xen_console_record_opt_set *cons, int indent)
{
    unsigned int i;
    xen_console_record *crec;

    for (i = 0; i < cons->size; i++)
    {
        xen_console_record_opt *ccont = cons->contents[i];
        if (ccont->is_record)
        {
            dump_console_record(ccont->u.record, indent);
        }
        else
        {
            if (xen_console_get_record(session, &crec, ccont->u.handle))
            {
                dump_console_record(crec, indent);
                xen_console_record_free(crec);
            }
        }
    }
}

static void
dump_vm(xen_session *session, struct xen_vm_set *vm_set, int indent)
{
    unsigned int i;
    xen_vm_record *rec = NULL;

    indent += INDENTATION_LEVEL;

    for (i = 0; i < vm_set->size; i++)
    {
        if (xen_vm_get_record(session, &rec, vm_set->contents[i]))
        {
            if (rec->is_a_template) continue;
            printf("------------------------------------------------------------------------------\n");
            printf("Virtual Machine: %s\n", rec->name_label);
            printf("------------------------------------------------------------------------------\n");
            PRINT_INDENT(indent, "uuid:", "%s", rec->uuid);
            PRINT_INDENT(indent, "power_state:", "%s", xen_vm_power_state_to_string(rec->power_state));
            PRINT_INDENT(indent, "name_label:", "%s", rec->name_label);
            PRINT_INDENT(indent, "name_description:", "%s", rec->name_description);
            PRINT_INDENT(indent, "user_version:", "%"PRId64, rec->user_version);
            PRINT_INDENT(indent, "is_a_template:", "%d", rec->is_a_template);

            // -- no need to follow resident_on -- it points back to current host
            PRINT_INDENT(indent, "memory_static_max:", "%"PRId64, rec->memory_static_max);
            PRINT_INDENT(indent, "memory_static_min:", "%"PRId64, rec->memory_static_min);
            PRINT_INDENT(indent, "memory_dynamic_max:", "%"PRId64, rec->memory_dynamic_max);
            PRINT_INDENT(indent, "memory_dynamic_min:", "%"PRId64, rec->memory_dynamic_min);
            if (rec->vcpus_params)
            {
                dump_string_map(rec->vcpus_params, "vcpus_params:", indent);
            }
            PRINT_INDENT(indent, "vcpus_max:", "%"PRId64, rec->vcpus_max);
            PRINT_INDENT(indent, "vcpus_at_startup:", "%"PRId64, rec->vcpus_at_startup);
            PRINT_INDENT(indent, "actions_after_shutdown:", "%d", rec->actions_after_shutdown);
            PRINT_INDENT(indent, "actions_after_reboot:", "%d", rec->actions_after_reboot);
            PRINT_INDENT(indent, "actions_after_crash:", "%d", rec->actions_after_crash);
            if (rec->consoles)
            {
                PRINT_INDENT(indent, "consoles:", "%s", "");
                dump_consoles(session, rec->consoles, indent);
            }
            if (rec->vbds)
            {
                unsigned int j;
                PRINT_INDENT(indent, "VBDS:", "%s", "");
                for (j = 0; j < rec->vbds->size; j++)
                    dump_vbd_opt_record(session, rec->vbds->contents[j], indent);
            }
            PRINT_INDENT(indent, "pv_bootloader:", "%s", rec->pv_bootloader);
            PRINT_INDENT(indent, "pv_kernel:", "%s", rec->pv_kernel);
            PRINT_INDENT(indent, "pv_ramdisk:", "%s", rec->pv_ramdisk);
            PRINT_INDENT(indent, "pv_args:", "%s", rec->pv_args);
            PRINT_INDENT(indent, "pv_bootloader_args:", "%s", rec->pv_bootloader_args);
            PRINT_INDENT(indent, "hvm_boot_policy:", "%s", rec->hvm_boot_policy);
            if (rec->hvm_boot_params)
            {
                dump_string_map(rec->hvm_boot_params, "hvm_boot_params:", indent);
            }

            PRINT_INDENT(indent, "pci_bus:", "%s", rec->pci_bus);
            if (rec->other_config)
            {
                dump_string_map(rec->other_config, "other_config:", indent);
            }
            PRINT_INDENT(indent, "domid:", "%"PRId64, rec->domid);
            PRINT_INDENT(indent, "is_control_domain:", "%d", rec->is_control_domain);
        }
        printf("------------------------------------------------------------------------------\n");
        session->ok = true;
        printf("\n");
        xen_vm_record_free(rec);
    }
}

int
main(int argc, char **argv)
{
    if (argc != 4)
    {
        usage();
    }

    url = argv[1];
    char *username = argv[2];
    char *password = argv[3];

    struct xen_vm_set *vm_set = 0;

    xmlInitParser();
    xen_init();
    curl_global_init(CURL_GLOBAL_ALL);

    xen_session *session = xen_session_login_with_password(call_func, NULL, username,
            password, xen_api_latest_version);

    /* get all vm entries */
    if (xen_vm_get_all(session, &vm_set))
        dump_vm(session, vm_set, 0);

    if (vm_set)
        xen_vm_set_free(vm_set);

    return 0;
}
