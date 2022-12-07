/*
 * Copyright (C) Citrix Systems Inc.
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

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <xenctrl.h>
#include <xenforeignmemory.h>
#include <xentoollog.h>

#include <sys/mman.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#define _H(__h) (*((xc_interface **)Data_custom_val(__h)))
#define _D(__d) ((uint32_t)Int_val(__d))

/* From xenctrl_stubs */
#define ERROR_STRLEN 1024

#define Xtl_val(x) (*((struct xentoollog_logger **)Data_custom_val(x)))
#define Xfm_val(x) (*((struct xenforeignmemory_handle **)Data_abstract_val(x)))
#define Addr_val(x) (*((void **)Data_abstract_val(x)))

// Defined in OCaml 4.12: https://github.com/ocaml/ocaml/pull/9734
#if OCAML_VERSION < 41200
#define Some_val(v) Field(v, 0)
#define Is_some(v) Is_block(v)
#endif

static void stub_xenctrlext_finalize(value v) { xc_interface_close(_H(v)); }

static struct custom_operations xenctrlext_ops = {
    .identifier = "xapi-project.xenctrlext",
    .finalize = stub_xenctrlext_finalize,
    .compare = custom_compare_default,         /* Can't compare     */
    .hash = custom_hash_default,               /* Can't hash        */
    .serialize = custom_serialize_default,     /* Can't serialize   */
    .deserialize = custom_deserialize_default, /* Can't deserialize */
    .compare_ext = custom_compare_ext_default, /* Can't compare     */
};

static void raise_unix_errno_msg(int err_code, const char *err_msg)
{
    CAMLparam0();
    value args[] = { unix_error_of_code(err_code), caml_copy_string(err_msg) };

    caml_raise_with_args(*caml_named_value("Xenctrlext.Unix_error"),
                         sizeof(args) / sizeof(args[0]), args);
    CAMLnoreturn;
}

static void failwith_xc(xc_interface *xch)
{
    static char error_str[XC_MAX_ERROR_MSG_LEN + 6];
    int real_errno = errno;
    if ( xch )
    {
        snprintf(error_str, sizeof(error_str), "%d: %s", errno,
                 strerror(errno));
    }
    else
    {
        snprintf(error_str, sizeof(error_str), "Unable to open XC interface");
    }
    raise_unix_errno_msg(real_errno, error_str);
}

CAMLprim value stub_xenctrlext_interface_open(void)
{
    CAMLparam0();
    CAMLlocal1(result);
    xc_interface *xch;

    caml_enter_blocking_section();
    xch = xc_interface_open(NULL, NULL, 0);
    caml_leave_blocking_section();

    if ( !xch )
        failwith_xc(xch);

    result = caml_alloc_custom(&xenctrlext_ops, sizeof(xch), 0, 1);
    _H(result) = xch;

    CAMLreturn(result);
}

CAMLprim value stub_xenctrlext_get_runstate_info(value xch, value domid)
{
    CAMLparam2(xch, domid);
#if defined(XENCTRL_HAS_GET_RUNSTATE_INFO)
    CAMLlocal1(result);
    xc_runstate_info_t info;
    int retval;

    caml_enter_blocking_section();
    retval = xc_get_runstate_info(_H(xch), _D(domid), &info);
    caml_leave_blocking_section();
    if ( retval < 0 )
        failwith_xc(_H(xch));

    /* Store
       0 : state (int32)
       1 : missed_changes (int32)
       2 : state_entry_time (int64)
       3-8 : times (int64s)
    */
    result = caml_alloc_tuple(9);
    Store_field(result, 0, caml_copy_int32(info.state));
    Store_field(result, 1, caml_copy_int32(info.missed_changes));
    Store_field(result, 2, caml_copy_int64(info.state_entry_time));
    Store_field(result, 3, caml_copy_int64(info.time[0]));
    Store_field(result, 4, caml_copy_int64(info.time[1]));
    Store_field(result, 5, caml_copy_int64(info.time[2]));
    Store_field(result, 6, caml_copy_int64(info.time[3]));
    Store_field(result, 7, caml_copy_int64(info.time[4]));
    Store_field(result, 8, caml_copy_int64(info.time[5]));

    CAMLreturn(result);
#else
    caml_failwith("XENCTRL_HAS_GET_RUNSTATE_INFO not defined");
#endif
}

CAMLprim value stub_xenctrlext_get_boot_cpufeatures(value xch)
{
    CAMLparam1(xch);
#if defined(XENCTRL_HAS_GET_CPUFEATURES)
    CAMLlocal1(v);
    uint32_t a, b, c, d, e, f, g, h;
    int ret;

    caml_enter_blocking_section();
    ret = xc_get_boot_cpufeatures(_H(xch), &a, &b, &c, &d, &e, &f, &g, &h);
    caml_leave_blocking_section();
    if ( ret < 0 )
        failwith_xc(_H(xch));

    v = caml_alloc_tuple(8);
    Store_field(v, 0, caml_copy_int32(a));
    Store_field(v, 1, caml_copy_int32(b));
    Store_field(v, 2, caml_copy_int32(c));
    Store_field(v, 3, caml_copy_int32(d));
    Store_field(v, 4, caml_copy_int32(e));
    Store_field(v, 5, caml_copy_int32(f));
    Store_field(v, 6, caml_copy_int32(g));
    Store_field(v, 7, caml_copy_int32(h));

    CAMLreturn(v);
#else
    caml_failwith("XENCTRL_HAS_GET_CPUFEATURES not defined");
#endif
}

static int xcext_domain_send_s3resume(xc_interface *xch, unsigned int domid)
{
    return xc_set_hvm_param(xch, domid, HVM_PARAM_ACPI_S_STATE, 0);
}

static int xcext_domain_set_timer_mode(xc_interface *xch, unsigned int domid,
                                       int mode)
{
    return xc_set_hvm_param(xch, domid, HVM_PARAM_TIMER_MODE,
                            (unsigned long)mode);
}

CAMLprim value stub_xenctrlext_domain_get_acpi_s_state(value xch, value domid)
{
    CAMLparam2(xch, domid);
    unsigned long v;
    int ret;

    caml_enter_blocking_section();
    ret = xc_get_hvm_param(_H(xch), _D(domid), HVM_PARAM_ACPI_S_STATE, &v);
    caml_leave_blocking_section();
    if ( ret != 0 )
        failwith_xc(_H(xch));

    CAMLreturn(Val_int(v));
}

CAMLprim value stub_xenctrlext_domain_send_s3resume(value xch, value domid)
{
    CAMLparam2(xch, domid);
    caml_enter_blocking_section();
    xcext_domain_send_s3resume(_H(xch), _D(domid));
    caml_leave_blocking_section();
    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenctrlext_domain_set_timer_mode(value xch, value id,
                                                     value mode)
{
    CAMLparam3(xch, id, mode);
    int ret;

    caml_enter_blocking_section();
    ret = xcext_domain_set_timer_mode(_H(xch), _D(id), Int_val(mode));
    caml_leave_blocking_section();
    if ( ret < 0 )
        failwith_xc(_H(xch));
    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenctrlext_get_max_nr_cpus(value xch)
{
    CAMLparam1(xch);
    xc_physinfo_t c_physinfo;
    int r;

    caml_enter_blocking_section();
    r = xc_physinfo(_H(xch), &c_physinfo);
    caml_leave_blocking_section();

    if ( r )
        failwith_xc(_H(xch));

    CAMLreturn(Val_int(c_physinfo.max_cpu_id + 1));
}

CAMLprim value stub_xenctrlext_domain_set_target(value xch, value domid,
                                                 value target)
{
    CAMLparam3(xch, domid, target);

    caml_enter_blocking_section();
    int retval = xc_domain_set_target(_H(xch), _D(domid), _D(target));
    caml_leave_blocking_section();
    if ( retval )
        failwith_xc(_H(xch));
    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenctrlext_physdev_map_pirq(value xch, value domid,
                                                value irq)
{
    CAMLparam3(xch, domid, irq);
    int pirq = Int_val(irq);
    caml_enter_blocking_section();
    int retval = xc_physdev_map_pirq(_H(xch), _D(domid), pirq, &pirq);
    caml_leave_blocking_section();
    if ( retval )
        failwith_xc(_H(xch));
    CAMLreturn(Val_int(pirq));
} /* ocaml here would be int -> int */

CAMLprim value stub_xenctrlext_assign_device(value xch, value domid,
                                             value machine_sbdf, value flag)
{
    CAMLparam4(xch, domid, machine_sbdf, flag);
    caml_enter_blocking_section();
    int retval = xc_assign_device(_H(xch), _D(domid), Int_val(machine_sbdf),
                                  Int_val(flag));
    caml_leave_blocking_section();
    if ( retval )
        failwith_xc(_H(xch));
    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenctrlext_deassign_device(value xch, value domid,
                                               value machine_sbdf)
{
    CAMLparam3(xch, domid, machine_sbdf);
    caml_enter_blocking_section();
    int retval = xc_deassign_device(_H(xch), _D(domid), Int_val(machine_sbdf));
    caml_leave_blocking_section();
    if ( retval )
        failwith_xc(_H(xch));
    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenctrlext_domid_quarantine(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(DOMID_IO));
}

CAMLprim value stub_xenctrlext_domain_soft_reset(value xch, value domid)
{
    CAMLparam2(xch, domid);
    caml_enter_blocking_section();
    int retval = xc_domain_soft_reset(_H(xch), _D(domid));
    caml_leave_blocking_section();
    if ( retval )
        failwith_xc(_H(xch));
    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenctrlext_domain_update_channels(value xch, value domid,
                                                      value store_port,
                                                      value console_port)
{
    CAMLparam4(xch, domid, store_port, console_port);
    caml_enter_blocking_section();
    int retval = xc_set_hvm_param(_H(xch), _D(domid), HVM_PARAM_STORE_EVTCHN,
                                  Int_val(store_port));
    if ( !retval )
        retval = xc_set_hvm_param(_H(xch), _D(domid), HVM_PARAM_CONSOLE_EVTCHN,
                                  Int_val(console_port));
    caml_leave_blocking_section();
    if ( retval )
        failwith_xc(_H(xch));
    CAMLreturn(Val_unit);
}

/* based on xenctrl_stubs.c */
static int get_cpumap_len(value xch, value cpumap)
{
    int ml_len = Wosize_val(cpumap);
    caml_enter_blocking_section();
    int xc_len = xc_get_max_cpus(_H(xch));
    caml_leave_blocking_section();

    return (ml_len < xc_len ? ml_len : xc_len);
}

CAMLprim value stub_xenctrlext_vcpu_setaffinity_soft(value xch, value domid,
                                                     value vcpu, value cpumap)
{
    CAMLparam4(xch, domid, vcpu, cpumap);
    int i, len = get_cpumap_len(xch, cpumap);
    xc_cpumap_t c_cpumap;
    int retval;

    caml_enter_blocking_section();
    c_cpumap = xc_cpumap_alloc(_H(xch));
    caml_leave_blocking_section();
    if ( c_cpumap == NULL )
        failwith_xc(_H(xch));

    for ( i = 0; i < len; i++ )
    {
        if ( Bool_val(Field(cpumap, i)) )
            c_cpumap[i / 8] |= 1 << (i & 7);
    }
    caml_enter_blocking_section();
    retval = xc_vcpu_setaffinity(_H(xch), _D(domid), Int_val(vcpu), NULL,
                                 c_cpumap, XEN_VCPUAFFINITY_SOFT);
    free(c_cpumap);
    caml_leave_blocking_section();

    if ( retval < 0 )
        failwith_xc(_H(xch));
    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenctrlext_numainfo(value xch)
{
    CAMLparam1(xch);
    CAMLlocal5(meminfos, distances, result, info, row);
    unsigned max_nodes = 0;
    xc_meminfo_t *meminfo = NULL;
    uint32_t *distance = NULL;
    unsigned i, j;
    int retval;

    caml_enter_blocking_section();
    retval = xc_numainfo(_H(xch), &max_nodes, NULL, NULL);
    caml_leave_blocking_section();
    if ( retval < 0 )
        failwith_xc(_H(xch));

    meminfo = calloc(max_nodes, sizeof(*meminfo));
    distance = calloc(max_nodes * max_nodes, sizeof(*distance));
    if ( !meminfo || !distance )
    {
        free(meminfo);
        free(distance);
        caml_raise_out_of_memory();
    }

    caml_enter_blocking_section();
    retval = xc_numainfo(_H(xch), &max_nodes, meminfo, distance);
    caml_leave_blocking_section();
    if ( retval < 0 )
    {
        free(meminfo);
        free(distance);
        failwith_xc(_H(xch));
    }

    meminfos = caml_alloc_tuple(max_nodes);
    for ( i = 0; i < max_nodes; i++ )
    {
        info = caml_alloc_tuple(2);
        Store_field(info, 0, caml_copy_int64(meminfo[i].memfree));
        Store_field(info, 1, caml_copy_int64(meminfo[i].memsize));
        Store_field(meminfos, i, info);
    }

    distances = caml_alloc_tuple(max_nodes);
    for ( i = 0; i < max_nodes; i++ )
    {
        row = caml_alloc_tuple(max_nodes);
        for ( j = 0; j < max_nodes; j++ )
            Store_field(row, j, Val_int(distance[i * max_nodes + j]));
        Store_field(distances, i, row);
    }

    free(meminfo);
    free(distance);

    result = caml_alloc_tuple(2);
    Store_field(result, 0, meminfos);
    Store_field(result, 1, distances);

    CAMLreturn(result);
}

CAMLprim value stub_xenctrlext_cputopoinfo(value xch)
{
    CAMLparam1(xch);
    CAMLlocal2(topo, result);
    xc_cputopo_t *cputopo = NULL;
    unsigned max_cpus, i;
    int retval;

    caml_enter_blocking_section();
    retval = xc_cputopoinfo(_H(xch), &max_cpus, NULL);
    caml_leave_blocking_section();
    if ( retval < 0 )
        failwith_xc(_H(xch));

    cputopo = calloc(max_cpus, sizeof(*cputopo));
    if ( !cputopo )
        caml_raise_out_of_memory();

    caml_enter_blocking_section();
    retval = xc_cputopoinfo(_H(xch), &max_cpus, cputopo);
    caml_leave_blocking_section();
    if ( retval < 0 )
    {
        free(cputopo);
        failwith_xc(_H(xch));
    }

    result = caml_alloc_tuple(max_cpus);
    for ( i = 0; i < max_cpus; i++ )
    {
        topo = caml_alloc_tuple(3);
        Store_field(topo, 0, Val_int(cputopo[i].core));
        Store_field(topo, 1, Val_int(cputopo[i].socket));
        Store_field(topo, 2, Val_int(cputopo[i].node));
        Store_field(result, i, topo);
    }
    free(cputopo);

    CAMLreturn(result);
}

CAMLprim value stub_xenforeignmemory_open(value logger)
{
    CAMLparam1(logger);
    struct xentoollog_logger *log_handle = NULL;
    struct xenforeignmemory_handle *fmem;
    CAMLlocal1(result);

    if ( Is_some(logger) )
    {
        log_handle = Xtl_val(Some_val(logger));
    }

    // allocate memory to store the result, if the call to get the xfm
    // handle fails the ocaml GC will collect this abstract tag
    result = caml_alloc(1, Abstract_tag);

    caml_enter_blocking_section();
    fmem = xenforeignmemory_open(log_handle, 0);
    caml_leave_blocking_section();

    if ( fmem == NULL )
    {
        caml_failwith("Error when opening foreign memory handle");
    }

    Xfm_val(result) = fmem;

    CAMLreturn(result);
}

CAMLprim value stub_xenforeignmemory_close(value fmem)
{
    CAMLparam1(fmem);
    int retval;
    struct xenforeignmemory_handle *handle = Xfm_val(fmem);

    if ( handle == NULL )
    {
        caml_invalid_argument(
            "Error: cannot close NULL foreign memory handle");
    }

    caml_enter_blocking_section();
    retval = xenforeignmemory_close(handle);
    caml_leave_blocking_section();

    if ( retval < 0 )
    {
        caml_failwith("Error when closing foreign memory handle");
    }

    // Protect against double close
    Xfm_val(fmem) = NULL;

    CAMLreturn(Val_unit);
}

CAMLprim value stub_xenforeignmemory_map(value fmem, value dom,
                                         value prot_flags, value pages)
{
    CAMLparam4(fmem, dom, prot_flags, pages);
    CAMLlocal2(cell, result);
    size_t i, pages_length;
    xen_pfn_t *arr;
    int prot, the_errno;
    void *retval;
    struct xenforeignmemory_handle *handle = Xfm_val(fmem);

    if ( Field(prot_flags, 0) == Val_false && Field(prot_flags, 1) == Val_false
         && Field(prot_flags, 2) == Val_false )
    {
        prot = PROT_NONE;
    }
    else
    {
        prot = 0;
        if ( Field(prot_flags, 0) == Val_true )
        {
            prot |= PROT_READ;
        }
        if ( Field(prot_flags, 1) == Val_true )
        {
            prot |= PROT_WRITE;
        }
        if ( Field(prot_flags, 2) == Val_true )
        {
            prot |= PROT_EXEC;
        }
    }

    // traverse list to know the length of the array
    cell = pages;
    for ( pages_length = 0; cell != Val_emptylist; pages_length++ )
    {
        cell = Field(cell, 1);
    }

    // allocate and populate the array
    arr = malloc(sizeof(xen_pfn_t) * pages_length);
    if ( arr == NULL )
    {
        caml_failwith(
            "Error: could not allocate page array before mapping memory");
    }

    cell = pages;
    for ( i = 0; i < pages_length; i++ )
    {
        arr[i] = Int64_val(Field(cell, 0));
        cell = Field(cell, 1);
    }

    caml_enter_blocking_section();
    retval
        = xenforeignmemory_map(handle, _D(dom), prot, pages_length, arr, NULL);
    the_errno = errno;
    free(arr);
    caml_leave_blocking_section();

    if ( retval == NULL )
    {
        raise_unix_errno_msg(the_errno,
                             "Error when trying to map foreign memory");
    }

    result = caml_ba_alloc_dims(CAML_BA_CHAR | CAML_BA_C_LAYOUT
                                    | CAML_BA_EXTERNAL,
                                1, retval, (long)4096 * pages_length);

    CAMLreturn(result);
}

CAMLprim value stub_xenforeignmemory_unmap(value fmem, value mapping)
{
    CAMLparam2(fmem, mapping);
    size_t pages;
    int retval, the_errno;
    struct xenforeignmemory_handle *handle = Xfm_val(fmem);
    void *data = Caml_ba_data_val(mapping);

    // convert mapping to pages and addr
    pages = Caml_ba_array_val(mapping)->dim[0] / 4096;

    caml_enter_blocking_section();
    retval = xenforeignmemory_unmap(handle, data, pages);
    caml_leave_blocking_section();
    the_errno = errno;

    if ( retval < 0 )
    {
        raise_unix_errno_msg(the_errno,
                             "Error when trying to unmap foreign memory");
    }

    CAMLreturn(Val_unit);
}

/*
 * Local variables:
 * indent-tabs-mode: t
 */
