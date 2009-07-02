/* Copyright (c) 2005-2006 XenSource Inc. */

#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>

#include <xenctrl.h>
#include <xenguest.h>
#include <xs.h>
#include <xen/hvm/hvm_info_table.h>
#include <xen/hvm/params.h>
#include <xen/hvm/e820.h>
#include <sys/mman.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/fail.h>

#define _H(__h) (Int_val(__h))
#define _D(__d) ((uint32_t)Int_val(__d))

#define None_val (Val_int(0))

#ifndef HVM_PARAM_NX_ENABLED
#define XEN_UNSTABLE
#endif 

#ifndef HVM_PARAM_VIRIDIAN
#warning missing viridian parameter
#endif

#include <stdio.h>

static void failwith_oss_xc(char *fct)
{
	char buf[80];
	const xc_error *error;

	error = xc_get_last_error();
	if (error->code == XC_ERROR_NONE)
		snprintf(buf, 80, "%s: [%d] %s", fct, errno, strerror(errno));
	else
		snprintf(buf, 80, "%s: [%d] %s", fct, error->code, error->message);
	xc_clear_last_error();
	caml_failwith(buf);
}

static int dispatch_suspend(int domid)
{
	value * __suspend_closure;
	int ret;

	__suspend_closure = caml_named_value("suspend_callback");
	if (!__suspend_closure)
		return 0;
	caml_leave_blocking_section();
	ret = Int_val(caml_callback(*__suspend_closure, Val_int(domid)));
	caml_enter_blocking_section();
	return ret;
}

static int suspend_flag_list[] = {
	XCFLAGS_DEBUG, XCFLAGS_LIVE, XCFLAGS_HVM
};

CAMLprim value stub_xenguest_init()
{
	int handle;

	handle = xc_interface_open();
	if (handle == -1)
		failwith_oss_xc("xc_interface_open");
	return Val_int(handle);
}

CAMLprim value stub_xenguest_close(value xenguest_handle)
{
	CAMLparam1(xenguest_handle);
	xc_interface_close(Int_val(xenguest_handle));
	CAMLreturn(Val_unit);
}

extern struct xc_dom_image *xc_dom_allocate(const char *cmdline, const char *features);


CAMLprim value stub_xc_linux_build_native(value xc_handle, value domid,
                                          value mem_mb, value image_name,
                                          value ramdisk_name, value cmdline,
                                          value features, value flags,
                                          value store_evtchn, value console_evtchn)
{
	CAMLparam5(xc_handle, domid, mem_mb, image_name, ramdisk_name);
	CAMLxparam4(cmdline, flags, store_evtchn, console_evtchn);
	CAMLlocal1(result);

	unsigned long store_mfn;
	unsigned long console_mfn;
	int r;
	struct xc_dom_image *dom;
	char c_protocol[64];

	/* Copy the ocaml values into c-land before dropping the mutex */
	int c_xc_handle = _H(xc_handle);
	unsigned int c_mem_mb = Int_val(mem_mb);
	uint32_t c_domid = _D(domid);
	char *c_image_name = strdup(String_val(image_name));
	char *c_ramdisk_name = ramdisk_name == None_val ? NULL : strdup(String_val(Field(ramdisk_name, 0)));
	unsigned long c_flags = Int_val(flags);
	unsigned int c_store_evtchn = Int_val(store_evtchn);
	unsigned int c_console_evtchn = Int_val(console_evtchn);

	xc_dom_loginit();
	dom = xc_dom_allocate(String_val(cmdline), String_val(features));
	if (!dom)
		failwith_oss_xc("xc_dom_allocate");

	caml_enter_blocking_section();
	r = xc_dom_linux_build(c_xc_handle, dom, c_domid, c_mem_mb,
	                       c_image_name, c_ramdisk_name, c_flags,
	                       c_store_evtchn, &store_mfn,
	                       c_console_evtchn, &console_mfn);
	caml_leave_blocking_section();

#ifndef XEN_UNSTABLE
	strncpy(c_protocol, xc_dom_get_native_protocol(dom), 64);
#else
	memset(c_protocol, '\0', 64);
#endif
	free(c_image_name);
	free(c_ramdisk_name);
	xc_dom_release(dom);

	if (r != 0)
		failwith_oss_xc("xc_dom_linux_build");

	result = caml_alloc_tuple(3);
	Store_field(result, 0, caml_copy_nativeint(store_mfn));
	Store_field(result, 1, caml_copy_nativeint(console_mfn));
	Store_field(result, 2, caml_copy_string(c_protocol));

	CAMLreturn(result);
}


CAMLprim value stub_xc_linux_build_bytecode(value * argv, int argn)
{
	return stub_xc_linux_build_native(argv[0], argv[1], argv[2], argv[3],
	                                  argv[4], argv[5], argv[6], argv[7],
	                                  argv[8], argv[9]);
}

static int pasprintf(char **buf, const char *fmt, ...)
{
    va_list ap;
    int ret = 0;

    if (*buf)
        free(*buf);
    va_start(ap, fmt);
    if (vasprintf(buf, fmt, ap) == -1) {
        buf = NULL;
        ret = -1;
    }
    va_end(ap);
    return ret;
}

static unsigned int
xenstore_get(char *key, int domid)
{
    char *buf = NULL, *path = NULL, *s;
    unsigned int value = 0;
    struct xs_handle *xsh = NULL;

    xsh = xs_daemon_open();
    if (xsh == NULL)
           return 0;

    path = xs_get_domain_path(xsh, domid);
    if (path == NULL)
        goto out;
    pasprintf(&buf, "%s/platform/%s", path, key);

    s = xs_read(xsh, XBT_NULL, buf, NULL);
    if (s) {
       if (!strcasecmp(s, "true"))
           value = 1;
       else if (sscanf(s, "%d", &value) != 1)
           value = 0;
       free(s);
    }

  out:
    xs_daemon_close(xsh);
    free(path);
    free(buf);
    return value;
}

static int hvm_build_set_params(int handle, int domid,
                                int apic, int acpi, int pae, int nx, int viridian, int vcpus,
                                int store_evtchn, unsigned long *store_mfn, 
                                uint32_t frames)
{
	struct hvm_info_table *va_hvm;
	uint8_t *va_map, sum;
	int i;

	va_map = xc_map_foreign_range(handle, domid,
			    XC_PAGE_SIZE, PROT_READ | PROT_WRITE,
			    HVM_INFO_PFN);
	if (va_map == NULL)
		return -1;

	va_hvm = (struct hvm_info_table *)(va_map + HVM_INFO_OFFSET);
	memset(va_hvm, 0, sizeof(*va_hvm));
	strncpy(va_hvm->signature, "HVM INFO", 8);
	va_hvm->length = sizeof(struct hvm_info_table);
	va_hvm->acpi_enabled = acpi;
	va_hvm->apic_mode = apic;
	va_hvm->nr_vcpus = vcpus;

#ifdef HVM_PARAM_VPT_ALIGN /* Ugly test for xen version with these fields */
        if ( frames <= (HVM_BELOW_4G_RAM_END >> 12) ) {
            va_hvm->low_mem_pgend = frames;
            va_hvm->high_mem_pgend = 0;
        } else {
            va_hvm->low_mem_pgend = (HVM_BELOW_4G_RAM_END >> 12);
            va_hvm->high_mem_pgend = 0x100000 + frames - va_hvm->low_mem_pgend;
        }
        /* PFN of the bufioreq page: here to 4G is out of bounds */
        {
            unsigned long pfn;
            xc_get_hvm_param(handle, domid, HVM_PARAM_BUFIOREQ_PFN, &pfn);
            va_hvm->reserved_mem_pgstart = pfn;
        }
#endif

        va_hvm->s4_enabled = xenstore_get("acpi_s4", domid);
        va_hvm->s3_enabled = xenstore_get("acpi_s3", domid);
	for (i = 0, sum = 0; i < va_hvm->length; i++)
		sum += ((uint8_t *) va_hvm)[i];
	va_hvm->checksum = -sum;
	munmap(va_map, XC_PAGE_SIZE);

	xc_get_hvm_param(handle, domid, HVM_PARAM_STORE_PFN, store_mfn);
	xc_set_hvm_param(handle, domid, HVM_PARAM_PAE_ENABLED, pae);
#ifdef HVM_PARAM_VIRIDIAN
	xc_set_hvm_param(handle, domid, HVM_PARAM_VIRIDIAN, viridian);
#endif
	xc_set_hvm_param(handle, domid, HVM_PARAM_STORE_EVTCHN, store_evtchn);
#ifndef XEN_UNSTABLE
	xc_set_hvm_param(handle, domid, HVM_PARAM_NX_ENABLED, nx);
#endif
	return 0;
}

CAMLprim value stub_xc_hvm_build_native(value xc_handle, value domid,
                                        value memsize, value image_name,
                                        value vcpus, value pae, value apic,
                                        value acpi, value nx, value viridian,
                                        value store_evtchn)
{
	CAMLparam5(xc_handle, domid, memsize, image_name, vcpus);
	CAMLxparam5(pae, apic, acpi, nx, store_evtchn);
	CAMLlocal1(ret);
	char *image_name_c = strdup(String_val(image_name));
	char *error[256];

	unsigned long store_mfn=0;
	int r;

	caml_enter_blocking_section ();
	r = xc_hvm_build(_H(xc_handle), _D(domid), Int_val(memsize),
	                 image_name_c);
	caml_leave_blocking_section ();

	free(image_name_c);

	if (r)
		failwith_oss_xc("hvm_build");


	r = hvm_build_set_params(_H(xc_handle), _D(domid),
	                         Bool_val(apic), Bool_val(acpi),
	                         Bool_val(pae), Bool_val(nx), Bool_val(viridian),
				 Int_val(vcpus),
	                         Int_val(store_evtchn), &store_mfn,
	                         ((uint32_t) Int_val(memsize)) << (20 - 12));
	if (r)
		failwith_oss_xc("hvm_build_params");

	ret = caml_copy_nativeint(store_mfn);
	CAMLreturn(ret);
}

CAMLprim value stub_xc_hvm_build_bytecode(value * argv, int argn)
{
	return stub_xc_hvm_build_native(argv[0], argv[1], argv[2], argv[3],
	                                argv[4], argv[5], argv[6], argv[7],
	                                argv[8], argv[9], argv[10]);
}

CAMLprim value stub_xc_hvm_build_mem_native(value xc_handle, value domid,
                                            value memsize, value image_buffer,
                                            value image_size, value vcpus,
                                            value pae, value apic, value acpi,
                                            value nx, value viridian, value store_evtchn)
{
	CAMLparam5(xc_handle, domid, memsize, image_buffer, image_size);
	CAMLxparam5(vcpus, pae, apic, acpi, nx);
	CAMLxparam1(store_evtchn);
	CAMLlocal1(ret);
	unsigned long store_mfn;
	unsigned long c_image_size;
	int r;

	c_image_size = Nativeint_val(image_size);

	caml_enter_blocking_section ();
	r = xc_hvm_build_mem(_H(xc_handle), _D(domid), Int_val(memsize),
	                     String_val(image_buffer), c_image_size);
	caml_leave_blocking_section ();

	if (r)
		failwith_oss_xc("xc_hvm_build_mem");

	r = hvm_build_set_params(_H(xc_handle), _D(domid),
	                         Bool_val(apic), Bool_val(acpi),
	                         Bool_val(pae), Bool_val(nx), Bool_val(viridian),
				 Int_val(vcpus),
	                         Int_val(store_evtchn), &store_mfn,
	                         ((uint32_t) Int_val(memsize)) << (20 - 12));
	if (r)
		failwith_oss_xc("hvm_build_params");

	ret = caml_copy_nativeint(store_mfn);
	CAMLreturn(ret);
}

CAMLprim value stub_xc_hvm_build_mem_bytecode(value * argv, int argn)
{
	return stub_xc_hvm_build_mem_native(argv[0], argv[1], argv[2], argv[3],
	                                    argv[4], argv[5], argv[6], argv[7],
	                                    argv[8], argv[9], argv[10], argv[11]);
}

extern void qemu_flip_buffer(int domid, int next_active);
extern void * init_qemu_maps(int domid, unsigned int bitmap_size);

CAMLprim value stub_xc_domain_save(value handle, value fd, value domid,
                                   value max_iters, value max_factors,
                                   value flags, value hvm)
{
	CAMLparam5(handle, fd, domid, max_iters, max_factors);
	CAMLxparam2(flags, hvm);
	void *(*init_maps)(int, unsigned);
	void (*flip_buffer)(int, int);
	uint32_t c_flags;
	int r;

	init_maps = (Bool_val(hvm)) ? init_qemu_maps : NULL;
	flip_buffer = (Bool_val(hvm)) ? qemu_flip_buffer : NULL;

	c_flags = caml_convert_flag_list(flags, suspend_flag_list);

	caml_enter_blocking_section();
	r = xc_domain_save(_H(handle), Int_val(fd), _D(domid),
	                   Int_val(max_iters), Int_val(max_factors),
	                   c_flags, dispatch_suspend,
	                   Bool_val(hvm), init_maps, flip_buffer);
	caml_leave_blocking_section();
	if (r)
		failwith_oss_xc("xc_domain_save");

	CAMLreturn(Val_unit);
}

CAMLprim value stub_xc_domain_save_bytecode(value *argv, int argn)
{
	return stub_xc_domain_save(argv[0], argv[1], argv[2], argv[3],
	                           argv[4], argv[5], argv[6]);
}

/* this is the slow version of resume for uncooperative domain,
 * the fast version is available in close source xc */
CAMLprim value stub_xc_domain_resume_slow(value handle, value domid)
{
	CAMLparam2(handle, domid);
	int r;

	/* hard code fast to 0, we only want to expose the slow version here */
	r = xc_domain_resume(_H(handle), _D(domid), 0);
	if (r)
		failwith_oss_xc("xc_domain_resume");
	CAMLreturn(Val_unit);
}


CAMLprim value stub_xc_domain_restore(value handle, value fd, value domid,
                                      value store_evtchn, value console_evtchn,
                                      value hvm, value pae, value viridian)
{
	CAMLparam5(handle, fd, domid, store_evtchn, console_evtchn);
	CAMLxparam3(hvm, pae, viridian);
	CAMLlocal1(result);
	unsigned long store_mfn, console_mfn;
	unsigned int c_store_evtchn, c_console_evtchn;
	int r;

	c_store_evtchn = Int_val(store_evtchn);
	c_console_evtchn = Int_val(console_evtchn);

#ifdef HVM_PARAM_VIRIDIAN
	xc_set_hvm_param(_H(handle), _D(domid), HVM_PARAM_VIRIDIAN, Bool_val(viridian));	
#endif

	caml_enter_blocking_section();
	r = xc_domain_restore(_H(handle), Int_val(fd), _D(domid),
	                      c_store_evtchn, &store_mfn,
	                      c_console_evtchn, &console_mfn,
			      Bool_val(hvm), Bool_val(pae));
	caml_leave_blocking_section();
	if (r)
		failwith_oss_xc("xc_domain_restore");

	result = caml_alloc_tuple(2);
	Store_field(result, 0, caml_copy_nativeint(store_mfn));
	Store_field(result, 1, caml_copy_nativeint(console_mfn));

	CAMLreturn(result);
}

CAMLprim value stub_xc_domain_restore_bytecode(value * argv, int argn)
{
	return stub_xc_domain_restore(argv[0], argv[1], argv[2], argv[3],
	                              argv[4], argv[5], argv[6], argv[7]);
}

CAMLprim value stub_xc_domain_dumpcore(value handle, value domid, value file)
{
	CAMLparam3(handle, domid, file);
	int r;

	r = xc_domain_dumpcore(_H(handle), _D(domid), String_val(file));
	if (r)
		failwith_oss_xc("xc_domain_dumpcore");
	CAMLreturn(Val_unit);
}
