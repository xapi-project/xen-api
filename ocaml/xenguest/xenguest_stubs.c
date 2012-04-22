/*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <syslog.h>

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

#define _H(__h) ((xc_interface *)(__h))
#define _D(__d) ((uint32_t)Int_val(__d))

#define None_val (Val_int(0))

#ifndef HVM_PARAM_NX_ENABLED
#define XEN_UNSTABLE
#endif 

#ifndef HVM_PARAM_VIRIDIAN
#warning missing viridian parameter
#endif

#include <stdio.h>

/* The following boolean flags are all set by their value
   in the platform area of xenstore. The only value that
   is considered true is the string 'true' */
struct flags {
  int vcpus;
  int vcpus_current;
  const char** vcpu_affinity; /* 0 means unset */
  uint16_t vcpu_weight;   /* 0 means unset (0 is an illegal weight) */
  uint16_t vcpu_cap;      /* 0 is default (no cap) */
  int nx;
  int viridian;
  int pae;
  int acpi;
  int apic;
  int acpi_s3;
  int acpi_s4;
  uint64_t mmio_size_mib;
};

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

static char *
xenstore_getsv(int domid, const char *fmt, va_list ap)
{
    char *path = NULL, *s;
    uint64_t value = 0;
    struct xs_handle *xsh = NULL;
	int n, m;
	char key[1024];

	bzero(key, sizeof(key));

    xsh = xs_daemon_open();
    if (xsh == NULL)
           return 0;

    path = xs_get_domain_path(xsh, domid);
    if (path == NULL)
        goto out;

	n = snprintf(key, sizeof(key), "%s/platform/", path);
	if (n < 0)
	  goto out;
	m = vsnprintf(key + n, sizeof(key) - n, fmt, ap);
	if (m < 0)
	  goto out;

    s = xs_read(xsh, XBT_NULL, key, NULL);
  out:
    xs_daemon_close(xsh);
    free(path);
    return s;
}

static char *
xenstore_gets(int domid, const char *fmt, ...)
{
	char *s;
	va_list ap;

	va_start(ap, fmt);
	s = xenstore_getsv(domid, fmt, ap);
	va_end(ap);
	return s;
}

static uint64_t
xenstore_get(int domid, const char *fmt, ...)
{
	char *s;
	uint64_t value = 0;
	va_list ap;

	va_start(ap, fmt);
	s = xenstore_getsv(domid, fmt, ap);
    if (s) {
       if (!strcasecmp(s, "true"))
           value = 1;
       else if (sscanf(s, "%Ld", &value) != 1)
           value = 0;
       free(s);
    }
	va_end(ap);
    return value;
}

static void 
get_flags(struct flags *f, int domid) 
{
  int n;
  f->vcpus    = xenstore_get(domid, "vcpu/number");
  f->vcpu_affinity = (const char**)(malloc(sizeof(char*) * f->vcpus));

  for (n = 0; n < f->vcpus; n++) {
	f->vcpu_affinity[n] = xenstore_gets(domid, "vcpu/%d/affinity", n);
  }
  f->vcpus_current = xenstore_get(domid, "vcpu/current");
  f->vcpu_weight = xenstore_get(domid, "vcpu/weight");
  f->vcpu_cap = xenstore_get(domid, "vcpu/cap");
  f->nx       = xenstore_get(domid, "nx");
  f->viridian = xenstore_get(domid, "viridian");
  f->apic     = xenstore_get(domid, "apic");
  f->acpi     = xenstore_get(domid, "acpi");
  f->pae      = xenstore_get(domid, "pae");
  f->acpi_s4  = xenstore_get(domid, "acpi_s4");
  f->acpi_s3  = xenstore_get(domid, "acpi_s3");
  f->mmio_size_mib = xenstore_get(domid, "mmio_size_mib");

  openlog("xenguest",LOG_NDELAY,LOG_DAEMON);
  syslog(LOG_INFO|LOG_DAEMON,"Determined the following parameters from xenstore:");
  syslog(LOG_INFO|LOG_DAEMON,"vcpu/number:%d vcpu/weight:%d vcpu/cap:%d nx: %d viridian: %d apic: %d acpi: %d pae: %d acpi_s4: %d acpi_s3: %d mmio_size_mib: %ld",
                f->vcpus,f->vcpu_weight,f->vcpu_cap,f->nx,f->viridian,f->apic,f->acpi,f->pae,f->acpi_s4,f->acpi_s3,f->mmio_size_mib);
  for (n = 0; n < f->vcpus; n++){
	syslog(LOG_INFO|LOG_DAEMON,"vcpu/%d/affinity:%s", n, (f->vcpu_affinity[n])?f->vcpu_affinity[n]:"unset");
  }
  closelog();
  
}


static void failwith_oss_xc(xc_interface *xch, char *fct)
{
	char buf[80];
	const xc_error *error;

	error = xc_get_last_error(xch);
	if (error->code == XC_ERROR_NONE)
		snprintf(buf, 80, "%s: [%d] %s", fct, errno, strerror(errno));
	else
		snprintf(buf, 80, "%s: [%d] %s", fct, error->code, error->message);
	xc_clear_last_error(xch);
	caml_failwith(buf);
}

static int dispatch_suspend(void *arg)
{
	value * __suspend_closure;
	int domid = (int) arg;
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
	xc_interface *xch;

	xch = xc_interface_open(NULL, NULL, 0);
	if (xch == NULL)
		failwith_oss_xc(NULL, "xc_interface_open");
	return (value)xch;
}

CAMLprim value stub_xenguest_close(value xenguest_handle)
{
	CAMLparam1(xenguest_handle);
	xc_interface_close(_H(xenguest_handle));
	CAMLreturn(Val_unit);
}

extern struct xc_dom_image *xc_dom_allocate(xc_interface *xch, const char *cmdline, const char *features);

static void configure_vcpus(xc_interface *xch, int domid, struct flags f){
  struct xen_domctl_sched_credit sdom;
  int i, j, r, size, pcpus_supplied, min;
  xc_cpumap_t cpumap;

  size = xc_get_cpumap_size(xch) * 8; /* array is of uint8_t */

  for (i=0; i<f.vcpus; i++){
	if (f.vcpu_affinity[i]){ /* NULL means unset */
	  pcpus_supplied = strlen(f.vcpu_affinity[i]);
	  min = (pcpus_supplied < size)?pcpus_supplied:size;
	  cpumap = xc_cpumap_alloc(xch);
	  if (cpumap == NULL)
		failwith_oss_xc(xch, "xc_cpumap_alloc");

	  for (j=0; j<min; j++) {
		if (f.vcpu_affinity[i][j] == '1')
		  cpumap[j/8] |= 1 << (j&7);
	  }
	  r = xc_vcpu_setaffinity(xch, domid, i, cpumap);
	  free(cpumap);
	  if (r) {
		failwith_oss_xc(xch, "xc_vcpu_setaffinity");
      }
    }
  }

  r = xc_sched_credit_domain_get(xch, domid, &sdom);
  /* This should only happen when a different scheduler is set */
  if (r) {
	syslog(LOG_WARNING|LOG_DAEMON, "Failed to get credit scheduler parameters: scheduler not enabled?");
	return;
  }
  if (f.vcpu_weight != 0L) sdom.weight = f.vcpu_weight;
  if (f.vcpu_cap != 0L) sdom.cap = f.vcpu_cap;
  /* This shouldn't fail, if "get" above succeeds. This error is fatal
	 to highlight the need to investigate further. */
  r = xc_sched_credit_domain_set(xch, domid, &sdom);
  if (r)
    failwith_oss_xc(xch, "xc_sched_credit_domain_set");
}

CAMLprim value stub_xc_linux_build_native(value xc_handle, value domid,
                                          value mem_max_mib, value mem_start_mib,
                                          value image_name, value ramdisk_name,
                                          value cmdline, value features,
                                          value flags, value store_evtchn,
                                          value console_evtchn)
{
	CAMLparam5(xc_handle, domid, mem_max_mib, mem_start_mib, image_name);
	CAMLxparam5(ramdisk_name, cmdline, features, flags, store_evtchn);
	CAMLxparam1(console_evtchn);
	CAMLlocal1(result);

	unsigned long store_mfn;
	unsigned long console_mfn;
	int r;
	struct xc_dom_image *dom;
	char c_protocol[64];

	/* Copy the ocaml values into c-land before dropping the mutex */
	xc_interface *xch = _H(xc_handle);
	unsigned int c_mem_start_mib = Int_val(mem_start_mib);
	uint32_t c_domid = _D(domid);
	char *c_image_name = strdup(String_val(image_name));
	char *c_ramdisk_name = ramdisk_name == None_val ? NULL : strdup(String_val(Field(ramdisk_name, 0)));
	unsigned long c_flags = Int_val(flags);
	unsigned int c_store_evtchn = Int_val(store_evtchn);
	unsigned int c_console_evtchn = Int_val(console_evtchn);

	struct flags f;
	get_flags(&f,c_domid);

	xc_dom_loginit(xch);
	dom = xc_dom_allocate(xch, String_val(cmdline), String_val(features));
	if (!dom)
		failwith_oss_xc(xch, "xc_dom_allocate");

	configure_vcpus(xch, c_domid, f);

	caml_enter_blocking_section();
	r = xc_dom_linux_build(xch, dom, c_domid, c_mem_start_mib,
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
		failwith_oss_xc(xch, "xc_dom_linux_build");

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
	                                  argv[8], argv[9], argv[10]);
}

static int hvm_build_set_params(xc_interface *xch, int domid,
                                int store_evtchn, unsigned long *store_mfn,
                                int console_evtchn, unsigned long *console_mfn,
				struct flags f)
{
	struct hvm_info_table *va_hvm;
	uint8_t *va_map, sum;
	int i;

	va_map = xc_map_foreign_range(xch, domid,
			    XC_PAGE_SIZE, PROT_READ | PROT_WRITE,
			    HVM_INFO_PFN);
	if (va_map == NULL)
		return -1;

	va_hvm = (struct hvm_info_table *)(va_map + HVM_INFO_OFFSET);
	va_hvm->acpi_enabled = f.acpi;
	va_hvm->apic_mode = f.apic;
	va_hvm->nr_vcpus = f.vcpus;
	memset(va_hvm->vcpu_online, 0, sizeof(va_hvm->vcpu_online));
	for (i = 0; i < f.vcpus_current; i++)
		va_hvm->vcpu_online[i/8] |= 1 << (i % 8);
#if defined(HVM_INFO_TABLE_HAS_S4_ENABLED)
	va_hvm->s4_enabled = f.acpi_s4;
#endif
#if defined(HVM_INFO_TABLE_HAS_S3_ENABLED)
	va_hvm->s3_enabled = f.acpi_s3;
#endif
	va_hvm->checksum = 0;
	for (i = 0, sum = 0; i < va_hvm->length; i++)
		sum += ((uint8_t *) va_hvm)[i];
	va_hvm->checksum = -sum;
	munmap(va_map, XC_PAGE_SIZE);

	xc_get_hvm_param(xch, domid, HVM_PARAM_STORE_PFN, store_mfn);
	xc_set_hvm_param(xch, domid, HVM_PARAM_PAE_ENABLED, f.pae);
#ifdef HVM_PARAM_VIRIDIAN
	xc_set_hvm_param(xch, domid, HVM_PARAM_VIRIDIAN, f.viridian);
#endif
	xc_set_hvm_param(xch, domid, HVM_PARAM_STORE_EVTCHN, store_evtchn);
#ifndef XEN_UNSTABLE
	xc_set_hvm_param(xch, domid, HVM_PARAM_NX_ENABLED, f.nx);
  xc_get_hvm_param(xch, domid, HVM_PARAM_CONSOLE_PFN, console_mfn);
  xc_set_hvm_param(xch, domid, HVM_PARAM_CONSOLE_EVTCHN, console_evtchn);
#endif
	return 0;
}

CAMLprim value stub_xc_hvm_build_native(value xc_handle, value domid,
    value mem_max_mib, value mem_start_mib, value image_name, value store_evtchn, value console_evtchn)
{
	CAMLparam5(xc_handle, domid, mem_max_mib, mem_start_mib, image_name);
	CAMLxparam2(store_evtchn, console_evtchn);
	CAMLlocal1(result);

	char *image_name_c = strdup(String_val(image_name));
	char *error[256];
	xc_interface *xch;

	unsigned long store_mfn=0;
  unsigned long console_mfn=0;
	int r;
	struct flags f;
	/* The xenguest interface changed and was backported to XCP: */
#if defined(XENGUEST_HAS_HVM_BUILD_ARGS) || (__XEN_LATEST_INTERFACE_VERSION__ >= 0x00040200)
	struct xc_hvm_build_args args;
#endif
	get_flags(&f, _D(domid));

	xch = _H(xc_handle);
	configure_vcpus(xch, _D(domid), f);

#if defined(XENGUEST_HAS_HVM_BUILD_ARGS) || (__XEN_LATEST_INTERFACE_VERSION__ >= 0x00040200)
	args.mem_size = (uint64_t)Int_val(mem_max_mib) << 20;
	args.mem_target = (uint64_t)Int_val(mem_start_mib) << 20;
	args.mmio_size = f.mmio_size_mib << 20;
	args.image_file_name = image_name_c;
#endif

	caml_enter_blocking_section ();
#if defined(XENGUEST_HAS_HVM_BUILD_ARGS) || (__XEN_LATEST_INTERFACE_VERSION__ >= 0x00040200)
	r = xc_hvm_build(xch, _D(domid), &args);
#else
	r = xc_hvm_build_target_mem(xch, _D(domid),
				    Int_val(mem_max_mib),
				    Int_val(mem_start_mib),
				    image_name_c);
#endif
	caml_leave_blocking_section ();

	free(image_name_c);

	if (r)
		failwith_oss_xc(xch, "hvm_build");


	r = hvm_build_set_params(xch, _D(domid), Int_val(store_evtchn), &store_mfn, 
                           Int_val(console_evtchn), &console_mfn, f);
	if (r)
		failwith_oss_xc(xch, "hvm_build_params");

  result = caml_alloc_tuple(2);
  Store_field(result, 0, caml_copy_nativeint(store_mfn));
  Store_field(result, 1, caml_copy_nativeint(console_mfn));

	CAMLreturn(result);
}

CAMLprim value stub_xc_hvm_build_bytecode(value * argv, int argn)
{
	return stub_xc_hvm_build_native(argv[0], argv[1], argv[2], argv[3],
                                  argv[4], argv[5], argv[6]);
}


int switch_qemu_logdirty(int domid, unsigned enable, void *data)
{
  char *path = NULL;
  struct xs_handle *xsh = NULL;
  bool rc;

  pasprintf(&path, "/local/domain/0/device-model/%u/logdirty/cmd", domid);

  xsh = xs_daemon_open();
  if (xsh == NULL)
    errx(1, "Couldn't contact xenstore");

  if (enable)
    rc = xs_write(xsh, XBT_NULL, path, "enable", strlen("enable"));
  else
    rc = xs_write(xsh, XBT_NULL, path, "disable", strlen("disable"));

  xs_daemon_close(xsh);
  free(path);
  return rc ? 0 : 1;

}

static struct save_callbacks save_callbacks = {
	.suspend = dispatch_suspend,
  .postcopy = switch_qemu_logdirty,
        .checkpoint = NULL,
};

CAMLprim value stub_xc_domain_save(value handle, value fd, value domid,
                                   value max_iters, value max_factors,
                                   value flags, value hvm)
{
	CAMLparam5(handle, fd, domid, max_iters, max_factors);
	CAMLxparam2(flags, hvm);
	struct save_callbacks callbacks;

	uint32_t c_flags;
  uint32_t c_domid;
	int r;

	c_flags = caml_convert_flag_list(flags, suspend_flag_list);
  c_domid = _D(domid);

	memset(&callbacks, 0, sizeof(callbacks));
  callbacks.data = c_domid;
	callbacks.suspend = dispatch_suspend;
	callbacks.switch_qemu_logdirty = switch_qemu_logdirty;

	caml_enter_blocking_section();
	r = xc_domain_save(_H(handle), Int_val(fd), c_domid,
	                   Int_val(max_iters), Int_val(max_factors),
	                   c_flags, &callbacks, Bool_val(hvm));
	caml_leave_blocking_section();
	if (r)
		failwith_oss_xc(_H(handle), "xc_domain_save");

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
		failwith_oss_xc(_H(handle), "xc_domain_resume");
	CAMLreturn(Val_unit);
}


CAMLprim value stub_xc_domain_restore(value handle, value fd, value domid,
                                      value store_evtchn, value console_evtchn,
                                      value hvm)
{
	CAMLparam5(handle, fd, domid, store_evtchn, console_evtchn);
	CAMLxparam1(hvm);
	CAMLlocal1(result);
	unsigned long store_mfn, console_mfn;
	unsigned int c_store_evtchn, c_console_evtchn;
	int r;

	struct flags f;
	get_flags(&f,_D(domid));

	c_store_evtchn = Int_val(store_evtchn);
	c_console_evtchn = Int_val(console_evtchn);

#ifdef HVM_PARAM_VIRIDIAN
	xc_set_hvm_param(_H(handle), _D(domid), HVM_PARAM_VIRIDIAN, f.viridian);	
#endif
	configure_vcpus(_H(handle), _D(domid), f);

	caml_enter_blocking_section();
	r = xc_domain_restore(_H(handle), Int_val(fd), _D(domid),
	                      c_store_evtchn, &store_mfn,
	                      c_console_evtchn, &console_mfn,
			      Bool_val(hvm), f.pae, 0 /*superpages*/);
	caml_leave_blocking_section();
	if (r)
		failwith_oss_xc(_H(handle), "xc_domain_restore");

	result = caml_alloc_tuple(2);
	Store_field(result, 0, caml_copy_nativeint(store_mfn));
	Store_field(result, 1, caml_copy_nativeint(console_mfn));

	CAMLreturn(result);
}

CAMLprim value stub_xc_domain_restore_bytecode(value * argv, int argn)
{
	return stub_xc_domain_restore(argv[0], argv[1], argv[2], argv[3],
	                              argv[4], argv[5]);
}

CAMLprim value stub_xc_domain_dumpcore(value handle, value domid, value file)
{
	CAMLparam3(handle, domid, file);
	int r;

	r = xc_domain_dumpcore(_H(handle), _D(domid), String_val(file));
	if (r)
		failwith_oss_xc(_H(handle), "xc_domain_dumpcore");
	CAMLreturn(Val_unit);
}
