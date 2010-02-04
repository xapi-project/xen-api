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

#ifdef WITH_INJECTION_CAPABILITY
#include "../fake/using.h"
#include "../fake/marshall.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#define HYPCALLcmd "hypcall"

static int fake_interface_open(void)
{
	struct sockaddr_un remote;
	char *s;
	int fd, len;

	s = getenv("XIU");
	if (!s)
		return -1;
	snprintf(remote.sun_path, 256, "%s-xc", s);
	remote.sun_family = AF_UNIX;
	len = strlen(remote.sun_path) + sizeof(remote.sun_family);

	fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (fd == -1)
		return -1;
	if (connect(fd, (struct sockaddr *)&remote, len) != 0)
		return -1;

	return fd;
}

static int fake_interface_close(int handle)
{
	return close(handle);
}

static int fake_interface_ioctl(int handle, int cmd, void *arg)
{
	return 0;
}

static void * fake_interface_mmap(void *start, size_t length, int prot, int flags,
                                  int fd, off_t offset)
{
	return mmap(start, length, prot, MAP_PRIVATE|MAP_ANONYMOUS, -1, offset);
}

#define DOMAINHANDLE "%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x-%02x"

static int fake_xen_domctl(int handle, struct xen_domctl *domctl)
{
	#define DOMCTLcmd "domctl"
	switch (domctl->cmd) {
	case XEN_DOMCTL_pausedomain:
	case XEN_DOMCTL_unpausedomain:
	case XEN_DOMCTL_resumedomain:
	case XEN_DOMCTL_destroydomain:
		marshall_command(handle, "%s,%d,%d\n", DOMCTLcmd, domctl->cmd, domctl->domain);
		return unmarshall_return(handle);
	case XEN_DOMCTL_createdomain: /* W ssidref */
		marshall_command(handle, "%s,%d,%d,%d," DOMAINHANDLE "\n", DOMCTLcmd,
		                 domctl->cmd,
		                 (domctl->u.createdomain.flags|XEN_DOMCTL_CDF_hvm_guest)?1:0,
		                 (domctl->u.createdomain.flags|XEN_DOMCTL_CDF_hap)?1:0,
		                 domctl->u.createdomain.handle[0],
		                 domctl->u.createdomain.handle[1],
		                 domctl->u.createdomain.handle[2],
		                 domctl->u.createdomain.handle[3],
		                 domctl->u.createdomain.handle[4],
		                 domctl->u.createdomain.handle[5],
		                 domctl->u.createdomain.handle[6],
		                 domctl->u.createdomain.handle[7],
		                 domctl->u.createdomain.handle[8],
		                 domctl->u.createdomain.handle[9],
		                 domctl->u.createdomain.handle[10],
		                 domctl->u.createdomain.handle[11],
		                 domctl->u.createdomain.handle[12],
		                 domctl->u.createdomain.handle[13],
		                 domctl->u.createdomain.handle[14],
		                 domctl->u.createdomain.handle[15]);
		domctl->domain = unmarshall_int(handle);
		return unmarshall_return(handle);
	case XEN_DOMCTL_max_mem: /* W domid, maxmem */
		marshall_command(handle, "%s,%d,%d,%d\n", DOMCTLcmd,
		                 domctl->cmd,
		                 domctl->domain, domctl->u.max_mem.max_memkb);
		return unmarshall_return(handle);
	case XEN_DOMCTL_settimeoffset: /* W domid, time */
		marshall_command(handle, "%s,%d,%d,%d\n", DOMCTLcmd,
		                 domctl->cmd,
		                 domctl->domain,
		                 domctl->u.settimeoffset.time_offset_seconds);
		return unmarshall_return(handle);
#ifdef XEN_DOMCTL_setvmxassist
	case XEN_DOMCTL_setvmxassist: /* W domid, use_vmxassist */
		marshall_command(handle, "%s,%d,%d,%d\n", DOMCTLcmd,
		                 domctl->cmd,
		                 domctl->domain,
		                 domctl->u.setvmxassist.use_vmxassist);
		return unmarshall_return(handle);
#endif
	case XEN_DOMCTL_max_vcpus: /* W domid, max */
		marshall_command(handle, "%s,%d,%d,%d\n", DOMCTLcmd,
		                 domctl->cmd,
		                 domctl->domain,
		                 domctl->u.max_vcpus.max);
		return unmarshall_return(handle);
	case XEN_DOMCTL_setdomainhandle: { /* domid, handle */
		marshall_command(handle, "%s,%d,%d," DOMAINHANDLE "\n", DOMCTLcmd,
		                 domctl->cmd,
		                 domctl->domain,
		                 domctl->u.setdomainhandle.handle[0],
		                 domctl->u.setdomainhandle.handle[1],
		                 domctl->u.setdomainhandle.handle[2],
		                 domctl->u.setdomainhandle.handle[3],
		                 domctl->u.setdomainhandle.handle[4],
		                 domctl->u.setdomainhandle.handle[5],
		                 domctl->u.setdomainhandle.handle[6],
		                 domctl->u.setdomainhandle.handle[7],
		                 domctl->u.setdomainhandle.handle[8],
		                 domctl->u.setdomainhandle.handle[9],
		                 domctl->u.setdomainhandle.handle[10],
		                 domctl->u.setdomainhandle.handle[11],
		                 domctl->u.setdomainhandle.handle[12],
		                 domctl->u.setdomainhandle.handle[13],
		                 domctl->u.setdomainhandle.handle[14],
		                 domctl->u.setdomainhandle.handle[15]);
		return unmarshall_return(handle);
		}
	/* just returning success : might need init */
	case XEN_DOMCTL_getvcpucontext:
	case XEN_DOMCTL_getvcpuaffinity:
		return 0;
	/* just returning success */
	case XEN_DOMCTL_scheduler_op:
		return 0;
	case XEN_DOMCTL_shadow_op:
		/* Only handle set/get allocation at the moment */
		marshall_command(handle, "%s,%d,%d,%d,%d,%d\n", DOMCTLcmd,
				 domctl->cmd,
				 domctl->domain,
				 domctl->u.shadow_op.op,
				 domctl->u.shadow_op.mode,
				 domctl->u.shadow_op.mb);
		if(domctl->u.shadow_op.op == XEN_DOMCTL_SHADOW_OP_GET_ALLOCATION) 
			domctl->u.shadow_op.mb = unmarshall_int(handle);

		return unmarshall_return(handle);

	/* just return success */
	case XEN_DOMCTL_ioport_permission: 
	case XEN_DOMCTL_irq_permission:
	case XEN_DOMCTL_iomem_permission:
	case XEN_DOMCTL_setvcpuaffinity:
	case XEN_DOMCTL_setvcpucontext:
	case XEN_DOMCTL_getmemlist:
	case XEN_DOMCTL_getvcpuinfo:
#ifdef XEN_DOMCTL_get_runstate_info
	case XEN_DOMCTL_get_runstate_info:
#endif
	case XEN_DOMCTL_set_machine_address_size:
#ifdef XEN_DOMCTL_suppress_spurious_page_faults
	case XEN_DOMCTL_suppress_spurious_page_faults:
#endif
		return 0;
	default:
		return -EINVAL;
	}
}

static int fake_xen_sysctl(int handle, struct xen_sysctl *sysctl)
{
	#define SYSCTLcmd "sysctl"
	switch (sysctl->cmd) {
	case XEN_SYSCTL_getdomaininfolist: {
		xc_domaininfo_t *info; int num, i;

		get_xen_guest_handle(info, sysctl->u.getdomaininfolist.buffer);

		marshall_command(handle, "%s,%d,%d,%d\n", SYSCTLcmd, sysctl->cmd,
		                 sysctl->u.getdomaininfolist.first_domain,
		                 sysctl->u.getdomaininfolist.max_domains);
		num = unmarshall_int(handle);
		for (i = 0; i < num; i++) {
			int uuid[16], j, flags;
			char **ret;
			
			ret = unmarshall_multiple(handle);
			if (!ret)
				return -EBADF;

			/* domid,uuid,flags */
			info->domain = atoi(ret[0]);

			parse_uuid(ret[1], uuid);
			for (j = 0; j < 16; j++)
				info->handle[j] = uuid[j] & 0xff;

			flags = atoi(ret[2]);
			info->flags = 0;
			if (flags & 0x1) info->flags |= XEN_DOMINF_dying;
			if (flags & 0x2) info->flags |= XEN_DOMINF_shutdown;
			if (flags & 0x4) info->flags |= XEN_DOMINF_paused;
			if (flags & 0x8) info->flags |= XEN_DOMINF_blocked;
			if (flags & 0x10) info->flags |= XEN_DOMINF_running;
			if (flags & 0x20) info->flags |= XEN_DOMINF_hvm_guest;
			info->flags |= ((flags >> 8) & 0xff) << XEN_DOMINF_shutdownshift;

			info->nr_online_vcpus = atoi(ret[3]);
			info->max_vcpu_id = atoi(ret[4]);

			info->tot_pages = atoi(ret[5]);
			info->max_pages = atoi(ret[6]);
			info->shared_info_frame = atoi(ret[7]);
			info->cpu_time = atoi(ret[8]);
			info->ssidref = atoi(ret[9]);

			string_split_free(ret);
			info++;

		}
		sysctl->u.getdomaininfolist.num_domains = num;
		return unmarshall_return(handle);
		}
	case XEN_SYSCTL_readconsole:
	case XEN_SYSCTL_debug_keys:
		return 0;
	case XEN_SYSCTL_physinfo: {
		char **ret;
		int sockets_per_node;

		marshall_command(handle, "%s,%d\n", SYSCTLcmd, sysctl->cmd);
		ret = unmarshall_multiple(handle);
		if (!ret) return -EBADF;

		sockets_per_node = atoi(ret[2]);

		sysctl->u.physinfo.threads_per_core = atoi(ret[0]);
		sysctl->u.physinfo.cores_per_socket = atoi(ret[1]);
#if XEN_SYSCTL_INTERFACE_VERSION < 6
		sysctl->u.physinfo.sockets_per_node = sockets_per_node;
#endif
		sysctl->u.physinfo.nr_nodes = atoi(ret[3]);
#if XEN_SYSCTL_INTERFACE_VERSION >= 6
		sysctl->u.physinfo.nr_cpus =
			sysctl->u.physinfo.threads_per_core *
			sysctl->u.physinfo.cores_per_socket *
			sockets_per_node *
			sysctl->u.physinfo.nr_nodes;
#endif
		sysctl->u.physinfo.cpu_khz = atoi(ret[4]);
		sysctl->u.physinfo.total_pages = atoi(ret[5]);
		sysctl->u.physinfo.free_pages = atoi(ret[6]);
		sysctl->u.physinfo.scrub_pages = 0;

		string_split_free(ret);
		return unmarshall_return(handle);
		}
	case XEN_SYSCTL_getcpuinfo: {
		uint64_t *info;
		int num, i;

		get_xen_guest_handle(info, sysctl->u.getcpuinfo.info);
		marshall_command(handle, "%s,%d,%d\n", SYSCTLcmd, sysctl->cmd, sysctl->u.getcpuinfo.max_cpus);
		num = unmarshall_int(handle);
		for (i = 0; i < num; i++) {
			info[i] = unmarshall_int64(handle);
		}
		return unmarshall_return(handle);
		}
	case XEN_SYSCTL_sched_id:
		return 0;
	default:
		return -EINVAL;
	}
	return 0;
}

static int fake_xen_eventchn(int handle, unsigned long cmd, unsigned long arg)
{
	switch (cmd) {
	case EVTCHNOP_alloc_unbound:
	case EVTCHNOP_reset:
		return 0;
	default:
		return -EINVAL;
	}
	return 0;
}

static int fake_xen_memoryop(int handle, unsigned long cmd, struct xen_memory_reservation *reservation)
{
	switch (cmd) {
	case XENMEM_set_memory_map:
	case XENMEM_increase_reservation:
	case XENMEM_decrease_reservation:
	case XENMEM_populate_physmap:
		return 0;
	default:
		return -EINVAL;
	}
	return 0;
}

static int fake_xen_hvmop(int handle, unsigned long cmd, unsigned long arg)
{
	switch (cmd) {
	case HVMOP_get_param:
		return 0;
	default:
		return -EINVAL;
	}
	return 0;
}

static int fake_xen_schedop(int handle, unsigned long cmd, sched_remote_shutdown_t *arg)
{
	switch (cmd) {
	case SCHEDOP_remote_shutdown:
		marshall_command(handle, "%s,%d,%d,%d\n", HYPCALLcmd,
						 1, 
		                 arg->domain_id,
						 arg->reason);
		return unmarshall_return(handle);
	default:
		return -EINVAL;
	}
	return 0;
}

static int fake_xen_versionop(int handle, unsigned long cmd, void * arg)
{
	switch (cmd) {
	case XENVER_extraversion:
		memset(arg, '\0', sizeof(xen_extraversion_t));
		memcpy(arg, ".0", 2);
		return 0;
	case XENVER_compile_info:
		memset(arg, '\0', sizeof(xen_compile_info_t));
		return 0;
	case XENVER_capabilities:
		memset(arg, '\0', sizeof(xen_capabilities_info_t));
		#define CAPA "xen-3.0-x86_64 xen-3.0-x86_32p hvm-3.0-x86_32 hvm-3.0-x86_32p hvm-3.0-x86_64"
		memcpy(arg, CAPA, strlen(CAPA));
		#undef CAPA
		return 0;
	case XENVER_changeset:
		memset(arg, '\0', sizeof(xen_changeset_info_t));
		return 0;
	case XENVER_platform_parameters:
		memset(arg, '\0', sizeof(xen_platform_parameters_t));
		return 0;
	case XENVER_version:
		return (((3 << 16) & 0xffff0000) + (1 & 0xffff)); /* 3.1 */
	default:
		return -EINVAL;
	}
	return 0;
}

static int fake_xen_hypercall(int handle, privcmd_hypercall_t *hypercall)
{
	if (!hypercall) return -EINVAL;
	switch (hypercall->op) {
	case __HYPERVISOR_domctl:
		return fake_xen_domctl(handle, (struct xen_domctl *) hypercall->arg[0]);
	case __HYPERVISOR_sysctl:
		return fake_xen_sysctl(handle, (struct xen_sysctl *) hypercall->arg[0]);
	case __HYPERVISOR_event_channel_op:
		return fake_xen_eventchn(handle, hypercall->arg[0], hypercall->arg[1]);
	case __HYPERVISOR_memory_op:
		return fake_xen_memoryop(handle, hypercall->arg[0], (struct xen_memory_reservation *) hypercall->arg[2]);
	case __HYPERVISOR_hvm_op:
		return fake_xen_hvmop(handle, hypercall->arg[0], hypercall->arg[2]);
	case __HYPERVISOR_sched_op:
		return fake_xen_schedop(handle, hypercall->arg[0], (sched_remote_shutdown_t *) hypercall->arg[1]);
	case __HYPERVISOR_xen_version:
		return fake_xen_versionop(handle, hypercall->arg[0], (void *) hypercall->arg[1]);
	default:
		return -EINVAL;
	}
}

#define pre_interface_open()     if (using_injection()) return fake_interface_open();
#define pre_interface_close(h)   if (using_injection()) return fake_interface_close(h);
#define pre_xen_hypercall(h, p)  if (using_injection()) return fake_xen_hypercall(h, p);
#define pre_ioctl(h, c, a)       if (using_injection()) return fake_interface_ioctl(h, c, a);
#define pre_mmap(s,l,p,f,d,o)    if (using_injection()) return fake_interface_mmap(s,l,p,f,d,o);
#else
#define pre_interface_open()     do {} while(0);
#define pre_interface_close(h)   do {} while(0);
#define pre_xen_hypercall(h, p)  do {} while(0);
#define pre_ioctl(h, c, a)       do {} while(0);
#define pre_mmap(s,l,p,f,d,o)    do {} while(0);
#endif
