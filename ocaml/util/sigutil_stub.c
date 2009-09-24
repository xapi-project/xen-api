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

#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>

static void fpe_handler(int signum, siginfo_t *info, void *ptr)
{
	FILE *file;
	if (signum != SIGFPE || info == NULL)
		return;
	file = fopen("/tmp/fpe_dump", "w+");
	if (file == NULL)
		return;

	fprintf(file, "si_signo: %d\n", info->si_signo);
	fprintf(file, "si_errno: %d\n", info->si_errno);
	fprintf(file, "si_code: %d\n", info->si_code);
	fprintf(file, "si_pid: %d\n", info->si_pid);
	fprintf(file, "si_status: %d\n", info->si_status);
	fprintf(file, "si_value: %d\n", info->si_value);
	fprintf(file, "si_int: %lx\n", info->si_int);
	fprintf(file, "si_ptr: %lx\n", (unsigned long) info->si_ptr);
	fprintf(file, "si_addr: %lx\n", (unsigned long) info->si_addr);
	fprintf(file, "uarg: %lx\n", (unsigned long) ptr);
	fclose(file);

	kill(0, SIGFPE);
}

static void segv_handler(int signum, siginfo_t *info, void *ptr)
{
	FILE *file;
	if (signum != SIGSEGV || info == NULL)
		return;
	file = fopen("/tmp/segv_dump", "w+");
	if (file == NULL)
		return;

	fprintf(file, "si_signo: %d\n", info->si_signo);
	fprintf(file, "si_errno: %d\n", info->si_errno);
	fprintf(file, "si_code: %d\n", info->si_code);
	fprintf(file, "si_pid: %d\n", info->si_pid);
	fprintf(file, "si_status: %d\n", info->si_status);
	fprintf(file, "si_value: %d\n", info->si_value);
	fprintf(file, "si_int: %lx\n", info->si_int);
	fprintf(file, "si_ptr: %lx\n", (unsigned long) info->si_ptr);
	fprintf(file, "si_addr: %lx\n", (unsigned long) info->si_addr);
	fprintf(file, "uarg: %lx\n", (unsigned long) ptr);
	fclose(file);

	kill(0, SIGSEGV);
}

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

CAMLprim value stub_install_fpe_handler(value unit)
{
	CAMLparam1(unit);
	struct sigaction act;

	act.sa_sigaction = fpe_handler;
	act.sa_flags = SA_SIGINFO | SA_RESETHAND;

	sigaction(SIGFPE, &act, NULL);

	CAMLreturn(Val_unit);
}

CAMLprim value stub_install_segv_handler(value unit)
{
	CAMLparam1(unit);
	struct sigaction act;
	act.sa_sigaction = segv_handler;
	act.sa_flags = SA_SIGINFO | SA_RESETHAND;

	sigaction(SIGSEGV, &act, NULL);

	CAMLreturn(Val_unit);
}
