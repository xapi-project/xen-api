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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <syslog.h>
#include <stdbool.h>
#include <fcntl.h>
#include <sys/wait.h>

#include "../lib/close_from.h"

static inline size_t quoted_length(const char c)
{
	return c == '\\' ? 2 :
		(c >= ' ' && c < 0x7f) ? 1 :
		4;
}

static const char hex[] = "0123456789ABCDEF";

static inline void write_quoted(char *const p, const char c)
{
	if (c == '\\') {
		p[0] = p[1] = c;
	} else if (c >= ' ' && c < 0x7f) {
		p[0] = c;
	} else {
		p[0] = '\\';
		p[1] = 'x';
		p[2] = hex[(c>>4)&0xf];
		p[3] = hex[c&0xf];
	}
}

static char quoted_buf[64000];
static const char *key = NULL;
static int child_pid;

static void syslog_line(const char *line)
{
	syslog(LOG_DAEMON|LOG_INFO, "%s[%d]: %s", key, child_pid, line);
}

static bool forward_to_syslog(int fd)
{
	FILE *f = fdopen(fd, "r");
	char *dest = quoted_buf;
	char *const dest_end = quoted_buf + sizeof(quoted_buf) - sizeof(" ...") - 1;
	bool overflowed = false;
	while (true) {
		int ch = getc_unlocked(f);

		if (!overflowed && dest != quoted_buf && (ch == '\n' || ch == EOF)) {
			*dest = 0;
			syslog_line(quoted_buf);
		}

		if (ch == EOF)
			return !!feof(f);

		if (ch == '\n') {
			overflowed = false;
			dest = quoted_buf;
			continue;
		}

		if (overflowed)
			continue;

		const size_t quoted_len = quoted_length(ch);
		if (dest + quoted_len >= dest_end) {
			strcpy(dest, " ...");
			syslog_line(quoted_buf);
			overflowed = true;
			continue;
		}
		write_quoted(dest, ch);
		dest += quoted_len;
	}
}

// first argument file descriptor for read pipe
// second option key
// others just arguments
int main(int argc, char **argv)
{
	int fds[2];
	int version;
	int status;
	int redirect_stderr_to_stdout;
	pid_t pid;

	if (argc < 4)
		return 125;

	// first argument, <version>:<redirect>:<file descriptor>
	if (sscanf(argv[1], "%d:%d:%d", &version, &redirect_stderr_to_stdout, &fds[0]) != 3)
		return 125;
	if (version != 1)
		return 125;

	// second argument, key
	key = argv[2];

	// others are the arguments
	argv += 3;

	fds[1] = -1;
	if (fds[0] < 0) {
		if (pipe(fds) < 0)
			return 125;
	}

	child_pid = (int) getpid();

	pid = fork();
	if (pid < 0)
		return 125;

	if (pid == 0) {
		// child
		if (fds[1] >= 0)
			close(fds[1]);

		close(0);
		close(1);
		close(2);
		open("/dev/null", O_RDONLY);
		open("/dev/null", O_WRONLY);
		open("/dev/null", O_WRONLY);
		if (fds[0] != 3) {
			dup2(fds[0], 3);
			fds[0] = 3;
		}
		close_fds_from(4);

		pid = fork();
		if (pid < 0)
			return 125;
		if (pid > 0)
			// parent
			return 0;

		openlog("forkexecd", 0, LOG_DAEMON);
		forward_to_syslog(fds[0]);
		return 0;
	}

	// parent
	wait(&status);
	if (!WIFEXITED(status) || WEXITSTATUS(status) != 0)
		return 125;

	close(fds[0]);
	if (fds[1] >= 0)
		dup2(fds[1], 1);
	if (fds[1] >= 0 && redirect_stderr_to_stdout)
		dup2(fds[1], 2);
	if (fds[1] >= 0)
		close(fds[1]);
	execv(argv[0], argv);

	return errno == ENOENT ? 127 : 126;
}
