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

#include "syslog.h"

#include <stdio.h>
#include <string.h>
#include <syslog.h>

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

static void syslog_line(const char *line, const char *key, int child_pid)
{
	syslog(LOG_DAEMON|LOG_INFO, "%s[%d]: %s", key, child_pid, line);
}

// Quote and forward every line from "fd" to the syslog.
// "fd" will be closed.
bool forward_to_syslog(int fd, const char *key, int child_pid)
{
#define syslog_line(line) syslog_line(line, key, child_pid)
	FILE *f = fdopen(fd, "r");
	char quoted_buf[64000];
	char *dest = quoted_buf;
	char *const dest_end = quoted_buf + sizeof(quoted_buf) - sizeof(" ...") - 1;
	bool overflowed = false;
	while (true) {
		int ch = getc_unlocked(f);

		if (!overflowed && dest != quoted_buf && (ch == '\n' || ch == EOF)) {
			*dest = 0;
			syslog_line(quoted_buf);
		}

		if (ch == EOF) {
			bool res = !!feof(f);
			fclose(f);
			return res;
		}

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
