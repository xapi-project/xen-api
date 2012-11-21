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

#ifndef FAKE_MARSHALL_H
#define FAKE_MARSHALL_H

#include <string.h>
#include <stdarg.h>
#include <stdio.h>

static int marshall_command(int handle, const char *fmt, ...)
{
	va_list ap;
	char buf[256];
	int ret;

	va_start(ap, fmt);
	ret = vsnprintf(buf, 256, fmt, ap);
	va_end(ap);

	if (ret > 255)
		return -1;
	if(write(handle, buf, ret) < ret)
		return -1;
	return 0;
}

static char ** string_split(const char *s, const char c)
{
	int found, i;
	char **ret;
	char *end;

	for (found = i = 0; s[i]; i++)
		if (s[i] == c) found++;
	ret = calloc(found + 2, sizeof(char *));
	if (!ret)
		return NULL;
	for (i = 0; i < found + 1; i++) {
		end = strchr(s, c);
		if (!end) {
			ret[i] = strdup(s);
			break;
		}

		ret[i] = strndup(s, end - s);
		s = end + 1;
	}
	return ret;
}

static void string_split_free(char **ss)
{
	int i;
	for (i = 0; ss[i] != NULL; i++)
		free(ss[i]);
	free(ss);
}

static int get_line(int handle, char *buf)
{
	int offset = 0;
	memset(buf, '\0', 256);
	while (1) {
		int r = read(handle, buf + offset, 1);
		if (r == -1 || r == 0) break;
		if (buf[offset] == '\n')
			break;
		else {
			if (offset >= 255)
				break;
			else
				offset += 1;
		}
	}
	return offset;
}


static int64_t unmarshall_int64(int handle)
{
	char buf[256];
	int negative;
	int64_t ret;

	ret = get_line(handle, buf);
	if (ret == 0)
		return -EBADF;
	negative = (buf[0] == '-');
	ret = atoll(buf + (negative ? 1 : 0));
	return (negative ? -ret : ret);
}

static int unmarshall_int(int handle)
{
	return (uint32_t) unmarshall_int64(handle);
}

static int parse_uuid(char *s, int uuid[])
{
	#define UUID_FMT "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x"
	sscanf(s, UUID_FMT, uuid + 0, uuid + 1, uuid + 2, uuid + 3,
                            uuid + 4, uuid + 5, uuid + 6, uuid + 7,
	                    uuid + 8, uuid + 9, uuid + 10, uuid + 11,
	                    uuid + 12, uuid + 13, uuid + 14, uuid + 15);
	return 0;
}

static char **unmarshall_multiple(int handle)
{
	char buf[256];
	int ret;

	ret = get_line(handle, buf);
	if (ret == 0)
		return NULL;
	return string_split(buf, ',');
}

static int unmarshall_return(int handle)
{
	int ret;
	ret = unmarshall_int(handle);
	if (ret < 0)
		errno = -ret;
	return ret;
}

#endif /* !FAKE_MARSHALL_H */
