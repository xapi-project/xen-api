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

#include "logs.h"

#if FORKEXECD_DEBUG_LOGS

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>

#include <caml/fail.h>

#define FILE_SIZE (32 * 1024)

struct priv_mapped_logs {
    uint32_t size;

    // Flags, we use characters instead of binary so
    // easily see them easily with different tools.
    char flags[4];
    char filename[64];
    pid_t pid;
    int num;
};

// flags order
enum { SUCCESS, FAILURE };

mapped_logs mapped_logs_open(void)
{
    static int last_num = 0;

    // create a mapped file with a given size, will write header as structure
    // and update using memory
    mkdir("/tmp/fe_repl", 0777);

    char tmpl[] = "/tmp/fe_repl/logXXXXXX";
    int fd = mkstemp(tmpl);
    if (!fd)
        caml_raise_out_of_memory();

    if (ftruncate(fd, FILE_SIZE) < 0) {
        close(fd);
        caml_raise_out_of_memory();
    }

    priv_mapped_logs *l = mmap(NULL, FILE_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if (l == MAP_FAILED) {
        close(fd);
        caml_raise_out_of_memory();
    }
    close(fd);

    l->size = sizeof(*l);
    memcpy(l->flags, "____", 4);
    strncpy(l->filename, tmpl, sizeof(l->filename));
    l->pid = getpid();
    l->num = ++last_num;

    return (mapped_logs){l};
}

#define RANGE \
    char *start = (char*) logs.priv + sizeof(priv_mapped_logs); \
    char *const end = (char*) logs.priv + FILE_SIZE

void mapped_logs_close(mapped_logs logs)
{
    if (!logs.priv)
        return;
    RANGE;
    bool written = false;
    bool success = logs.priv->flags[FAILURE] == '_' && logs.priv->flags[SUCCESS] != '_';
    if (!success) {
        FILE *f = fopen("/tmp/fe_repl/all_logs", "a");
        if (f) {
            end[-1] = 0;
            size_t len = strlen(start);
            written = (fwrite(start, 1, len, f) == len);
            fclose(f);
        }
    }
    if (written || success)
        unlink(logs.priv->filename);
    munmap(logs.priv, FILE_SIZE);
}

void mapped_logs_failure(mapped_logs logs)
{
    if (!logs.priv)
        return;
    logs.priv->flags[FAILURE] = 'F';
}

void mapped_logs_success(mapped_logs logs)
{
    if (!logs.priv)
        return;
    logs.priv->flags[SUCCESS] = 'S';
}

void mapped_logs_add(mapped_logs logs, const char *fmt, ...)
{
    if (!logs.priv)
        return;
    int save_errno = errno;
    RANGE;
    start += strlen(start);
    if (start >= end -1) {
        errno = save_errno;
        return; // no more space
    }
    size_t len = end - start;
    int l = snprintf(start, len, "%d:%d ", (int) logs.priv->pid, logs.priv->num);
    if (l >= len) {
        errno = save_errno;
        return;
    }
    start += l;
    len -= l;
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(start, len, fmt, ap);
    va_end(ap);

    errno = save_errno;
}
#endif

// vim: expandtab ts=4 sw=4 sts=4:
