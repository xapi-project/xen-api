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

#include "close_from.h"

#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/resource.h>

#ifdef __linux__
#include <sys/syscall.h>
#endif

// try to use close_range on Linux even if not defined by headers
#if defined(__linux__) && !defined(SYS_close_range)
#  if defined(__alpha__)
#    define SYS_close_range 546
#  elif defined(__amd64__) || defined(__x86_64__) || defined(__arm__) || \
        defined(__aarch64__) || defined(__hppa__) || defined(__i386__) || \
        defined(__ia64__) || defined(__m68k__) || defined(__mips__) || \
        defined(__powerpc__) || defined(__powerpc64__) || defined(__sparc__) || \
        defined(__s390x__)
#    define SYS_close_range 436
#  endif
#endif

bool
close_fds_from(int fd_from)
{
    // first method, use close_range
#if (defined(__linux__) && defined(SYS_close_range)) \
    || (defined(__FreeBSD__) && defined(CLOSE_RANGE_CLOEXEC))
    static bool close_range_supported = true;
    if (close_range_supported) {
#if defined(__linux__)
        if (syscall(SYS_close_range, fd_from, ~0U, 0) == 0)
#else
        if (close_range(fd_from, ~0U, 0) == 0)
#endif
            return true;

        if (errno == ENOSYS)
            close_range_supported = false;
    }
#endif

    // second method, read fds list from /proc
    DIR *dir = opendir("/proc/self/fd");
    if (dir) {
        const int dir_fd = dirfd(dir);
        struct dirent *ent;
        while ((ent = readdir(dir)) != NULL) {
            char *end = NULL;
            unsigned long fd = strtoul(ent->d_name, &end, 10);
            if (end == NULL || *end)
                continue;
            if (fd >= fd_from && fd != dir_fd)
                close(fd);
        }
        closedir(dir);
        return true;
    }

    // third method, use just a loop
    struct rlimit limit;
    if (getrlimit(RLIMIT_NOFILE, &limit) < 0)
        return false;
    for (int fd = fd_from; fd < limit.rlim_cur; ++ fd)
        close(fd);

    return true;
}
