/*
    Copyright (C) 2005 Christian Limpach

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

/*
 * Copied from xen-unstable.hg:tools/xcutils/xc_save.c
 * Covered by the same copyright as that file (GPLv2)
 * Modifications (c) Citrix Systems Inc
 */

#include <err.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <grp.h>

#include <xs.h>
#include <xenctrl.h>
#include <xenguest.h>

static char *qemu_active_path;
static char *qemu_next_active_path;
static struct xs_handle *xs;


/* Get qemu to change buffers. */
void qemu_flip_buffer(int domid, int next_active)
{
    char digit = '0' + next_active;
    unsigned int len;
    char *active_str, **watch;
    struct timeval tv;
    fd_set fdset;
    int rc;
    char *path, *p;

    if (xs == NULL) {
        if ((xs = xs_daemon_open()) == NULL)
            errx(1, "Couldn't contact xenstore");
        if (!(path = xs_get_domain_path(xs, domid)))
            errx(1, "can't get domain path in store");
        if (!(path = realloc(path, strlen(path)
                             + strlen("/logdirty/next-active") + 1)))
            errx(1, "no memory for constructing xenstore path");
        strcat(path, "/logdirty/");
        p = path + strlen(path);
        
        /* Watch for qemu's indication of the active buffer, and request it
         * to start writing to buffer 0 */
        strcpy(p, "active");
        if (!xs_watch(xs, path, "qemu-active-buffer"))
            errx(1, "can't set watch in store (%s)\n", path);
        if (!(qemu_active_path = strdup(path)))
            errx(1, "no memory for copying xenstore path");
        
        strcpy(p, "next-active");
        if (!(qemu_next_active_path = strdup(path)))
            errx(1, "no memory for copying xenstore path");
        
        free(path);
    }

    /* Tell qemu that we want it to start writing log-dirty bits to the
     * other buffer */
    if (!xs_write(xs, XBT_NULL, qemu_next_active_path, &digit, 1)) {
        errx(1, "can't write next-active to store path (%s)\n",
              qemu_next_active_path);
        exit(1);
    }

    /* Wait a while for qemu to signal that it has switched to the new
     * active buffer */
 read_again:
    tv.tv_sec = 60;
    tv.tv_usec = 0;
    FD_ZERO(&fdset);
    FD_SET(xs_fileno(xs), &fdset);
    rc = select(xs_fileno(xs) + 1, &fdset, NULL, NULL, &tv);
    if (rc == 0)
        errx(1, "timed out waiting for qemu to switch buffers\n");
    else if (rc < 0) {
        if (errno == EINTR) 
            goto read_again;
        err(1, "error waiting for qemu to switch buffers");
    }
    watch = xs_read_watch(xs, &len);
    free(watch);

    active_str = xs_read(xs, XBT_NULL, qemu_active_path, &len);
    if (active_str == NULL || active_str[0] - '0' != next_active)
        /* Watch fired but value is not yet right */
        goto read_again;
}

/***********************************************************************/
