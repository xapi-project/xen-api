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
static int qemu_shmid = -1;
static struct xs_handle *xs;


/* Mark the shared-memory segment for destruction */
static void qemu_destroy_buffer(void)
{
    if (qemu_shmid != -1)
        shmctl(qemu_shmid, IPC_RMID, NULL);
    qemu_shmid = -1;
}

/* Get qemu to change buffers. */
void qemu_flip_buffer(int domid, int next_active)
{
    char digit = '0' + next_active;
    unsigned int len;
    char *active_str, **watch;
    struct timeval tv;
    fd_set fdset;
    int rc;

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

void * init_qemu_maps(int domid, unsigned int bitmap_size)
{
    key_t key;
    char key_ascii[17] = {0,};
    void *seg;
    char *path, *p;
    struct shmid_ds ds_buf;
    struct group *gr;

    /* Make a shared-memory segment */
    do {
        key = rand(); /* No security, just a sequence of numbers */
        qemu_shmid = shmget(key, 2 * bitmap_size, 
                       IPC_CREAT|IPC_EXCL|S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP);
        if (qemu_shmid == -1 && errno != EEXIST)
            errx(1, "can't get shmem to talk to qemu-dm");
    } while (qemu_shmid == -1);

    /* Remember to tidy up after ourselves */
    atexit(qemu_destroy_buffer);

    /* Change owner so that qemu can map it. */
    gr = getgrnam("qemu_base");
    if (!gr)
        err(1, "can't get qemu gid");
    if (shmctl(qemu_shmid, IPC_STAT, &ds_buf) < 0)
        err(1, "can't get status of shm area");
    ds_buf.shm_perm.gid = gr->gr_gid + (unsigned short)domid;
    if (shmctl(qemu_shmid, IPC_SET, &ds_buf) < 0)
        err(1, "can't change gid of shm area");

    /* Map it into our address space */
    seg = shmat(qemu_shmid, NULL, 0);
    if (seg == (void *) -1)
        errx(1, "can't map shmem to talk to qemu-dm");
    memset(seg, 0, 2 * bitmap_size);

    /* Write the size of it into the first 32 bits */
    *(uint32_t *)seg = bitmap_size;

    /* Tell qemu about it */
    if ((xs = xs_daemon_open()) == NULL)
        errx(1, "Couldn't contact xenstore");
    if (!(path = xs_get_domain_path(xs, domid)))
        errx(1, "can't get domain path in store");
    if (!(path = realloc(path, strlen(path)
                         + strlen("/logdirty/next-active") + 1)))
        errx(1, "no memory for constructing xenstore path");
    strcat(path, "/logdirty/");
    p = path + strlen(path);

    strcpy(p, "key");
    snprintf(key_ascii, 17, "%16.16llx", (unsigned long long) key);
    if (!xs_write(xs, XBT_NULL, path, key_ascii, 16))
        errx(1, "can't write key (%s) to store path (%s)\n", key_ascii, path);

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

    qemu_flip_buffer(domid, 0);

    free(path);
    return seg;
}

/***********************************************************************/
