
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

#undef NDEBUG
#define DEBUG 1

#if DEBUG
#define log(fmt, ...) printf(fmt "\n", ##__VA_ARGS__)
#else
#define log(fmt, ...) do {} while(0)
#endif

// include as first file to make sure header is self contained
#include "redirect_algo.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

static int fake_close(int fd);

typedef struct {
    bool open;
    bool cloexec;
    char *name;
} fd;

#define NUM_FDS 4096
static fd fds[NUM_FDS];

static bool
fake_close_fds_from(int fd_from)
{
    for (int fd = fd_from; fd < NUM_FDS; ++fd)
        fake_close(fd);

    return true;
}

#define O_WRONLY 1
static int
fake_open(const char *fn, int dummy)
{
    for (int i = 0; i < NUM_FDS; ++i)
        if (!fds[i].open) {
            assert(fds[i].name == NULL);
            fds[i].name = strdup(fn);
            fds[i].open = true;
            fds[i].cloexec = false;
            return i;
        }
    assert(0);
    return -1;
}

static int
fake_close(int fd)
{
    assert(fd >= 0);
    assert(fd < NUM_FDS);
    if (!fds[fd].open) {
        errno = EBADF;
        return -1;
    }
    fds[fd].open = false;
    free(fds[fd].name);
    fds[fd].name = NULL;
    return 0;
}

static int
fake_dup2(int from, int to)
{
    assert(from >= 0 && from < NUM_FDS);
    assert(to >= 0 && to < NUM_FDS);
    assert(fds[from].open);
    assert(from != to);
    free(fds[to].name);
    fds[to].open = true;
    fds[to].name = strdup(fds[from].name);
    fds[to].cloexec = false;
    return 0;
}

static int
fake_fcntl(int fd)
{
    assert(fd >= 0 && fd < NUM_FDS);
    assert(fds[fd].open);
    fds[fd].cloexec = false;
    return 0;
}

int main(int argc, char **argv)
{
    // Input where a given FD goes??
    // No, not enough, can be duplicated.
    // Numbers >4096 in 2 bytes not file descriptor,
    // (-1 for standard, skip for normal).
    // We should add some random fds.
    enum { MAX_FILE_BUF = 2048 };
    uint16_t file_buf[MAX_FILE_BUF];
    size_t read = fread(file_buf, 2, MAX_FILE_BUF, stdin);
    if (read < 3)
        return 0;

    static const char standard_names[][8] = {
        "stdin", "stdout", "stderr"
    };
    int num_mappings = 0;
    uint16_t *num = file_buf;
    mapping mappings[MAX_FILE_BUF];
    int i = 0;
    for (i = 0; i < 3; ++i) {
        mapping *m = &mappings[num_mappings++];
        m->uuid = standard_names[i];
        uint16_t n = *num++;
        m->current_fd = n < NUM_FDS ? n : -1;
        m->wanted_fd = i;
    }
    for (; i < read; ++i) {
        uint16_t n = *num++;
        if (n >= NUM_FDS)
            continue;

        mapping *m = &mappings[num_mappings++];
        m->current_fd = n;
        m->wanted_fd = -1;
        char buf[64];
        sprintf(buf, "file%d", i);
        m->uuid = strdup(buf);
    }
    if (num_mappings > MAX_TOTAL_MAPPINGS)
        return 0;

    for (unsigned n = 0; n < num_mappings; ++n) {
        mapping *m = &mappings[n];
        int fd = m->current_fd;
        if (fd < 0)
            continue;
        fake_close(fd);
        fds[fd].open = true;
        fds[fd].name = strdup(m->uuid);
        fds[fd].cloexec = true;
    }

    // Check in the final file mapping all valid mappings
    // have an open file descriptor.
    // There should be no duplicate numbers in current_fd.
    // current_fd must be in a range.
    // Only if wanted_fd >= 0 current_fd can be -1.
    // There should be a correspondance between input and output names.
    // If current_fd was -1 it will still be -1.
    // If wanted_fd >= 0 current_fd should be the same.

    fd_operation operations[MAX_OPERATIONS];
    int num_operations =
        redirect_mappings(mappings, num_mappings, operations);
    assert(num_operations > 0);
    assert(num_operations <= MAX_OPERATIONS);

    for (int i = 0; i < num_operations; ++i) {
        const fd_operation* op = &operations[i];
        log("op %d %d %d", op->fd_from, op->fd_to, op->operation);
        switch (op->operation) {
        case FD_OP_DUP:
            if (op->fd_from == op->fd_to)
                fake_fcntl(op->fd_from);
            else
                fake_dup2(op->fd_from, op->fd_to);
            break;
        case FD_OP_MOVE:
            assert(op->fd_from != op->fd_to);
            fake_dup2(op->fd_from, op->fd_to);
            fake_close(op->fd_from);
            break;
        case FD_OP_DEVNULL:
            // first close old, then create new one
            fake_close(op->fd_to);
            // TODO ideally we want read only for input for Ocaml did the same...
            assert(fake_open("/dev/null", O_WRONLY) == op->fd_to);
            break;
        case FD_OP_CLOSE_FROM:
            fake_close_fds_from(op->fd_from);
            break;
        default:
            assert(0);
        }
    }

    // check files opened
    for (int fd = 0; fd < NUM_FDS; ++fd)
        assert(fds[fd].open == (fd < num_mappings));

    for (int fd = 0; fd < num_mappings; ++fd) {
        assert(fds[fd].cloexec == false);
        log("file %d %s", fd, fds[fd].name);
    }

    // Check in the final file mapping all valid mappings
    // has an open file descriptor.
    bool already_found[NUM_FDS] = { false, };
    for (unsigned n = 0; n < num_mappings; ++n) {
        const int fd = mappings[n].current_fd;
        const int wanted = mappings[n].wanted_fd;
        if (fd >= 0) {
            assert(fd < NUM_FDS);
            assert(fds[fd].open);

            // There should be no duplicate numbers in current_fd.
            assert(!already_found[fd]);
            already_found[fd] = true;
        } else {
            // Only if wanted_fd >= 0 current_fd can be -1.
            assert(mappings[n].wanted_fd >= 0);
            assert(fd == -1);
        }

        // If wanted_fd >= 0 current_fd should be the same.
        if (wanted >= 0)
            assert(wanted == fd || fd == -1);

        // current_fd must be in a range.
        assert(fd >= -1);
        assert(fd < num_mappings);
    }

    // There should be a correspondance between input and output names.
    // If current_fd was -1 it will still be -1.
}
