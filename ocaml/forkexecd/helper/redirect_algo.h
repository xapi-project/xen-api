/* Algorithm used to remap file handles before executing a process.
 * The algorithm is separated in a different file in order to reuse for
 * fuzzing it.
 */

#pragma once

#if !defined(DEBUG)
#define DEBUG 0
#endif

#if (DEBUG) != 0 && (DEBUG) != 1
#error Expected DEBUG to be defined either 0 or 1
#endif

#ifndef log
#error Expected log macro to be defined
#endif

#include <stdio.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    const char *uuid;
    int current_fd;
    int wanted_fd;
} mapping;

typedef struct {
    // source file
    int fd_from;
    // destination file
    short fd_to;
    // see FD_OP_ constants
    uint8_t operation;
} fd_operation;

typedef enum {
    // Duplicate from fd_from to fd_to.
    // If fd_from is the same as fd_to remove FD_CLOEXEC flag.
    FD_OP_DUP,
    // Duplicate from fd_from to fd_to and close fd_from.
    FD_OP_MOVE,
    // Open /dev/null on fd_to.
    FD_OP_DEVNULL,
    // Close from fd_from to the sky!
    FD_OP_CLOSE_FROM,
} FD_OP;

#define MAX_OPERATIONS 1024
#define MAX_TOTAL_MAPPINGS (MAX_OPERATIONS - 4)

static uint16_t remap_fds(mapping *const mappings, unsigned num_mappings, int from, int to);

// Given the passed mappings update them (current_fd) and returns the
// requested operations to do the job.
// First 3 mappings should refer to standard file descriptors (stdin,
// stdout, stderr).
// Returns the number of operations to perform or negative if error.
static int
redirect_mappings(mapping *const mappings, const unsigned num_mappings, fd_operation *operations)
{
    mapping *const end_mappins = mappings + num_mappings;
    uint16_t used_fds[MAX_OPERATIONS] = {0,};
    fd_operation *ops = operations;

#define DUMP_MAPPINGS do { \
    if (DEBUG) { \
        for (unsigned i = 0; i < num_mappings; ++i) { \
            const mapping *m __attribute__((unused)) = &mappings[i]; \
            log("mapping %s %d %d", m->uuid, m->current_fd, m->wanted_fd); \
        } \
        char lbuf[MAX_OPERATIONS* 16]; \
        lbuf[0] = 0; \
        for (int i = 0; i < MAX_OPERATIONS; ++i) { \
            if (used_fds[i]) \
                sprintf(strchr(lbuf, 0), "%d=%d,", i, used_fds[i]); \
        } \
        log("used %s", lbuf); \
    } \
} while(0);

    log("handle");

    // parse all mappings + standard fds, mark ones using range 0-MAX_OPERATIONS
    for (mapping *m = mappings; m < end_mappins; ++m) {
        if (m->current_fd < 0 || m->current_fd >= MAX_OPERATIONS)
            continue;
        used_fds[m->current_fd]++;
    }
    DUMP_MAPPINGS;

    // Move standard file descriptors out of the way.
    // Maximum 3 operations.
    log("move standard fds away");
    for (mapping *m = mappings; m < end_mappins; ++m) {
        const int current_fd = m->current_fd;
        if (current_fd < 0 || current_fd > 2)
            continue;
        // find first available fd to use
        int fd = 3;
        while (used_fds[fd])
            ++fd;
        *ops++ = (fd_operation){ current_fd, fd, FD_OP_DUP };
        uint16_t changed = remap_fds(mappings, num_mappings, current_fd, fd);
        log("changed %d from %d to %d", changed, current_fd, fd);
        used_fds[current_fd] = 0;
        used_fds[fd] = changed;
    }
    DUMP_MAPPINGS;

    // Move standard fds into proper positions
    // Maximum 3 operations (standard fds to be moved).
    log("move standard fds correctly");
    for (mapping *m = mappings; m < end_mappins; ++m) {
        const int current_fd = m->current_fd;
        if (current_fd < 0 || m->wanted_fd < 0)
            continue;
        int fd = m->wanted_fd;
        FD_OP op = FD_OP_DUP;
        if (current_fd >= num_mappings) {
            // move
            op = FD_OP_MOVE;
            uint16_t changed = remap_fds(mappings, num_mappings, current_fd, fd);
            log("changed %d from %d to %d", changed, current_fd, fd);
            used_fds[fd] = changed;
        } else {
            // duplicate
            m->current_fd = fd;
            if (--used_fds[current_fd] == 0)
                op = FD_OP_MOVE;
            used_fds[fd] = 1;
        }
        *ops++ = (fd_operation){ current_fd, fd, op };
    }
    DUMP_MAPPINGS;

    // Remove cloexec on range [3, 3 + num mappings).
    // Maximum no standard mappings operations.
    log("remove cloexec flags");
    for (int fd = 3; fd < num_mappings; ++fd) {
        if (!used_fds[fd])
            continue;
        log("remove cloexec from %d", fd);
        *ops++ = (fd_operation){ fd, fd, FD_OP_DUP };
    }
    DUMP_MAPPINGS;

    // Move all fds left in range [3, 3 + num mappings).
    // Maximum no standard mapping operations; then sum with
    // the above is the no standard mapping operations.
    log("move all fds left in range");
    int last_free = 3;
    for (mapping *m = mappings; m < end_mappins; ++m) {
        const int current_fd = m->current_fd;
        if (m->wanted_fd >= 0)
            continue;
        if (current_fd < num_mappings && used_fds[current_fd] == 1)
            continue;
        while (used_fds[last_free])
            ++last_free;
        int fd = last_free;
        // TODO copied from above
        FD_OP op = FD_OP_DUP;
        if (current_fd >= num_mappings) {
            // move
            op = FD_OP_MOVE;
            uint16_t changed = remap_fds(mappings, num_mappings, current_fd, fd);
            log("changed %d from %d to %d", changed, current_fd, fd);
            used_fds[fd] = changed;
        } else {
            // duplicate
            m->current_fd = fd;
            if (--used_fds[current_fd] == 0)
                op = FD_OP_MOVE;
            used_fds[fd] = 1;
        }
        *ops++ = (fd_operation){ current_fd, fd, op };
    }
    DUMP_MAPPINGS;

    // Close extra fds.
    *ops++ = (fd_operation){ num_mappings, 0, FD_OP_CLOSE_FROM };

    // Create missing standard fds.
    // Maximum standard mapping operations, but not the above,
    // so the sum with move the standard is 3.
    for (int fd = 0; fd < 3; ++fd) {
        if (used_fds[fd])
            continue;
        *ops++ = (fd_operation){ fd, fd, FD_OP_DEVNULL };
    }

    return ops - operations;
}

static uint16_t
remap_fds(mapping *const mappings, unsigned num_mappings, int from, int to)
{
    uint16_t res = 0;
    for (unsigned i = 0; i < num_mappings; ++i) {
        mapping *m = &mappings[i];
        if (m->current_fd == from) {
            m->current_fd = to;
            res++;
        }
    }
    return res;
}
