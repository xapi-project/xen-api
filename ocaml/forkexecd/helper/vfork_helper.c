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

#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>
#include <pthread.h>
#include <limits.h>
#include <fcntl.h>
#include <unistd.h>
#include <syslog.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <sys/resource.h>

#include "close_from.h"
#include "syslog.h"
#include "logs.h"
#include "vfork_helper.h"

#define log(...) do {} while(0)
#include "redirect_algo.h"

typedef struct {
    char **args;
    mapping *mappings;
    fd_operation operations[MAX_OPERATIONS];
    int err;
    const char *err_func;
} exec_info;

static void adjust_args(char **args, mapping *const mappings, unsigned num_mappings);
static void reset_signal_handlers(void);
static void clear_cgroup(void);
static const char *get_arg(int *argc, char ***argv);
static int get_fd(int *argc, char ***argv);
static void error(int err, const char *msg, ...);
static void init_syslog(const char *key, bool redirect_stderr_to_stdout);

static int error_fd = -1;

int main(int argc, char **argv)
{
    unsigned num_mappings = 3;
    bool redirect_stderr_to_stdout = false;
    const char *key = NULL;
    struct rlimit nofile_limit;
    mapping mappings_buf[MAX_TOTAL_MAPPINGS];
    exec_info info[1] = { NULL, };
    const char *directory = "/";

    mapped_logs logs = mapped_logs_open();
#undef log
#define log(fmt, ...) mapped_logs_add(logs, fmt "\n", ## __VA_ARGS__)
#define log_fail(fmt, ...) do {\
    mapped_logs_failure(logs); \
    mapped_logs_add(logs, fmt "\n", ## __VA_ARGS__); \
} while(0)

    log("starting");

    info->mappings = mappings_buf;
    for (int i = 0; i < 3; ++i) {
        mapping *const m = &info->mappings[i];
        m->uuid = NULL;
        m->current_fd = -1;
        m->wanted_fd = i;
    }

    // Scan all arguments, check them and collect some information.
    ++argv;
    --argc;
    for (;;) {
        // we must have an argument left
        const char *arg = get_arg(&argc, &argv);

        // next must be a single letter option
        if (arg[0] != '-' || arg[1] == 0 || arg[2] != 0)
            error(EINVAL, "Invalid option %s", arg);

        // final "--"
        if (arg[1] == '-')
            break;

        switch (arg[1]) {
        case 'I': // stdin
            info->mappings[0].current_fd = get_fd(&argc, &argv);
            break;
        case 'O': // stdout
            info->mappings[1].current_fd = get_fd(&argc, &argv);
            break;
        case 'E': // stderr
            info->mappings[2].current_fd = get_fd(&argc, &argv);
            break;
        case 'm': { // mapping
                if (num_mappings >= MAX_TOTAL_MAPPINGS) {
                    log_fail("too many mappings");
                    mapped_logs_close(logs);
                    error(EINVAL, "Too many mappings");
                }
                const char *uuid = get_arg(&argc, &argv);
                if (strlen(uuid) != 36) {
                    log_fail("invalid mapping");
                    mapped_logs_close(logs);
                    error(EINVAL, "Invalid mapping UUID");
                }
                const int fd = get_fd(&argc, &argv);
                mapping* const m = &info->mappings[num_mappings++];
                m->uuid = uuid;
                m->current_fd = fd;
                m->wanted_fd = -1;
            }
            break;
        case 's': // syslog (with key)
            key = get_arg(&argc, &argv);
            break;
        case 'S': // syslog stderr to stdout
            redirect_stderr_to_stdout = true;
            break;
        case 'd':
            directory = get_arg(&argc, &argv);
            break;
        case 'e': { // error file descriptor
                error_fd = get_fd(&argc, &argv);
                if (num_mappings >= MAX_TOTAL_MAPPINGS) {
                    log_fail("too many mappings");
                    mapped_logs_close(logs);
                    error(EINVAL, "Too many mappings");
                }
                mapping* const m = &info->mappings[num_mappings++];
                m->uuid = NULL;
                m->current_fd = error_fd;
                m->wanted_fd = -1;
            }
            break;
        default:
            error(EINVAL, "Invalid option %s", arg);
        }
    }

    if (argc < 1) {
        log_fail("no args");
        mapped_logs_close(logs);
        error(EINVAL, "No command arguments");
    }

    info->args = argv;

    if (getrlimit(RLIMIT_NOFILE, &nofile_limit) < 0) {
        int err = errno;
        log_fail("getrlimit error");
        mapped_logs_close(logs);
        error(err, "getrlimit");
    }

    sigset_t sigset;

    // Compute the file operations we need to do for the file mappings
    int num_operations =
        redirect_mappings(info->mappings, num_mappings, info->operations);

    if (FORKEXECD_DEBUG_LOGS) {
        for (size_t n = 0; info->args[n]; ++n)
            log("arg %zd %s", n, info->args[n]);
    }

    // Rename all command line.
    adjust_args(info->args, info->mappings, num_mappings);

    if (FORKEXECD_DEBUG_LOGS) {
        for (size_t n = 0; info->args[n]; ++n)
            log("arg %zd %s", n, info->args[n]);
    }

    reset_signal_handlers();

    if (strcmp(directory, ".") != 0 && chdir(directory) < 0) {
        int err = errno;
        log_fail("chdir %d", err);
        error(err, "chdir");
    }

    // Clear cgroup otherwise systemd will shutdown processes if
    // toolstack is restarted.
    clear_cgroup();

    if (setsid() < 0) {
        int err = errno;
        log_fail("setsid %d", errno);
        error(err, "setsid");
    }

    // Redirect file descriptors.
    int err = 0;
    const char *err_func = NULL;
    for (int i = 0; i < num_operations && err == 0; ++i) {
        const fd_operation* const op = &info->operations[i];
        log("op %d %d %d", op->fd_from, op->fd_to, op->operation);
        switch (op->operation) {
        case FD_OP_DUP:
            if (op->fd_from == op->fd_to) {
                // These file descriptors came from another process,
                // so surely they have the CLOEXEC flag set, nothing
                // to do.
                break;
            } else {
                err_func = "dup2";
                if (dup2(op->fd_from, op->fd_to) < 0)
                    err = errno;
                if (op->fd_from == error_fd)
                    error_fd = op->fd_to;
            }
            break;
        case FD_OP_MOVE:
            err_func = "dup2";
            if (dup2(op->fd_from, op->fd_to) < 0)
                err = errno;
            if (op->fd_from == error_fd)
                error_fd = op->fd_to;
            close(op->fd_from);
            break;
        case FD_OP_DEVNULL:
            // first close old, then create new one
            close(op->fd_to);
            // TODO ideally we want read only for input for Ocaml did the same...
            err_func = "open";
            if (open("/dev/null", O_WRONLY) != op->fd_to)
                err = errno ? errno : EBADF;
            break;
        case FD_OP_CLOSE_FROM:
            close_fds_from(op->fd_from);
            break;
        default:
            err_func = "safe_exec";
            err = EINVAL;
        }
    }
    if (err != 0) {
        info->err = err;
        info->err_func = err_func;
        log_fail("redirect error %d in %s", err, err_func);
        error(err, "%s", err_func);
    }

    if (key)
        init_syslog(key, redirect_stderr_to_stdout);

    // Limit number of files limits to standard limit to avoid
    // creating bugs with old programs.
    if (nofile_limit.rlim_cur > 1024) {
        nofile_limit.rlim_cur = 1024;
        setrlimit(RLIMIT_NOFILE, &nofile_limit);
    }

    // Reset signal mask, inherited by the process we are going to execute
    sigemptyset(&sigset);
    pthread_sigmask(SIG_SETMASK, &sigset, NULL);

    log("execv...");
    mapped_logs_success(logs);
    if (error_fd >= 0)
        close(error_fd);
    execv(info->args[0], info->args);
    log_fail("execve failed %d", errno);
    // Here we could set err and err_func but we kept compatibility
    // with forkexecd daemon.
    exit(errno == ENOENT ? 127 : 126);
}

static void
adjust_args(char **args, mapping *const mappings, unsigned num_mappings)
{
    for (; *args; ++args) {
        char *arg = *args;
        size_t len = strlen(arg);
        if (len < 36)
            continue;

        // replace uuid with file descriptor
        char *uuid = arg + len - 36;
        for (unsigned i = 0; i < num_mappings; ++i) {
            const mapping *m = &mappings[i];
            if (m->uuid == NULL || strcmp(m->uuid, uuid) != 0)
                continue;
            sprintf(uuid, "%d", m->current_fd);
        }
    }
}

static void
reset_signal_handlers(void)
{
    for (int sig = 1; sig < NSIG; ++sig) {
        // these signals can't be overridden
        if (sig == SIGKILL || sig == SIGSTOP)
            continue;

        // Set signal dispositions.
        // This avoids inherit unwanted overrides.
        // Also prevent handling unwanted signal handler, especially using vfork().
        // Use ignore SIGPIPE for compatibility with forkexecd.
        signal(sig, sig == SIGPIPE ? SIG_IGN : SIG_DFL);
    }
}

static void
clear_cgroup(void)
{
    int fd = open("/sys/fs/cgroup/systemd/cgroup.procs", O_WRONLY|O_CLOEXEC);
    if (fd >= 0) {
        char string_pid[32];
        int ignored __attribute__((unused));
        sprintf(string_pid, "%d\n", (int) getpid());
        ignored = write(fd, string_pid, strlen(string_pid));
        close(fd);
    }
}

static const char *
get_arg(int *argc, char ***argv)
{
    if (*argc < 0)
        error(EINVAL, "Expected one more argument");

    const char *arg = **argv;
    --(*argc);
    ++(*argv);
    return arg;
}

static int
get_fd(int *argc, char ***argv)
{
    const char *arg = get_arg(argc, argv);
    unsigned long fd = strtoul(arg, NULL, 0);
    if (fd < 0 || fd > INT_MAX)
        error(EINVAL, "Expected valid file descriptor number");
    return (int) fd;
}

static void
error(int err, const char *format, ...)
{
    if (error_fd >= 0) {
        msg_t msg = { err };
        va_list ap;
        va_start(ap, format);
        int ignored __attribute__((unused));
        vsnprintf(msg.msg_buf, sizeof(msg.msg_buf), format, ap);
        msg.msg_buf[sizeof(msg.msg_buf) - 1] = 0;
        va_end(ap);
        ignored = write(error_fd, &msg, offsetof(msg_t, msg_buf) + strlen(msg.msg_buf) + 1);
    }
    exit(125);
}

static void
init_syslog(const char *key, bool redirect_stderr_to_stdout)
{
    int fds[2];
    if (pipe(fds) < 0)
        error(errno, "pipe");
    dup2(fds[1], 1);
    if (redirect_stderr_to_stdout)
        dup2(fds[1], 2);
    close(fds[1]);

    const int child_pid = (int) getpid();

    pid_t pid = fork();
    if (pid < 0)
        error(errno, "fork");

    if (pid == 0) {
        // child
        close(0);
        close(1);
        if (open("/dev/null", O_RDONLY) != 0
            || open("/dev/null", O_WRONLY) != 1)
            error(errno, "open");
        dup2(1, 2);
        if (fds[0] != 3) {
            dup2(fds[0], 3);
            fds[0] = 3;
        }
        close_fds_from(4);

        pid = fork();
        if (pid < 0)
            error(errno, "fork");
        if (pid > 0)
            // parent
            exit(0);

        openlog("forkexecd", 0, LOG_DAEMON);
        forward_to_syslog(fds[0], key, child_pid);
        exit(0);
    }

    close(fds[0]);

    // parent
    int status;
    wait(&status);
    if (!WIFEXITED(status))
        error(EPIPE, "syslogger");

    switch (WEXITSTATUS(status)) {
    case 0:
        // success
        return;
    case 125:
        // forward error, a proper message will be forwarded
        exit(125);
    }
    error(EPIPE, "syslogger");
}
