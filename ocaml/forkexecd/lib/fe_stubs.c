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

#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <pthread.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <sys/resource.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>

#include "close_from.h"

#define FOREACH_LIST(name, list) \
    for(value name = (list); name != Val_emptylist; name = Field(name, 1))

#include "logs.h"

// Create thread reducing stack usage to a minimum to reduce memory usage.
// Returns error number (like pthread_create).
static int create_thread_minstack(pthread_t *th, void *(*proc)(void *), void *arg);

static value alloc_process_status(int pid, int status);

// Internal custom object.
// This structure cannot be put into Ocaml custom data to avoid move it
// as containing a mutex (which cannot be moved).
typedef struct {
    // pid of the process we wait, never changed
    pid_t pid;

    // Protects reaped flag below.
    // This lock is used while Ocaml is in unblocking state so no
    // blocking calls should be made.
    pthread_mutex_t mtx;

    // if pid was reaped, we need to reap only once
    bool reaped;
} pidwaiter;

#define PIDWAITER(name, value) \
    pidwaiter *const name = (*((pidwaiter**) Data_custom_val(value)))

static void *
thread_proc_reap(void *arg)
{
    pid_t pid = (pid_t) (intptr_t) arg;

    int status;
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR)
        continue;

    return NULL;
}

static void
pidwaiter_finalize(value v)
{
    PIDWAITER(waiter, v);

    pid_t pid = waiter->pid;
    bool reaped = waiter->reaped;
    pthread_mutex_destroy(&waiter->mtx);
    caml_stat_free(waiter);

    // reap the pid to avoid zombies
    if (!reaped) {
        pthread_t th;
        if (create_thread_minstack(&th, thread_proc_reap, (void *) (intptr_t) pid) == 0)
            pthread_detach(th);
    }
}

static struct custom_operations custom_ops = {
    "com.xenserver.pidwaiter",
    pidwaiter_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static value
pidwaiter_alloc(pid_t pid)
{
    // Allocate static memory, we can't retain pointer to
    // Ocaml dynamic memory when we release Ocaml engine.
    pidwaiter *waiter = caml_stat_alloc(sizeof(pidwaiter));

    waiter->pid = pid;
    pthread_mutex_init(&waiter->mtx, NULL);
    waiter->reaped = false;

    value v = caml_alloc_custom(&custom_ops, sizeof(pidwaiter*), 0, 1);
    *((pidwaiter**) Data_custom_val(v)) = waiter;
    return v;
}

#define log(...) do {} while(0)
#include "fe_stubs_redirect_algo.h"

typedef struct {
    char **args;
    char **environment;
    mapping *mappings;
    fd_operation operations[MAX_OPERATIONS];
    int err;
    const char *err_func;
} exec_info;

// Appends a string to *p_dest buffer.
// It updates *p_dest to point after copied string.
// Returns copied string.
static char *
append_string(char **p_dest, const char *s)
{
    char *const dest = *p_dest;
    size_t const size = strlen(s) + 1;
    memcpy(dest, s, size);
    *p_dest = dest + size;
    return dest;
}

static void adjust_args(char **args, mapping *const mappings, unsigned num_mappings);
static void reset_signal_handlers(void);
static void clear_cgroup(void);
static const char *get_syslog_key(value syslog_stdout, value args);

CAMLprim value
caml_safe_exec_nat(value environment,
    value stdin_fd, value stdout_fd, value stderr_fd,
    value id_mapping, value syslog_stdout, value redirect, value args)
{
    // We need to retain all paramters, they are either array,
    // list or options.
    CAMLparam4(environment, stdin_fd, stdout_fd, stderr_fd);
    CAMLxparam4(id_mapping, syslog_stdout, redirect, args);
    CAMLlocal1(waiter);

    value res;
    unsigned num_args = 0, num_mappings = 0;
    const mlsize_t num_environment = caml_array_length(environment);
    size_t strings_size = 0;
    const bool redirect_stderr_to_stdout =
        (syslog_stdout == Val_none || redirect == Val_none) ? false : Bool_val(Some_val(redirect));
    const char *const key = get_syslog_key(syslog_stdout, args);
    pid_t pid;
    struct rlimit nofile_limit;

    mapped_logs logs = mapped_logs_open();
#undef log
#define log(fmt, ...) mapped_logs_add(logs, fmt "\n", ## __VA_ARGS__)
#define log_fail(fmt, ...) do {\
    mapped_logs_failure(logs); \
    mapped_logs_add(logs, fmt "\n", ## __VA_ARGS__); \
} while(0)

    log("starting");

    // Scan all arguments, check them and collect some information.

    // Standard file descriptors.
    int standard_fds[3];
    const value standard_fd_values[3] = { stdin_fd, stdout_fd, stderr_fd };
    for (int i = 0; i < 3; ++i) {
        int fd = -1;
        if (standard_fd_values[i] != Val_none) {
            fd = Int_val(Some_val(standard_fd_values[i]));
            if (fd < 0) {
                log_fail("invalid standard fd");
                mapped_logs_close(logs);
                unix_error(EINVAL, "safe_exec", Nothing);
            }
        }
        standard_fds[i] = fd;
    }

    FOREACH_LIST(arg, args) {
        strings_size += strlen(String_val(Field(arg, 0))) + 1;
        ++num_args;
    }
    if (num_args < 1) {
        log_fail("no args");
        mapped_logs_close(logs);
        unix_error(EINVAL, "safe_exec", Nothing);
    }

    for (mlsize_t n = 0; n < num_environment; ++n)
        strings_size += strlen(String_val(Field(environment, n))) + 1;

    FOREACH_LIST(map, id_mapping) {
        ++num_mappings;
        value item = Field(map, 0);
        const char *const uuid = String_val(Field(item, 0));
        const int current_fd = Int_val(Field(item, 1));
        // Check values.
        // The check for UUID length makes sure we'll have space to replace last part of
        // the command line arguments with file descriptor.
        if (strlen(uuid) != 36 || current_fd < 0) {
            log_fail("invalid mapping");
            mapped_logs_close(logs);
            unix_error(EINVAL, "safe_exec", Nothing);
        }
        strings_size += 36 + 1;
    }
    // Limit number of mappings to some sensible value.
    // Also to avoid overflows with redirect code.
    if (num_mappings > MAX_TOTAL_MAPPINGS) {
        log_fail("too many mappings");
        mapped_logs_close(logs);
        unix_error(EINVAL, "safe_exec", Nothing);
    }

    // Prefix arguments with our helper to support syslog redirection.
    if (key) {
        num_args += 3;
        strings_size += 64;
        strings_size += strlen(key) + 1;
    }

    if (getrlimit(RLIMIT_NOFILE, &nofile_limit) < 0) {
        log_fail("getrlimit error");
        mapped_logs_close(logs);
        unix_error(errno, "safe_exec", Nothing);
    }

    // Allocate buffer to copy all data that we need.
    num_mappings += 3;
    size_t total_size =
        sizeof(exec_info) +
        sizeof(char*) * (num_args + 1 + num_environment + 1) +
        sizeof(mapping) * num_mappings +
        strings_size;
    exec_info *info = (exec_info *) calloc(total_size, 1);
    if (!info)
        caml_raise_out_of_memory();

    log("copying");

    // Copy all needed information.
    {
        char **ptrs = (char**)(info + 1);
        mapping *mappings = (mapping *) (ptrs + num_args + 1 + num_environment + 1);
        char *strings = (char*) (mappings + num_mappings);
#ifdef FORKEXECD_DEBUG_LOGS
        mapping *const initial_mappings = mappings;
        char *const initial_strings = strings;
#endif

        // Copy arguments into list.
        info->args = ptrs;
        if (key) {
            *ptrs++ = "/usr/libexec/xapi/syslogger";
            sprintf(strings, "1:%d:-1", !!redirect_stderr_to_stdout);
            *ptrs++ = strings;
            strings += 64;
            *ptrs++ = append_string(&strings, key);
        }
        FOREACH_LIST(arg, args) {
            const char *const s = String_val(Field(arg, 0));
            *ptrs++ = append_string(&strings, s);
        }
        *ptrs++ = NULL;

        // Copy environments into list.
        // We need to copy also the content, Ocaml system could move it
        // after calling caml_enter_blocking_section.
        info->environment = ptrs;
        for (mlsize_t n = 0; n < num_environment; ++n) {
            const char *const s = String_val(Field(environment, n));
            *ptrs++ = append_string(&strings, s);
        }
        *ptrs++ = NULL;

        // Copy all mappings, we need to change them.
        info->mappings = mappings;
        for (int i = 0; i < 3; ++i) {
            mapping *m = mappings++;
            m->uuid = NULL;
            m->current_fd = standard_fds[i];
            m->wanted_fd = i;
        }
        FOREACH_LIST(map, id_mapping) {
            mapping *m = mappings++;
            value item = Field(map, 0);
            m->uuid = append_string(&strings, String_val(Field(item, 0)));
            m->current_fd = Int_val(Field(item, 1));
            m->wanted_fd = -1;
        }

#ifdef FORKEXECD_DEBUG_LOGS
        if (strings - (char*)info != total_size
            || initial_mappings != (mapping*) ptrs
            || initial_strings != (char*) mappings) {
            log_fail("internal error copying");
            free(info);
            mapped_logs_close(logs);
            unix_error(EACCES, "safe_exec", Nothing);
        }
#endif
    }

    // Release Ocaml engine, it could feel all operations are pretty quick
    // so there's no need but potentially launching a process can take
    // some time (like files on NFS), so better safe than sorry.
    caml_enter_blocking_section();
    {
    // make sure we don't use Ocaml variables anymore in the section
    enum {
        environment, stdin_fd, stdout_fd, stderr_fd, waiter,
        id_mapping, syslog_stdout, redirect, args,
        key, strings_size, res
    };

    sigset_t sigset, old_sigset;
    int cancellation_state;

    // Compute the file operations we need to do for the file mappings,
    // this is done before vfork() call to reduce operations done between
    // vfork() and execve().
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

    // Disable cancellation to avoid some signals.
    // Glibc use some signals to handle thread cancellation.
    pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancellation_state);

    // Block all possible signals to avoid receiving some in the child.
    // Signal mask is inherited to new process/thread will start with
    // all signals disabled and we can safely change them.
    sigfillset(&sigset);
    pthread_sigmask(SIG_BLOCK, &sigset, &old_sigset);

    log("vfork");

    // fork
    pid = vfork();
    if (pid < 0) {
        int err = errno;
        // error, notify with an exception
        log_fail("vfork %d", err);
        free(info);
        mapped_logs_close(logs);
        caml_leave_blocking_section();
        unix_error(err, "safe_exec", Nothing);
    }

    if (pid == 0) {
        // child
        log("child");

        reset_signal_handlers();

        if (chdir("/") < 0) {
            info->err = errno;
            info->err_func = "chdir";
            log_fail("chdir %d", errno);
            _exit(125);
        }

        // Clear cgroup otherwise systemd will shutdown processes if
        // toolstack is restarted.
        clear_cgroup();

        if (setsid() < 0) {
            info->err = errno;
            info->err_func = "setsid";
            log_fail("setsid %d", errno);
            _exit(125);
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
                    err_func = "fcntl";
                    int flags = fcntl(op->fd_from, F_GETFD);
                    if (flags == -1) {
                        err = errno;
                    } else if ((flags & FD_CLOEXEC) != 0) {
                        if (fcntl(op->fd_from, F_SETFD, flags & ~FD_CLOEXEC) == -1)
                            err = errno;
                    }
                } else {
                    err_func = "dup2";
                    if (dup2(op->fd_from, op->fd_to) < 0)
                        err = errno;
                }
                break;
            case FD_OP_MOVE:
                err_func = "dup2";
                if (dup2(op->fd_from, op->fd_to) < 0)
                    err = errno;
                close(op->fd_from);
                break;
            case FD_OP_CLOSE:
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
            _exit(125);
        }

        // Limit number of files limits to standard limit to avoid
        // creating bugs with old programs.
        if (nofile_limit.rlim_cur > 1024) {
            nofile_limit.rlim_cur = 1024;
            setrlimit(RLIMIT_NOFILE, &nofile_limit);
        }

        // Reset signal mask, inherited by the process we are going to execute
        sigemptyset(&sigset);
        pthread_sigmask(SIG_SETMASK, &sigset, NULL);

        log("execve...");
        mapped_logs_success(logs);
        execve(info->args[0], info->args, info->environment);
        log_fail("execve failed %d", errno);
        // Here we could set err and err_func but we kept compatibility
        // with forkexecd daemon.
        _exit(errno == ENOENT ? 127 : 126);
    }

    log("done");

    // Restore thread state
    pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);
    pthread_setcancelstate(cancellation_state, NULL);
    mapped_logs_close(logs);

    }
    caml_leave_blocking_section();

    // Handle errors from child.
    int err = info->err;
    const char *err_func = info->err_func;
    free(info);
    if (err != 0 && err_func != NULL) {
        // Here we are reaping a zombie process, no
        // blocking is possible.
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR)
            continue;
        unix_error(err, err_func, Nothing);
    }

    waiter = pidwaiter_alloc(pid);

    res = caml_alloc_small(2, 0);
    Field(res, 0) = waiter;
    Field(res, 1) = Val_int(pid);

    CAMLreturn(res);
}

CAMLprim value
caml_safe_exec_byte(value *argv)
{
    return caml_safe_exec_nat(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}

// Helper function to return syslog key.
// Note that pointer will point to Ocaml memory so particular attention has to
// be taken to avoid GC to start.
static const char *
get_syslog_key(value syslog_stdout, value args)
{
    // no syslog parameter
    if (syslog_stdout == Val_none)
        return NULL;

    // remove the optional
    syslog_stdout = Some_val(syslog_stdout);

    // no-constant, user passed the key
    if (Is_block(syslog_stdout))
        return String_val(Field(syslog_stdout,0));

    // no syslog wanted
    if (Int_val(syslog_stdout) == 0)
        return NULL;

    // wrong arguments, will be checked later on the code
    if (args == Val_emptylist)
        return NULL;

    // get basename of first argument
    const char *filename = String_val(Field(args, 0));
    const char *sep = strrchr(filename, '/');
    return sep ? sep + 1 : filename;
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

CAMLprim value
caml_pidwaiter_dontwait(value waiter_val)
{
    CAMLparam1(waiter_val);
    PIDWAITER(waiter, waiter_val);

    pthread_mutex_lock(&waiter->mtx);
    bool reaped = waiter->reaped;
    waiter->reaped = true;
    pthread_mutex_unlock(&waiter->mtx);

    // reap the pid to avoid zombies
    if (!reaped) {
        pthread_t th;
        if (create_thread_minstack(&th, thread_proc_reap, (void *) (intptr_t) waiter->pid) == 0)
            pthread_detach(th);
    }

    CAMLreturn(Val_unit);
}

typedef struct {
    pid_t pid;
    bool timed_out;
    bool stop;
    struct timespec deadline;
    pthread_mutex_t mtx;
    pthread_cond_t cond;
} timeout_kill;

static void *
thread_proc_timeout_kill(void *arg)
{
    timeout_kill *tm = (timeout_kill *) arg;

    pthread_mutex_lock(&tm->mtx);
    int res  = tm->stop ? 0:
        pthread_cond_timedwait(&tm->cond, &tm->mtx, &tm->deadline);
    pthread_mutex_unlock(&tm->mtx);

    if (res == ETIMEDOUT) {
        kill(tm->pid, SIGKILL);
        tm->timed_out = true;
    }
    return NULL;
}

static int
create_thread_minstack(pthread_t *th, void *(*proc)(void *), void *arg)
{
    int res;

    // disable any possible signal handler so we can safely use a small stack
    // for the thread
    sigset_t sigset, old_sigset;
    sigfillset(&sigset);
    pthread_sigmask(SIG_BLOCK, &sigset, &old_sigset);

    pthread_attr_t th_attr;
    res = pthread_attr_init(&th_attr);
    if (res)
        return res;
    pthread_attr_setstacksize(&th_attr, PTHREAD_STACK_MIN);

    res = pthread_create(th, &th_attr, proc, arg);

    pthread_attr_destroy(&th_attr);
    pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);

    return res;
}

/*
 * Wait a process with a given timeout.
 * At the end of timeout (if trigger) kill the process.
 * To avoid race we need to wait a specific process, but this is blocking
 * and we use a timeout to implement the wait. Timer functions are per
 * process, not per thread.
 * Returns <0 if error, 0 if not timed out, >0 if timedout.
 */
static int
wait_process_timeout(pid_t pid, double timeout)
{
    int err;

    // compute deadline
    timeout_kill tm = { pid, false, false };
    if (clock_gettime(CLOCK_MONOTONIC, &tm.deadline) < 0)
        return -errno;

    double f = floor(timeout);
    tm.deadline.tv_sec += f;
    tm.deadline.tv_nsec += (timeout - f) * 1000000000.;
    if (tm.deadline.tv_nsec >= 1000000000) {
        tm.deadline.tv_nsec -= 1000000000;
        tm.deadline.tv_sec += 1;
    }

    pthread_condattr_t attr;
    err = pthread_condattr_init(&attr);
    if (err)
        return -err;
    err = pthread_condattr_setclock(&attr, CLOCK_MONOTONIC);
    if (!err)
        err = pthread_cond_init(&tm.cond, &attr);
    pthread_condattr_destroy(&attr);
    if (err)
        return -err;

    err = pthread_mutex_init(&tm.mtx, NULL);
    if (err) {
        pthread_cond_destroy(&tm.cond);
        return -err;
    }

    // Create timeout thread
    pthread_t th;
    err = create_thread_minstack(&th, thread_proc_timeout_kill, &tm);
    if (err) {
        pthread_cond_destroy(&tm.cond);
        pthread_mutex_destroy(&tm.mtx);
        return -err;
    }

    // Wait the process, we avoid to reap the other process to avoid
    // race conditions. Consider:
    // - process exit;
    // - we reap the thread;
    // - OS reuse the pid;
    // - timeout thread terminate the pid, now reused.
    // Avoiding reaping the process will create a zombie process so
    // the KILL would be directed to that.
    siginfo_t info;
    waitid(P_PID, pid, &info, WEXITED|WNOWAIT);

    // Close the timeout thread
    pthread_mutex_lock(&tm.mtx);
    // We use also a variable to avoid races like
    // - we create the thread;
    // - we start waiting the process which was already exited;
    // - we came here trying to close the thread;
    // - thread waits for signal.
    tm.stop = true;
    pthread_cond_broadcast(&tm.cond);
    pthread_mutex_unlock(&tm.mtx);
    pthread_join(th, NULL);

    // Cleanup
    pthread_cond_destroy(&tm.cond);
    pthread_mutex_destroy(&tm.mtx);

    return tm.timed_out ? 1 : 0;
}

// Similar to waitpid.
// Returns 0 on process still running, pid > 0 process
// exited, -1 error (on errno).
static pid_t
pidwaiter_wait(mapped_logs logs, pidwaiter *waiter, int *status, bool nohang)
{
    pid_t pid;

    pthread_mutex_lock(&waiter->mtx);
    if (waiter->reaped) {
        // if already reaped do not attempt to reap again to avoid
        // potentially reaping another child.
        log_fail("reaped was true");
        errno = ECHILD;
        pid = -1;
    } else if (nohang) {
        log("before waitpid");
        pid = waitpid(waiter->pid, status, WNOHANG);
        waiter->reaped = (pid > 0);
        if (pid == -1)
            mapped_logs_failure(logs);
        log("waitpid res %d reaped %d errno %d", (int) pid, waiter->reaped, errno);
    } else {
        log("before waitpid");

        // We are going always to wait and reap, release
        // mutex to avoid blocking other threads.
        waiter->reaped = true;
        pthread_mutex_unlock(&waiter->mtx);
        do {
            pid = waitpid(waiter->pid, status, 0);
        } while (pid == -1 && errno == EINTR);
        log("waitpid res %d errno %d", (int) pid, errno);
        return pid;
    }
    pthread_mutex_unlock(&waiter->mtx);
    return pid;
}

CAMLprim value
caml_pidwaiter_waitpid(value timeout_value, value waiter_value)
{
    CAMLparam1(waiter_value);
    double timeout = timeout_value == Val_none ? 0 : Double_val(Some_val(timeout_value));
    PIDWAITER(waiter, waiter_value);
    pid_t pid = waiter->pid;
    int status = 0;

    pthread_mutex_lock(&waiter->mtx);
    bool reaped = waiter->reaped;
    pthread_mutex_unlock(&waiter->mtx);

    mapped_logs logs = mapped_logs_open();
    log("waitpid pid %d reaped %d tid %d", (int) waiter->pid, reaped, (int) syscall(SYS_gettid));

    // already reaped, we cannot wait more
    if (reaped) {
        mapped_logs_failure(logs);
        mapped_logs_close(logs);
        unix_error(EINVAL, "pidwaiter_waitpid", Nothing);
    }

    caml_enter_blocking_section();

    bool timed_out = false;
    int err = 0;
    if (timeout > 0) {
        int res = wait_process_timeout(pid, timeout);
        log("after wait_process_timeout res %d", res);
        if (res < 0)
            err = -res;
        else if (res != 0)
            timed_out = true;
    }

    if (!err) {
        pid = pidwaiter_wait(logs, waiter, &status, false);
        if (pid == -1)
            err = errno;
    }

    caml_leave_blocking_section();

    if (timed_out)
        err = ETIMEDOUT;

    if (err) {
        log_fail("final res %d", err);
        mapped_logs_close(logs);
        unix_error(err, "waitpid", Nothing);
    }

    mapped_logs_success(logs);
    mapped_logs_close(logs);

    CAMLreturn(alloc_process_status(pid, status));
}

CAMLprim value
caml_pidwaiter_waitpid_nohang(value waiter_value)
{
    CAMLparam1(waiter_value);
    PIDWAITER(waiter, waiter_value);

    int status = 0;
    pid_t pid = pidwaiter_wait(NULL_MAPPED_LOGS, waiter, &status, true);

    if (pid == -1)
        unix_error(errno, "waitpid", Nothing);

    CAMLreturn(alloc_process_status(pid, status));
}

// function to build a "int * Unix.process_status", taken a bit
// from Ocaml source, a bit from dune sources.
static value
alloc_process_status(int pid, int status)
{
    CAMLextern int caml_rev_convert_signal_number(int);

    enum {
        TAG_WEXITED,
        TAG_WSIGNALED,
        TAG_WSTOPPED
    };

    CAMLparam0();
    CAMLlocal1(st);
    value res;
    int tag, val;

    // status is undefined when pid is zero so we set a default value.
    if (pid == 0) status = 0;

    if (WIFEXITED(status)) {
        tag = TAG_WEXITED;
        val = WEXITSTATUS(status);
    } else if (WIFSTOPPED(status)) {
        tag = TAG_WSTOPPED;
        val = caml_rev_convert_signal_number(WSTOPSIG(status));
    } else {
        tag = TAG_WSIGNALED;
        val = caml_rev_convert_signal_number(WTERMSIG(status));
    }
    st = caml_alloc_small(1, tag);
    Field(st, 0) = Val_int(val);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = Val_int(pid);
    Field(res, 1) = st;
    CAMLreturn(res);
}

// vim: expandtab ts=4 sw=4 sts=4:
