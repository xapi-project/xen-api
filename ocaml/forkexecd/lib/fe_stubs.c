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
#include <sys/wait.h>

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>

#include "../helper/vfork_helper.h"

#define FOREACH_LIST(name, list) \
    for(value name = (list); name != Val_emptylist; name = Field(name, 1))

// Create thread reducing stack usage to a minimum to reduce memory usage.
// Returns error number (like pthread_create).
static int create_thread_minstack(pthread_t *th, void *(*proc)(void *), void *arg);

static inline void
reap_pid(pid_t pid)
{
    int status;
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR)
        continue;
}

static void *
thread_proc_reap(void *arg)
{
    pid_t pid = (pid_t) (intptr_t) arg;

    reap_pid(pid);

    return NULL;
}

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

static char**
copy_string_list(value list)
{
    size_t strings_size = 0;
    size_t list_size = 0;
    char **res, **ptrs;
    char *strings;

    FOREACH_LIST(item, list) {
        strings_size += strlen(String_val(Field(item, 0))) + 1;
        ++list_size;
    }

    res = (char **) malloc(sizeof(char*) * (list_size + 1) + strings_size);
    if (!res)
        return NULL;

    ptrs = res;
    strings = (char *) (res + (list_size + 1));
    FOREACH_LIST(item, list)
        *ptrs++ = append_string(&strings, String_val(Field(item, 0)));
    *ptrs = NULL;

    return res;
}

static void
close_fd(int *const p_fd)
{
    const int fd = *p_fd;
    if (fd >= 0) {
        *p_fd = -1;
        close(fd);
    }
}

typedef struct {
    const char *err_msg;
    pid_t pid;
    msg_t msg;
} safe_exec_result;

static int
safe_exec_with_helper(safe_exec_result *res, char **args, char **envs)
{
    int err = EINVAL;
    char fd_string[48];
    int pipe_fds[2] = { -1, -1 };

    res->err_msg = "safe_exec";

    if (!args[0] || !args[1] || !args[2])
        return EINVAL;

    if (strcmp(args[1], "-e") == 0) {
        if (pipe(pipe_fds) < 0) {
            res->err_msg = "pipe";
            return errno;
        }
        sprintf(fd_string, "%d", pipe_fds[1]);
        args[2] = fd_string;
    }

    sigset_t sigset, old_sigset;
    int cancellation_state;

    // Disable cancellation to avoid some signals.
    // Glibc use some signals to handle thread cancellation.
    pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancellation_state);

    // Block all possible signals to avoid receiving some in the child.
    // Signal mask is inherited to new process/thread will start with
    // all signals disabled and we can safely change them.
    sigfillset(&sigset);
    pthread_sigmask(SIG_BLOCK, &sigset, &old_sigset);

    // fork
    err = 0;
    res->pid = vfork();
    if (res->pid < 0) {
        err = errno;
    } else if (res->pid == 0) {
        // child
        if (pipe_fds[0] >= 0)
            close(pipe_fds[0]);
        execve(args[0], args, envs);
        // keep compatibility with forkexecd daemon.
        _exit(errno == ENOENT ? 127 : 126);
    }

    // Restore thread state
    pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);
    pthread_setcancelstate(cancellation_state, NULL);

    // We don't need writing pipe anymore and we need to detect
    // if closed so we can't keep it open
    close_fd(&pipe_fds[1]);

    if (err != 0) {
        close_fd(&pipe_fds[0]);
        res->err_msg = "vfork";
        return err;
    }

    // Handle errors from helper
    if (pipe_fds[0] >= 0) {
        int readed;
        // Note that buffer is small and written atomically by
        // the helper, no reason for the kernel to split it.
        while ((readed = read(pipe_fds[0], &res->msg, sizeof(res->msg))) < 0
               && errno == EINTR)
            continue;
        close_fd(&pipe_fds[0]);
        if (readed != 0 && readed < offsetof(msg_t, msg_buf) + 1) {
            // This should never happen !!!
            // At this point the process is created and we have a pid so
            // we cannot just return an error.
            // We could try to wait the process but it should fail, let
            // returns success and let caller read process status result.
            return 0;
        }
        res->msg.msg_buf[sizeof(res->msg.msg_buf) - 1] = 0;
        if (readed > 0) {
            // Wait the process otherwise we'll have a zombie
            reap_pid(res->pid);

            res->err_msg = res->msg.msg_buf;
            return res->msg.err;
        }
    }
    return 0;
}

CAMLprim value
caml_safe_exec_with_helper(value args, value environment)
{
    CAMLparam2(args, environment);

    // Copy parameters to C
    char **c_args = copy_string_list(args);
    char **c_envs = copy_string_list(environment);
    if (!c_envs || !c_args) {
        free(c_envs);
        free(c_args);
        caml_raise_out_of_memory();
    }

    // potentially slow section, release Ocaml engine
    caml_enter_blocking_section();

    safe_exec_result res;
    int err = safe_exec_with_helper(&res, c_args, c_envs);

    free(c_envs);
    free(c_args);

    caml_leave_blocking_section();

    // error, notify with an exception
    if (err != 0)
        unix_error(err, res.err_msg, Nothing);

    CAMLreturn(Val_int(res.pid));
}

CAMLprim value
caml_pidwaiter_dontwait(value pid_val)
{
    CAMLparam1(pid_val);
    pid_t pid = Int_val(pid_val);

    // reap the pid to avoid zombies
    pthread_t th;
    if (create_thread_minstack(&th, thread_proc_reap, (void *) (intptr_t) pid) == 0)
        pthread_detach(th);

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
    int res;

    do {
        pthread_mutex_lock(&tm->mtx);
        res  = tm->stop ? 0:
            pthread_cond_timedwait(&tm->cond, &tm->mtx, &tm->deadline);
        pthread_mutex_unlock(&tm->mtx);

        if (res == ETIMEDOUT) {
            kill(tm->pid, SIGKILL);
            tm->timed_out = true;
            break;
        }
        // handle spurious wakeups
    } while (!tm->stop && res == 0);
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
    if (!res) {
        pthread_attr_setstacksize(&th_attr, PTHREAD_STACK_MIN);

        res = pthread_create(th, &th_attr, proc, arg);

        pthread_attr_destroy(&th_attr);
    }
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
    err = 0;
    while (waitid(P_PID, pid, &info, WEXITED|WNOWAIT) == -1) {
        if (errno != EINTR) {
            err = -errno;
            break;
        }
    }

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

    return err ? err : (tm.timed_out ? 1 : 0);
}

CAMLprim value
caml_pidwaiter_waitpid(value timeout_value, value pid_value)
{
    CAMLparam0();
    double timeout = timeout_value == Val_none ? 0 : Double_val(Some_val(timeout_value));
    pid_t pid = Int_val(pid_value);

    caml_enter_blocking_section();

    bool timed_out = false;
    int err = 0;
    if (timeout > 0) {
        int res = wait_process_timeout(pid, timeout);
        if (res < 0)
            err = -res;
        else if (res != 0)
            timed_out = true;
    }

    caml_leave_blocking_section();

    if (err)
        unix_error(err, "waitpid", Nothing);

    CAMLreturn(timed_out ? Val_true: Val_false);
}
