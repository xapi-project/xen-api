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

// Definitions to write logs into memory mapped objects.
// We use a memory mapped object here because we close file descriptors
// so writing to file using them would cause logs to be lost.

#pragma once

#if !defined(FORKEXECD_DEBUG_LOGS)
#define FORKEXECD_DEBUG_LOGS 0
#endif

#if (FORKEXECD_DEBUG_LOGS) != 0 && (FORKEXECD_DEBUG_LOGS) != 1
#error Expected FORKEXECD_DEBUG_LOGS to be defined either 0 or 1
#endif

typedef struct priv_mapped_logs priv_mapped_logs;
typedef struct mapped_logs mapped_logs;

#if FORKEXECD_DEBUG_LOGS
struct mapped_logs {
    priv_mapped_logs *priv;
};
#define NULL_MAPPED_LOGS ((mapped_logs){0})
mapped_logs mapped_logs_open(void);
void mapped_logs_close(mapped_logs logs);

// Add a log entry, similar to printf.
void mapped_logs_add(mapped_logs logs, const char *fmt, ...);

// Mark as failed, any failure will keep the log.
void mapped_logs_failure(mapped_logs logs);

// Mark as successful, if successful and no failure during
// execution the log will be removed.
void mapped_logs_success(mapped_logs logs);
#else
// Use an empty structure, compiler will strip it passing
// it as a parameter without the needs to change the source
// code.
struct mapped_logs {};
#define NULL_MAPPED_LOGS ((mapped_logs){})
static inline mapped_logs mapped_logs_open(void) {
    return (mapped_logs){};
}

static inline void mapped_logs_close(mapped_logs logs) {
}

static inline void mapped_logs_failure(mapped_logs logs) {
}

static inline void mapped_logs_success(mapped_logs logs) {
}

#define mapped_logs_add(...) \
    do {} while(0)
#endif
