/*
 * Copyright (C) Cloud Software Group
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
#define _GNU_SOURCE

#include <pthread.h>

int set_name(const char *name){
    pthread_t thread;

    thread = pthread_self();

    return pthread_setname_np(thread, name);
  }

int get_name(char* thread_name, size_t len){
  pthread_t thread;

  thread = pthread_self();

  return pthread_getname_np(thread, thread_name, len);
}
