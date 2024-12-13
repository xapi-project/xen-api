/*
 * Copyright (C) 2024 Cloud Software Group
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

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/custom.h>
#include <caml/unixsupport.h>

#include <stdbool.h>
#include <errno.h>
#include <pthread.h>

typedef struct delay {
	pthread_mutex_t mtx;
	pthread_cond_t cond;
	bool signaled;
} delay;

// Initialize delay
// Returns error number or 0 if success
static int delay_init(delay *d)
{
	int err;
	pthread_condattr_t cond_attr;

	d->signaled = false;

	err = pthread_condattr_init(&cond_attr);
	if (err)
		goto err0;
	err = pthread_condattr_setclock(&cond_attr, CLOCK_MONOTONIC);
	if (!err)
		err = pthread_cond_init(&d->cond, &cond_attr);
	if (err)
		goto err1;
	err = pthread_mutex_init(&d->mtx, NULL);
	if (err)
		goto err2;
	pthread_condattr_destroy(&cond_attr);
	return 0;

err2:
	pthread_cond_destroy(&d->cond);
err1:
	pthread_condattr_destroy(&cond_attr);
err0:
	return err;
}

static void delay_destroy(delay *d)
{
	pthread_cond_destroy(&d->cond);
	pthread_mutex_destroy(&d->mtx);
}

static void delay_signal(delay *d)
{
	// there are quite some chances lock is not held
	if (pthread_mutex_trylock(&d->mtx) == 0) {
		d->signaled = true;
		pthread_cond_signal(&d->cond);
		pthread_mutex_unlock(&d->mtx);
		return;
	}

	// slow way, release engine
	caml_release_runtime_system();
	pthread_mutex_lock(&d->mtx);
	d->signaled = true;
	pthread_cond_signal(&d->cond);
	pthread_mutex_unlock(&d->mtx);
	caml_acquire_runtime_system();
}

// Wait for deadline or signal.
// Returns error number or 0 if success.
// Error can be ETIMEDOUT.
int delay_wait(delay *d, const struct timespec *deadline)
{
	int err;

	caml_release_runtime_system();
	pthread_mutex_lock(&d->mtx);
	do {
		if (d->signaled) {
			d->signaled = false;
			err = 0;
			break;
		}
		err = pthread_cond_timedwait(&d->cond, &d->mtx, deadline);
	} while (err == 0);
	pthread_mutex_unlock(&d->mtx);
	caml_acquire_runtime_system();
	return err;
}

#define delay_val(v) (*((delay **)Data_custom_val(v)))

static void delay_finalize(value v_delay)
{
	delay *d = delay_val(v_delay);
	delay_destroy(d);
	caml_stat_free(d);
}

static struct custom_operations delay_ops = {
	"xapi.delay",
	delay_finalize,
	custom_compare_default,
	custom_hash_default,
	custom_serialize_default,
	custom_deserialize_default,
	custom_compare_ext_default,
	custom_fixed_length_default
};

CAMLprim value caml_xapi_delay_create(value v_unit)
{
	CAMLparam1(v_unit);
	CAMLlocal1(res);
	delay *d;
	int err;

	d = caml_stat_alloc(sizeof(*d));
	err = delay_init(d);
	if (err) {
		caml_stat_free(d);
		unix_error(err, "caml_delay_create", Nothing);
	}
	res = caml_alloc_custom(&delay_ops, sizeof(delay *), 0, 1);
	delay_val(res) = d;
	CAMLreturn(res);
}

CAMLprim value caml_xapi_delay_signal(value v_delay)
{
	CAMLparam1(v_delay);
	delay *d = delay_val(v_delay);
	delay_signal(d);
	CAMLreturn(Val_unit);
}

CAMLprim value caml_xapi_delay_wait(value v_delay, value v_deadline)
{
	CAMLparam2(v_delay, v_deadline);
	delay *d = delay_val(v_delay);
	uint64_t deadline = (uint64_t) Int64_val(v_deadline);
	struct timespec ts = {
		deadline / 1000000000u,
		deadline % 1000000000u
	};

	int err = delay_wait(d, &ts);
	if (err != 0 && err != ETIMEDOUT)
		unix_error(err, "caml_delay_wait", Nothing);

	CAMLreturn(err ? Val_true : Val_false);
}
