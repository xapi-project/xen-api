
#include <sys/epoll.h>
#include <errno.h>
#include <sys/resource.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

/* Make all constants available to clients by exporting them via a
 * function. This avoids having to hard code them on the client side. 
 * */

#define CONSTANT(name,i) \
  CAMLprim value name(value unit) { return Val_int(i); }

CONSTANT(caml_polly_EPOLLIN, EPOLLIN);
CONSTANT(caml_polly_EPOLLPRI, EPOLLPRI);
CONSTANT(caml_polly_EPOLLOUT, EPOLLOUT);
CONSTANT(caml_polly_EPOLLRDNORM, EPOLLRDNORM);
CONSTANT(caml_polly_EPOLLRDBAND, EPOLLRDBAND);
CONSTANT(caml_polly_EPOLLWRNORM, EPOLLWRNORM);
CONSTANT(caml_polly_EPOLLWRBAND, EPOLLWRBAND);
CONSTANT(caml_polly_EPOLLMSG, EPOLLMSG);
CONSTANT(caml_polly_EPOLLERR, EPOLLERR);
CONSTANT(caml_polly_EPOLLHUP, EPOLLHUP);
CONSTANT(caml_polly_EPOLLRDHUP, EPOLLRDHUP);
CONSTANT(caml_polly_EPOLLWAKEUP, EPOLLWAKEUP);
CONSTANT(caml_polly_EPOLLONESHOT, EPOLLONESHOT);
CONSTANT(caml_polly_EPOLLET, EPOLLET);

#if 0
CONSTANT(caml_polly_EPOLLEXCLUSIVE, EPOLLEXCLUSIVE);
#endif

CAMLprim value caml_polly_create1(value val_unit)
{
	CAMLparam1(val_unit);
	CAMLlocal1(val_res);
	int fd;

	if ((fd = epoll_create1(0)) == -1)
		uerror("epoll_create1", Nothing);

	val_res = Val_int(fd);

	CAMLreturn(val_res);
}

static value
caml_polly_ctl(value val_epfd, value val_fd, value val_events, int op)
{
	CAMLparam3(val_epfd, val_fd, val_events);
	struct epoll_event event = {
		.events = (uint32_t) Int_val(val_events),
		.data.fd = Int_val(val_fd)
	};

	if (epoll_ctl(Int_val(val_epfd), op, Int_val(val_fd), &event) == -1)
		uerror("epoll_ctl", Nothing);

	CAMLreturn(Val_unit);
}

CAMLprim value caml_polly_add(value val_epfd, value val_fd, value val_events)
{
	return caml_polly_ctl(val_epfd, val_fd, val_events, EPOLL_CTL_ADD);
}

CAMLprim value caml_polly_mod(value val_epfd, value val_fd, value val_events)
{
	return caml_polly_ctl(val_epfd, val_fd, val_events, EPOLL_CTL_MOD);
}

CAMLprim value caml_polly_del(value val_epfd, value val_fd, value val_events)
{
	return caml_polly_ctl(val_epfd, val_fd, val_events, EPOLL_CTL_DEL);
}

CAMLprim value
caml_polly_wait(value val_epfd, value val_max, value val_timeout, value val_f)
{
	CAMLparam4(val_epfd, val_max, val_timeout, val_f);
	CAMLlocal1(ignore);

	struct epoll_event events[Int_val(val_max)];
	int ready, i;

	if (Int_val(val_max) <= 0)
		uerror("epoll_wait", Nothing);

	caml_enter_blocking_section();
	ready = epoll_wait(Int_val(val_epfd), events, Int_val(val_max),
			   Int_val(val_timeout));
	caml_leave_blocking_section();

	if (ready == -1)
		uerror("epoll_wait", Nothing);

	for (i = 0; i < ready; i++) {
		ignore = caml_callback3(val_f,
					val_epfd,
					Val_int(events[i].data.fd),
					Val_int(events[i].events));
	}

	CAMLreturn(Val_int(ready));
}

/* vim: set ts=8 noet: */
