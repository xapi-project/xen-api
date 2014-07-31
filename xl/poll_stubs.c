#include <stdio.h>
#include <poll.h>
#include <errno.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>

/* From the OCaml Libxl bindings in Xen */
short Poll_events_val(value event_list);
value Val_poll_events(short events);

static int list_len(value v)
{
	int len = 0;
	while ( v != Val_emptylist ) {
		len++;
		v = Field(v, 1);
	}
	return len;
}

value stub_poll(value fds, value timeout)
{
	CAMLparam2(fds, timeout);
	CAMLlocal3(fd, tmp, result);
	int rc, i;
	const int c_nfds = list_len(fds);
	struct pollfd c_fds[c_nfds];
	int c_timeout = Int_val(timeout);

	for (i = 0; fds != Val_emptylist; i++) {
		fd = Field(fds, 0);
		c_fds[i].fd = Int_val(Field(fd, 0));
		c_fds[i].events = Poll_events_val(Field(fd, 1));
		c_fds[i].revents = 0;
		fds = Field(fds, 1);
	}

	caml_enter_blocking_section();
	rc = poll(c_fds, c_nfds, c_timeout);
	caml_leave_blocking_section();

	for (i = c_nfds - 1; i >= 0; i--) {
		tmp = caml_alloc(2, 0);
		Store_field(tmp, 0, Val_poll_events(c_fds[i].revents));
		Store_field(tmp, 1, fds);
		fds = tmp;
	}

	result = caml_alloc(2, 0);
	Store_field(result, 0, fds);
	Store_field(result, 1, Val_int(rc));
	CAMLreturn(result);
}
