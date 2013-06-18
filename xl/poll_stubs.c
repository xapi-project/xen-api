#include <stdio.h>
#include <poll.h>
#include <errno.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>

/*
short Poll_val(value event);
short Poll_events_val(value event_list);
value Val_poll(short event);
value add_event(value event_list, short event);
*/
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

/*
short Poll_val(value event)
{
	CAMLparam1(event);
	short res = -1;

	switch (Int_val(event)) {
		case 0: res = POLLIN; break;
		case 1: res = POLLPRI; break;
		case 2: res = POLLOUT; break;
		case 3: res = POLLERR; break;
		case 4: res = POLLHUP; break;
		case 5: res = POLLNVAL; break;
	}

	CAMLreturn(res);
}

short Poll_events_val(value event_list)
{
	CAMLparam1(event_list);
	short events = 0;

	while (event_list != Val_emptylist) {
		events |= Poll_val(Field(event_list, 0));
		event_list = Field(event_list, 1);
	}

	CAMLreturn(events);
}

value Val_poll(short event)
{
	CAMLparam0();
	CAMLlocal1(res);

	switch (event) {
		case POLLIN: res = Val_int(0); break;
		case POLLPRI: res = Val_int(1); break;
		case POLLOUT: res = Val_int(2); break;
		case POLLERR: res = Val_int(3); break;
		case POLLHUP: res = Val_int(4); break;
		case POLLNVAL: res = Val_int(5); break;
		default:
			caml_raise_with_string(*caml_named_value("poll exception"), "unknown poll event value");
			break;
	}

	CAMLreturn(res);
}

value add_event(value event_list, short event)
{
	CAMLparam1(event_list);
	CAMLlocal1(new_list);

	new_list = caml_alloc(2, 0);
	Store_field(new_list, 0, Val_poll(event));
	Store_field(new_list, 1, event_list);

	CAMLreturn(new_list);
}

value Val_poll_events(short events)
{
	CAMLparam0();
	CAMLlocal1(event_list);

	event_list = Val_emptylist;
	if (events & POLLIN)
		event_list = add_event(event_list, POLLIN);
	if (events & POLLPRI)
		event_list = add_event(event_list, POLLPRI);
	if (events & POLLOUT)
		event_list = add_event(event_list, POLLOUT);
	if (events & POLLERR)
		event_list = add_event(event_list, POLLERR);
	if (events & POLLHUP)
		event_list = add_event(event_list, POLLHUP);
	if (events & POLLNVAL)
		event_list = add_event(event_list, POLLNVAL);

	CAMLreturn(event_list);
}
*/

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

