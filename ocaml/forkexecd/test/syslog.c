#define _GNU_SOURCE
#define _DEFAULT_SOURCE
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <alloca.h>
#include <unistd.h>
#include <dlfcn.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#define START(name) \
	static typeof(name) *old_func = NULL; \
	if (!old_func) \
		old_func = (typeof(name) *) dlsym(RTLD_NEXT, #name);

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
	static const char dev_log[] = "/dev/log";
	START(connect);

	struct sockaddr_un *un = (struct sockaddr_un *) addr;
	if (!addr || addr->sa_family != AF_UNIX
	    || memcmp(un->sun_path, dev_log, sizeof(dev_log)) != 0)
		return old_func(sockfd, addr, addrlen);

	struct sockaddr_un new_addr;
	new_addr.sun_family = AF_UNIX;
	strcpy(new_addr.sun_path, "/tmp/xyz");
	return old_func(sockfd, (struct sockaddr *) &new_addr, sizeof(new_addr));
}

static const char *month_name(int month)
{
	static const char names[12][4] = {
		"Jan",
		"Feb",
		"Mar",
		"Apr",
		"May",
		"Jun",
		"Jul",
		"Aug",
		"Sep",
		"Oct",
		"Nov",
		"Dec",
	};
	if (month >= 0 && month < 12)
		return names[month];

	return "Xxx";
}

static void vsyslog_internal(int priority, const char *format, va_list ap)
{
	// format is "<13>Jul  9 07:19:01 hostname: message"
	time_t now = time(NULL);
	struct tm tm, *p;
	p = gmtime_r(&now, &tm);

	if (LOG_FAC(priority) == 0)
		priority |= LOG_USER;

	char buffer[1024];
	char *buf = buffer;
	const int prefix_len = sprintf(buffer, "<%d> %s % 2d %02d:%02d:%02d %s: ", priority, month_name(p->tm_mon),
		p->tm_mday, p->tm_hour, p->tm_min, p->tm_sec, "dummy");

	int left = (int) sizeof(buffer) - prefix_len;
	int l = vsnprintf(buffer + prefix_len, left, format, ap);
	if (l >= left) {
		buf = malloc(prefix_len + l + 1);
		if (!buf)
			return;
		memcpy(buf, buffer, prefix_len);
		l = vsnprintf(buf + prefix_len, l + 1, format, ap);
	}

	int sock = socket(AF_UNIX, SOCK_DGRAM | SOCK_CLOEXEC, 0);
	if (sock >= 0) {
		struct sockaddr_un addr;
		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "/tmp/xyz");
		sendto(sock, buf, prefix_len + l, MSG_NOSIGNAL, &addr, sizeof(addr));

		close(sock);
	}
	if (buf != buffer)
		free(buf);
}

void syslog(int priority, const char *format, ...)
{
	va_list ap;
	va_start(ap, format);
	vsyslog_internal(priority, format, ap);
	va_end(ap);
}

void vsyslog(int priority, const char *format, va_list ap)
{
	vsyslog_internal(priority, format, ap);
}

void __syslog_chk(int priority, int flags, const char *format, ...)
{
	va_list ap;
	va_start(ap, format);
	vsyslog_internal(priority, format, ap);
	va_end(ap);
}

void __vsyslog_chk(int priority, int flags, const char *format, va_list ap)
{
	vsyslog_internal(priority, format, ap);
}
