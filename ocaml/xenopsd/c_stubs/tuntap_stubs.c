/*
 * (c) Citrix
 *
 * This could be replaced by https://github.com/mirage/ocaml-tuntap
 * if more features are required.
 */

#include <string.h>
#include <unistd.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <linux/if_tun.h>
#include <fcntl.h>
#include <errno.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#define PATH_NET_TUN "/dev/net/tun"

/* open /dev/net/tun and set it up
 * external _tap_open : string -> Unix.file_descr = "stub_tap_open"
 */
CAMLprim value stub_tap_open(value ocaml_ifname)
{
    CAMLparam1(ocaml_ifname);
    CAMLlocal1(path_net_tun);
    unsigned int features;
    struct ifreq ifr;
    const char *ifname = String_val(ocaml_ifname);

    memset(&ifr, 0, sizeof(ifr));

    size_t len = strlen(ifname);
    if (len == 0) {
        caml_failwith("empty string argument in " __FILE__);
    }
    if (len >= IFNAMSIZ) {
        caml_failwith("string argument too long in "__FILE__);
    }
    strncpy(ifr.ifr_name, ifname, IFNAMSIZ);

    path_net_tun = caml_copy_string(PATH_NET_TUN);
    int fd = open(PATH_NET_TUN, O_RDWR);
    if (fd < 0) {
        uerror("open", path_net_tun);
    }

    if (ioctl(fd, TUNGETFEATURES, &features) == -1) {
        int saved_errno = errno;
        close(fd);
        unix_error(saved_errno, "ioctl/TUNGETFEATURES", path_net_tun);
    }

    ifr.ifr_flags = IFF_TAP | IFF_NO_PI | (features & IFF_ONE_QUEUE);
    if (ioctl(fd, TUNSETIFF, (void *) &ifr) != 0) {
        int saved_errno = errno;
        close(fd);
        unix_error(saved_errno,"ioctl/TUNSETIFF", path_net_tun);
    }

    fcntl(fd, F_SETFL, O_NONBLOCK);
    CAMLreturn(Val_int(fd));
}

