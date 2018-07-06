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


#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>

#define PATH_NET_TUN "/dev/net/tun"

/* open /dev/net/tun and set it up
 * external _tap_open : string -> Unix.file_descr = "stub_tap_open"
 */
CAMLprim value stub_tap_open(value ocaml_ifname)
{
    CAMLparam1(ocaml_ifname);
    unsigned int features;
    struct ifreq ifr;
    char *ifname = String_val(ocaml_ifname);

    memset(&ifr, 0, sizeof(ifr));

    size_t len = strlen(ifname);
    if (len == 0) {
        caml_failwith("empty string argument in " __FILE__);
    }
    if (len >= IFNAMSIZ) {
        caml_failwith("string argument too long in "__FILE__);
    }
    strncpy(ifr.ifr_name, ifname, IFNAMSIZ);

    int fd = open(PATH_NET_TUN, O_RDWR);
    if (fd < 0) {
        caml_failwith("open(" PATH_NET_TUN ") failed in " __FILE__);
    }

    if (ioctl(fd, TUNGETFEATURES, &features) == -1) {
        close(fd);
        caml_failwith("TUNGETFEATURES failed in " __FILE__);
    }

    ifr.ifr_flags = IFF_TAP | IFF_NO_PI | (features & IFF_ONE_QUEUE);
    if (ioctl(fd, TUNSETIFF, (void *) &ifr) != 0) {
        close(fd);
        caml_failwith("ioctl failed in " __FILE__);
    }

    fcntl(fd, F_SETFL, O_NONBLOCK);
    CAMLreturn(Val_int(fd));
}

