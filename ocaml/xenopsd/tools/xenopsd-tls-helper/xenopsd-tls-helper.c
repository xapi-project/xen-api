/*
 * xenopsd-tls-helper — sender-side kTLS helper for VM live migration.
 *
 * Steps:
 *   1. open a TCP socket to the destination host:port
 *   2. TLS handshake using the pool CA bundle (matching the stunnel migration
 *      client)
 *   3. ask OpenSSL to install kTLS (SSL_OP_ENABLE_KTLS) and confirm the kernel
 *      installed kTLS for BOTH directions (TX and RX) — the migration channel
 *      is bidirectional
 *   4. hand the kTLS-enabled socket fd back to xenopsd via SCM_RIGHTS on a
 *      unix-socket fd inherited from the parent
 *   5. exit; xenopsd then read()s/write()s plaintext and the kernel
 *      decrypts/encrypts on the fly
 *
 * Build: see the dune file / Makefile. Installed at
 * /usr/libexec/xenopsd/xenopsd-tls-helper.
 *
 * Invocation:
 *   xenopsd-tls-helper --host <h> --port <p> --send-fd <N>
 *                      [--ca <pem> | --no-verify] [--sni <name>]
 *
 * Exit codes:
 *   0  success (kTLS active in both directions, fd handed off)
 *   1  any failure (TLS handshake, kTLS install, SCM_RIGHTS send, ...)
 *      stderr carries a single human-readable line for xenopsd to log.
 */

#define _GNU_SOURCE
#include <errno.h>
#include <getopt.h>
#include <limits.h>
#include <netdb.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>

#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>

static void die(const char *fmt, ...) __attribute__((noreturn, format(printf, 1, 2)));

static void die(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    fputs("xenopsd-tls-helper: ", stderr);
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
    va_end(ap);
    exit(1);
}

static const char *ossl_err(void) {
    unsigned long e = ERR_peek_last_error();
    return e ? ERR_reason_error_string(e) : "(no OpenSSL error)";
}

static int send_fd(int sock, int fd) {
    char dummy = 'x';
    struct iovec io = { .iov_base = &dummy, .iov_len = 1 };
    union {
        struct cmsghdr cm;
        char buf[CMSG_SPACE(sizeof(int))];
    } u;
    memset(&u, 0, sizeof(u));
    struct msghdr msg = {0};
    msg.msg_iov = &io;
    msg.msg_iovlen = 1;
    msg.msg_control = u.buf;
    msg.msg_controllen = sizeof(u.buf);
    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    cmsg->cmsg_len = CMSG_LEN(sizeof(int));
    memcpy(CMSG_DATA(cmsg), &fd, sizeof(int));
    ssize_t n;
    do {
        n = sendmsg(sock, &msg, 0);
    } while (n < 0 && errno == EINTR);
    return (int) n;
}

static int tcp_connect(const char *host, const char *port) {
    struct addrinfo hints = {0};
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    struct addrinfo *res = NULL;
    int rc = getaddrinfo(host, port, &hints, &res);
    if (rc != 0)
        die("getaddrinfo(%s, %s): %s", host, port, gai_strerror(rc));
    int fd = -1;
    int last_errno = 0;
    for (struct addrinfo *ai = res; ai; ai = ai->ai_next) {
        fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
        if (fd < 0) { last_errno = errno; continue; }
        if (connect(fd, ai->ai_addr, ai->ai_addrlen) == 0) break;
        last_errno = errno;
        close(fd);
        fd = -1;
    }
    freeaddrinfo(res);
    if (fd < 0)
        die("connect %s:%s: %s", host, port, strerror(last_errno));
    int one = 1;
    (void) setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
    (void) setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, &one, sizeof(one));
    return fd;
}

int main(int argc, char **argv) {
    const char *host = NULL;
    const char *port = NULL;
    const char *ca = NULL;
    const char *sni = NULL;
    int send_fd_num = -1;
    int no_verify = 0;

    static struct option long_opts[] = {
        {"host",                required_argument, 0, 'h'},
        {"port",                required_argument, 0, 'p'},
        {"ca",                  required_argument, 0, 'a'},
        {"sni",                 required_argument, 0, 's'},
        {"send-fd",             required_argument, 0, 'f'},
        {"no-verify",           no_argument,       0, 'n'},
        {0, 0, 0, 0}
    };
    int idx;
    int c;
    while ((c = getopt_long(argc, argv, "", long_opts, &idx)) != -1) {
        switch (c) {
            case 'h': host = optarg; break;
            case 'p': port = optarg; break;
            case 'a': ca = optarg; break;
            case 's': sni = optarg; break;
            case 'n': no_verify = 1; break;
            case 'f': {
                /* --send-fd is a Forkhelpers placeholder rewritten to an
                   integer fd before exec; validate it strictly anyway. */
                char *end = NULL;
                errno = 0;
                long v = strtol(optarg, &end, 10);
                if (optarg[0] == '\0' || *end != '\0' || errno != 0
                    || v < 0 || v > INT_MAX)
                    die("invalid --send-fd value: %s", optarg);
                send_fd_num = (int) v;
                break;
            }
            default:
                die("usage: %s --host H --port P --send-fd N "
                    "[--ca PEM | --no-verify] [--sni NAME]", argv[0]);
        }
    }
    if (!host || !port || send_fd_num < 0)
        die("missing required argument; need --host --port --send-fd");
    if (!no_verify && !ca)
        die("--ca is required unless --no-verify is given");

    int fd = tcp_connect(host, port);

    const SSL_METHOD *method = TLS_client_method();
    SSL_CTX *ctx = SSL_CTX_new(method);
    if (!ctx)
        die("SSL_CTX_new: %s", ossl_err());

    /* Pin TLSv1.2 and the AES-GCM cipher list to match stunnel.ml's migration
       client config so the destination negotiates one of these. The TLS1.2 pin
       is load-bearing for kTLS, not merely stunnel parity: it forecloses
       TLS1.3 post-handshake KeyUpdate, which the kernel kTLS data path cannot
       process once OpenSSL has handed off the socket. */
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    SSL_CTX_set_max_proto_version(ctx, TLS1_2_VERSION);
    /* Mirrors xapi's Constants.good_ciphersuites (kept as a literal to avoid a
       build-time dependency from the C helper on the OCaml constant). */
    if (SSL_CTX_set_cipher_list(
            ctx,
            "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES128-GCM-SHA256") != 1)
        die("SSL_CTX_set_cipher_list: %s", ossl_err());
    /* Match stunnel.ml's ECDHE group pin (curve = secp384r1). */
    if (SSL_CTX_set1_groups_list(ctx, "secp384r1") != 1)
        die("SSL_CTX_set1_groups_list: %s", ossl_err());

    /* Request kTLS BEFORE the handshake; also forbid renegotiation (defence in
       depth — the AES-GCM TLS1.2 suites do not renegotiate, and the kernel
       could not process a post-handoff renegotiation anyway). If kTLS install
       fails along the way the handshake still succeeds but the BIO_get_ktls_*
       checks below return 0. */
    SSL_CTX_set_options(ctx, SSL_OP_ENABLE_KTLS | SSL_OP_NO_RENEGOTIATION);

    if (no_verify) {
        SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);
    } else {
        if (SSL_CTX_load_verify_locations(ctx, ca, NULL) != 1)
            die("SSL_CTX_load_verify_locations(%s): %s", ca, ossl_err());
        SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
    }

    SSL *ssl = SSL_new(ctx);
    if (!ssl)
        die("SSL_new: %s", ossl_err());

    /* BIO_NOCLOSE: SSL_free must not close the underlying fd, since we are
       handing it to xenopsd. */
    BIO *bio = BIO_new_socket(fd, BIO_NOCLOSE);
    if (!bio)
        die("BIO_new_socket: %s", ossl_err());
    SSL_set_bio(ssl, bio, bio);

    if (sni && SSL_set_tlsext_host_name(ssl, sni) != 1)
        die("SSL_set_tlsext_host_name(%s): %s", sni, ossl_err());

    ERR_clear_error();
    if (SSL_connect(ssl) != 1)
        die("SSL_connect: %s", ossl_err());

    /* SSL_VERIFY_PEER (set above) already makes SSL_connect fail on a bad
       certificate, so in normal operation this re-check never fires. It is a
       defence-in-depth guard against a future change that drops or weakens
       that flag; if it ever did fire it reports the specific failure reason. */
    if (!no_verify) {
        long vr = SSL_get_verify_result(ssl);
        if (vr != X509_V_OK)
            die("certificate verification failed: %s",
                X509_verify_cert_error_string(vr));
    }

    /* No-plaintext invariant: confirm the kernel installed kTLS in BOTH
       directions BEFORE handing the fd to xenopsd. The migration channel is
       bidirectional (the sender both writes the memory image and reads
       handshake replies on this fd), so TX-only kTLS is not sufficient. These
       gates MUST stay strictly after SSL_connect and strictly before send_fd
       below — a refactor must not reorder them, or plaintext could be sent. */
    if (!BIO_get_ktls_send(SSL_get_wbio(ssl)))
        die("kTLS TX not installed (tls.ko missing or cipher rejected by "
            "kernel); negotiated cipher = %s", SSL_get_cipher_name(ssl));
    if (!BIO_get_ktls_recv(SSL_get_rbio(ssl)))
        die("kTLS RX not installed (kernel lacks TLS_RX for the negotiated "
            "cipher); negotiated cipher = %s", SSL_get_cipher_name(ssl));

    if (send_fd(send_fd_num, fd) < 0)
        die("sendmsg(SCM_RIGHTS) on fd %d: %s", send_fd_num, strerror(errno));

    /* Do NOT SSL_shutdown — it would send close_notify down the socket that
       xenopsd is about to use. SSL_free is safe (BIO_NOCLOSE). The kTLS state
       lives in the kernel socket, so userspace teardown here is purely local;
       end-of-stream integrity is enforced by the migration layer. */
    SSL_free(ssl);
    SSL_CTX_free(ctx);

    /* Closing our fd is fine: the kernel keeps the socket alive while xenopsd
       holds the duplicated fd received via SCM_RIGHTS. */
    close(fd);
    close(send_fd_num);
    return 0;
}
