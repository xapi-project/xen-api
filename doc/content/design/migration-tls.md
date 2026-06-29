---
title: Migration Stream Encryption with kernel TLS (kTLS)
layout: default
design_doc: true
revision: 1
status: proposed
---
# Migration Stream Encryption with kernel TLS (kTLS)

VM-migrate sends the guest memory over a TLS connection so the data is
encrypted on the wire. Today that TLS is provided by stunnel, an external
process. This document describes an alternative that keeps the same TLS
security but removes stunnel from the sender's data path, using the kernel's
own TLS (kTLS), and so makes vm-migrate and host-evacuate faster.

## Existing transport: stunnel

stunnel is an external process that terminates TLS for the xenguest migration
stream. As it is a separate process from xenguest, the dom0 kernel has to pipe
the plaintext between the two, so every byte of guest RAM is copied through an
extra userspace hop and encrypted in userspace. That wastes dom0 cpu and memory
throughput, so the result is a slower vm-migrate, and therefore a slower
host-evacuate for the user. The slowdown is worst exactly when dom0 cpu is the
bottleneck.

stunnel cannot be removed by linking it into xenguest, as it is not a library:
it is a configuration wrapper around OpenSSL.

## Solution: kTLS on the sender

The data pipe between xenguest and stunnel disappears if the kernel does the
bulk TLS encryption in place, on the same socket xenguest already writes to.
Linux 6.6 supports this through kTLS: once the symmetric key is installed on a
socket with `setsockopt(SOL_TLS, ...)`, the kernel encrypts/decrypts every
subsequent `read`/`write` transparently, using AES-NI, producing the same
byte stream stunnel produces today.

Phase 1 changes only the sender host and already provides the speed-up (see
Performance), while proving the kTLS sender interoperates with the unchanged
stunnel receiver on the destination. Benefits:

* significant speed-up: host-evacuate is around 1.5x faster for 10 parallel
  VMs, for less dom0 cpu (see Performance).
* xenguest is unmodified: no libssl in xenguest, so no extra xen-devel
  upstreaming, maintenance or security reviews on xenguest/libxenguest.
* same TLS security as stunnel: a small `xenopsd-tls-helper` does the OpenSSL
  handshake with the same key size, cipher, certificate and verification stunnel
  uses, then asks the kernel to take over the bulk encryption.
* backwards-compatible & conservative: stunnel stays the default; kTLS is an
  option, introduced gradually until it is proven in the field.

## Principles

* P1. kernel data pipes between userspace processes add latency and cut data
  throughput in dom0.
* P2. keep the existing security guarantees.
* P3. backwards-compatibility.
* P4. guarded new features.

## Use cases

* U1. Admin configuration:
    * U1.1. a new `migration_ktls` option selects the faster datapath for
      vm-migrate and host-evacuate (P1, P2).
    * U1.2. the option can be reverted to the original stunnel datapath (P3).
* U2. Admin usage when the option is selected:
    * U2.1. vm-migrate is faster, or at least the same (P1).
    * U2.2. host-evacuate is faster, or at least the same (P1).
    * U2.3. vm-migrate and host-evacuate still work between hosts configured
      with different options (P3).
* U3. Over time the option may graduate to become the default for vm-migrate
  and host-evacuate (P4).

## Requirements

* R1. Admin configuration:
    * R1.1.1. Host-level: `xe-enable-experimental-feature migration_ktls` sets
      `/etc/xenserver/features.d/migration_ktls` to 1 on the host, enabling the
      option there.
    * R1.1.2. Pool-level: when every host has the entry, the matching
      `restrict_migration_ktls` shown by `xe pool-list params=restrictions`
      reads `false`.
    * R1.1.3. Pool-level (future): a helper `xe pool-experimental-feature-set
      name=migration_ktls` sets the entry on each host of the pool.
* R2. Admin usage:
    * R2.1. vm-migrate: when the option is enabled, xenopsd uses
      `xenopsd-tls-helper` instead of stunnel to set up kTLS transport for
      xenguest (see Design).
    * R2.2. host-evacuate: the vm-migrate operations it drives inherit R2.1.
* R3. Future: once kTLS is the default, a XenAPI field
  `pool.migration_transport` could expose `ktls` (default) or `stunnel`.

Implementation note: R1.1.1 describes the intended activation. The current
implementation does not yet read `features.d/migration_ktls`; it selects the
transport per host from `xenopsd.conf` instead:

```
migration-tls = "ktls"      # use the helper
migration-tls = "stunnel"   # explicit default
migration-tls = ""          # currently defaults to "stunnel"
```

Wiring `xe-enable-experimental-feature migration_ktls` to this `xenopsd.conf`
option (so the host flag drives R1.1.1/R1.1.2) is outstanding.

## Design

### Considered designs

The goal is to remove the plaintext data pipe between stunnel and xenguest.

| Design | Summary | Analysis |
| --- | --- | --- |
| A: TLS inside xenguest/libxenguest | link libssl into xenguest; xenguest does `SSL_connect`/`SSL_accept` and `SSL_read`/`SSL_write` directly. Needs an SNI dispatcher (eg. sniproxy) on the receiver to share port 443. | Removes the pipe, but embeds TLS in xenguest: lots of changes, lots of xen-devel upstreaming, and ongoing security-maintenance on xenguest/libxenguest. Too expensive to create and maintain. |
| B: xenguest as a stunnel SNI backend | use stunnel's SNI dispatch to route migration traffic to xenguest. | Easy (stunnel config only), but the plaintext pipe between stunnel and the backend remains, so it defeats the goal. |
| C: kTLS via a small TLS helper | `xenopsd-tls-helper` does the OpenSSL handshake, asks OpenSSL to enable kTLS on the socket, and hands the kTLS socket fd to xenopsd over SCM_RIGHTS. xenguest then sees only plaintext; the kernel encrypts transparently. | xenguest is unmodified, stunnel leaves the sender data path, the helper reuses stunnel's OpenSSL config/keys/certificates, and kTLS can later be offloaded to hardware NICs. |

Conclusion: design C (kTLS) seems superior, as it reaches the goal with fewer
changes, is toolstack-only (no xen-devel upstream loop, no future xenguest
security maintenance), is a small focused tool rather than a change to the
highly-complex xenguest, leaves the stunnel option in place, and opens the way
to kTLS hardware offload later.

### kTLS sender

Performance: the kernel encrypts the migration stream in the xenguest context,
so dom0 no longer copies the whole guest RAM between xenguest and stunnel. The
effect is largest where dom0 cpu, not the wire, is the bottleneck.

Security: the helper uses OpenSSL exactly as stunnel does, so the bulk kTLS
stream is byte-identical to stunnel's and is accepted by the unchanged
destination stunnel.

* same handshake: TLS 1.2, cipher `ECDHE-RSA-AES256-GCM-SHA384`, ECDHE group
  `secp384r1`, renegotiation disabled, authenticated with `SSL_VERIFY_PEER`
  against the destination's pool-internal certificate (CN = host uuid,
  SNI = `pool`).
* single source of truth: the helper's verification is derived from the same
  configuration the stunnel fallback would use, so `--ca`/`--sni` come from it
  and `--no-verify` is sent only when verification is disabled pool-wide. SNI is
  always sent, so the destination serves its pool-internal certificate either
  way.
* the TLS 1.2 pin is necessary for kTLS, as a TLS 1.3 post-handshake
  KeyUpdate cannot be carried by the kernel kTLS data path.
* no new security code in xenguest: after the handshake OpenSSL installs the
  symmetric key in the kernel, and everything xenguest writes as plaintext is
  encrypted transparently.
* the helper hands the socket over only after confirming the kernel actually
  enabled kTLS in both directions (`BIO_get_ktls_send` and `BIO_get_ktls_recv`),
  as the migration channel is bidirectional; otherwise it errors and the sender
  falls back to stunnel.

Backwards-compatibility: the new behaviour is hidden behind the per-host option,
and a kTLS sender works with a stunnel receiver, so the worst case is exactly
today's behaviour.

### The helper

`xenopsd-tls-helper` does only what stunnel cannot, then gets out of the way:

1. perform the TLS handshake with OpenSSL (same protocol/cipher/cert/verification
   as stunnel).
2. ask OpenSSL to enable kTLS on the socket.
3. confirm the kernel took over send and receive (`BIO_get_ktls_send`/`_recv`);
   if not, error so the sender falls back to stunnel.
4. hand the now-encrypting socket to xenopsd over SCM_RIGHTS, then exit.

The helper is not a proxy and is not in the data path: it is a short-lived "set
up the encrypted socket, hand it over, get out of the way" step. xenopsd brokers
the fd to xenguest just as it brokers the stunnel fd today, and xenguest writes
the migration stream as plaintext while the kernel encrypts each `write` in
place.

### Why stunnel cannot remove the data pipe

stunnel is a general-purpose TLS proxy, and a proxy must receive plaintext on
one side to encrypt it on the other, so the xenguest -> stunnel plaintext hop is
structural and cannot be removed while stunnel is in the path. stunnel has no
pass-through mode and cannot hand its encrypted socket to another process, so its
bulk encryption stays in userspace.

An alternative considered but rejected was to make stunnel use kTLS and then pull
its kernel-encrypting socket out of the stunnel process with `pidfd_getfd(2)`.
This has lots of issues: it needs ptrace-level privilege over stunnel and racy
fd-table scraping; there is no clean ownership handoff, as the TLS sequence
number and any partial record are per-socket and two writers corrupt the stream;
and nobody owns the TLS control path, so an inbound TLS 1.3 KeyUpdate stalls the
receive side. The safe form of "hand a kTLS socket to the datapath" is precisely
the dedicated helper above.

## Performance

Single host-evacuate, 10 VMs migrating in parallel, sender kTLS vs sender
stunnel, same host pair. The improvement grows with guest load, as a busier
guest has more memory to transmit and pushes dom0 cpu harder:

| Guest load | stunnel | kTLS | Improvement |
| --- | --- | --- | --- |
| idle | 185s | 163s | 1.13x |
| medium (windows apps) | 249s | 171s | 1.46x |
| high (synthetic page thrasher) | 1341s | 835s | 1.60x |

host-evacuate improvement is around 1.5x for 10 parallel VMs, for
lower dom0 cpu (eg. on the medium load, mean dom0 cpu drops from ~0.73 to ~0.53
of the 16 dom0 vCPUs). The improvement is smaller for idle guests, as dom0 is
then not the bottleneck.

## Implementation

The sender path is in xenopsd. Outside the helper itself, the change is small:

* `ocaml/xenopsd/tools/xenopsd-tls-helper/` — the standalone C helper, with its
  Makefile and README. It is built on its own (`make`) and deployed to
  `/usr/libexec/xenopsd/xenopsd-tls-helper`.
* `ocaml/xenopsd/lib/migrate_connect.ml` — a drop-in replacement for
  `Open_uri.with_open_uri` that spawns the helper when `migration-tls = "ktls"`
  and falls back to `Open_uri` otherwise.
* `ocaml/xenopsd/lib/xenops_server.ml` — the three migration fd call sites
  (vm, mem, vgpu) in the `VM_migrate` branch go through `Migrate_connect`.
* `ocaml/xenopsd/lib/xenopsd.ml`, `ocaml/xenopsd/xc/xc_resources.ml`,
  `ocaml/xenopsd/xenopsd.conf` — register the `migration-tls` option and the
  `xenopsd-tls-helper` resource, and document them.

Fallback: the sender falls back to stunnel only when the kTLS path fails to
produce the fd (helper spawn, TLS handshake, kTLS install or SCM_RIGHTS). It logs
a single warn line and uses stunnel for that one connection, so the migration
still proceeds. Once the fd is handed to the migration, any later exception is a
migration-layer failure and propagates unchanged, as silently retrying it over a
fresh stunnel socket would re-enter the in-progress receive on the destination
and corrupt its state.

## Upgrade

The kTLS sender produces the same TLS stream as stunnel and the receiver is
unchanged, so a kTLS-enabled host migrates to a stunnel host with no
coordination. xapi only permits migration from older to newer toolstacks, so a
newer kTLS sender is never required by an older receiver. The option is off by
default, so an upgrade changes nothing until an admin enables it.

## Outlook

* Phase 2: a kTLS receiver, so the destination also drops stunnel from the data
  path. It needs an SNI dispatcher (eg. sniproxy) to share port 443 between the
  receiver and the existing stunnel endpoint.
* kTLS hardware-offload NICs could move the bulk encryption off the cpu and make
  the transfer faster still.
* `pool.migration_transport` (R3) once kTLS becomes the default.
* mutual TLS (client-certificate auth) and destination-hostname pinning exceed
  the stunnel migration client and can be done with the Phase 2 receiver work.

