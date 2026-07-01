# xenopsd-tls-helper

Sender-side kTLS helper for VM live migration. It replaces the per-migration
stunnel client subprocess on the sender: it performs the TLS handshake in
userspace, installs the symmetric keys into the kernel via kTLS, and hands the
kTLS-enabled socket fd back to xenopsd via `SCM_RIGHTS`.

Once xenopsd holds the fd, every subsequent `read()`/`write()` is
decrypted/encrypted by the kernel on the calling thread — no stunnel pipe, no
extra context switches, no extra copies.

## Enabling

Set the following in `/etc/xenopsd.conf` (or `/etc/xenopsd.conf.d/migration.conf`)
and restart xenopsd:

```
migration-tls = ktls
```

If the option is absent or set to `stunnel` (the default), xenopsd uses the
existing stunnel-based path. If the helper fails for any reason (binary missing,
TLS handshake error, kTLS install rejected by the kernel), xenopsd logs a `warn`
and transparently retries that single connection over stunnel, so the migration
still succeeds over TLS.

## Invocation contract

```
xenopsd-tls-helper \
    --host <dest-host> \
    --port <dest-port> \
    --send-fd <N> \
    [--ca <ca-bundle.pem> | --no-verify] \
    [--sni <name>]
```

- `--send-fd N` is the integer fd of a Unix-domain socket inherited from
  xenopsd. After a successful handshake the helper confirms kTLS is installed in
  both directions, then writes a single byte plus the kTLS-enabled socket fd
  onto `N` via `SCM_RIGHTS` and exits 0 (the byte is a required carrier for the
  `SCM_RIGHTS` fd; its value is ignored). On any failure it writes a single line
  to stderr and exits 1.
- `--ca` is the CA bundle used to verify the destination; `--no-verify` skips
  verification (used when pool certificate verification is disabled).
