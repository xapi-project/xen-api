
+++
title = "Certificates and PEM Files"
+++

Xapi uses certificates for secure communication within a pool and with
external clients. These certificates are using the PEM file format and
reside in the Dom0 file system. This documents explains the purpose of
these files.

##  Design Documents

* [Pool Certificates](../../design/pool-certificates.md)
* [User Certificates](../../design/user-certificates.md)

## Paths

Below are paths used by Xapi for certificates; additional certficates
may be installed but they are not fundamental for Xapi's operation.

```
/etc/xensource/xapi-ssl.pem
/etc/xensource/xapi-pool-tls.pem
/etc/stunnel/certs-pool/1c111a1f-412e-47c0-9003-60789b839bc3.pem
/etc/stunnel/certs-pool/960abfff-6017-4d97-bd56-0a8f1a43e51a.pem
/etc/stunnel/xapi-stunnel-ca-bundle.pem
/etc/stunnel/certs/
/etc/stunnel/xapi-pool-ca-bundle.pem
```


## Fundamental Certificates

Certificates that identify a host. These certificates are comprised of
both a private and a public key. The public key may be distributed to
other hosts.

### xapi-ssl.pem

This certificate identifies a host for extra-pool clients.

This is the certificate used by the API HTTPS server that clients like
XenCenter or CVAD connect to. On installation of XenServer it is auto
generated but can be updated by a user using the API. This is the most
important certificate for a user to establish an HTTPS connection to a
pool or host to be used as an API.

* /etc/xensource/xapi-ssl.pem
* contains private and public key for this host
* `Host.get_server_certificate` API call
* referenced by /etc/stunnel/xapi.conf
* `xe host-server-certificate-install` XE command to replace the
  certificate.
* See below for xapi-stunnel-ca-bundle for additional certificates that
  can be added to a pool in support of a user-supplied host certificate.
* `xe host-reset-server-certificate` creates a new self-signed certificate.


### `xapi-pool-tls.pem`

This certificate identifies a host inside a pool. It is auto generated
and used for all intra-pool HTTPS connections. It needs to be
distributed inside a pool to establish trust. The distribution of the
public part of the certificate is performed by the API and must not be
done manually.

* /etc/xensource/xapi-pool-tls.pem
* contains private and public key for this host
* referenced by /etc/stunnel/xapi.conf
* This certificate can be re-generated using the API or XE
* `Host.refresh_server_certificate`
* `xe host-refresh-server-certificate`

## Certificate Bundles

Certifiacte bundles are used by stunnel. They are a collection of public
keys from hosts and certificates provided by a user. Knowing a host's
public key facilitates stunnel connecting to the host.

Bundles by themselves are a technicality as they organise a set of
certificates in a single file but don't add new certificates.

### `xapi-pool-ca-bundle.pem` and `certs-pool/*.pem`

Collection of public keys from xapi-pool-tls.pem across the
pool. The public keys are collected in the certs-pool directory: each is
named after the UUID of its host and the bundle is constructed from
them.

* bundle of public keys from hosts' `xapi-pool-tls.pem`
* constructed from PEM files in `certs-pool/`
* `/opt/xensource/bin/update-ca-bundle.sh` generates the bundle from PEM
  files

### `xapi-stunnel-ca-bundle.pem` and `certs/*.pem`

User-supplied certificates; they are not essential for the operation of
a pool from Xapi's perspective. They make stunnel aware of certificates
used by clients when using HTTPS for API calls.

* in a plain pool installation, these are empty; PEMs supplied by a user
  are stored here and bundled into the `xapi-stunnerl-ca-bundle.pem`.
* bundle of public keys supploed by a user
* constructed from PEM files in `certs/`
* `/opt/xensource/bin/update-ca-bundle.sh` generates the bundle from PEM files
* Updated by a user using `xe pool-install-ca-certificate`
* `Pool.install_ca_certificate`
* `Pool.uninstall_ca_certificate`
* `xe pool-certificate-sync` explicitly distribute these certificates in
  the pool.
* User-provided certificates can be used to let xapi connect to WLB.
