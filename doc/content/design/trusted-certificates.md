---
title: Trusted certificates for identity validation in TLS connections
layout: default
design_doc: true
revision: 1
status: draft
---

# Overview

In various use cases, TLS connections are established on the host on which XAPI runs.
When establishing a TLS connection, the peer identity needs to be validated.
This is done using either a root CA certificate to perform certificate chain validation, or a known peer certificate for validation with certificate pinning.
The root CA certificates and peer certificates involved in this process are referred to as trusted certificates.
When a trusted certificate is installed, the local endpoint can validate the peer identity during TLS connection establishment.
Certificate chain validation is a general-purpose, standards-based approach but requires additional steps, such as getting the peer's certificate signed by a CA.
In contrast, certificate pinning offers a quicker way to set up trust in some cases without the overhead of CA signing.
For example, when establishing a TLS connection, the peer endpoint presenting a self-signed server certificate, the local endpoint, after explicit user confirmation, can set up the trust by pinning the server certificate for this peer.
For subsequent connections, the local endpoint validates the peer against the pinned certificate.
This allows the use case to start in quicker and easier way without prior CA signing and without compromising security.

As the unified API for the whole system, XAPI also exposes interfaces for users to install and manage trusted certificates that are used by system components for different purposes.

The base design described in [pool-certificates.md](https://github.com/minglumlu/xen-api/blob/5d1ea1520825d502c57a90a02db476cd7d6a9132/doc/content/design/pool-certificates.md) defines the database, API, and trust store in the filesystem for managing trusted certificates.
This document introduces the following enhancements to that design:

* Explicit separation of root CA certificates and peer certificates:
In the base design, both certificate types share the same database schema, APIs, and are stored together in a single bundle file.
This makes it difficult to determine the appropriate validation approach based on the certificate type.
The improvement introduces a type value to separate root CA certificates and peer certificates explicitly.

* Add a "purpose" attribute for trusted certificates:
According to the base design, only certificates used for internal TLS connections among XAPI processes within a pool are stored separately.
All other trusted certificates are grouped in a single bundle, which may include certificates for multiple purposes.
By introducing a "purpose" attribute, certificates can be organized by their intended use, improving clarity and reducing ambiguity.

# Use Cases
* An XAPI client establishes a TLS connection to an XAPI service.
This case is outside the scope of trusted certificates managed by XAPI and is included here only for completeness.
* An XAPI process on one host initiates a TLS connection to an XAPI process on another host within the same pool.
This case is covered in the base design and is listed here for completeness.
* An XAPI process initiates a TLS connection to an external service, such as an appliance.
This case benefits from the improvements introduced in this design.
* A non-XAPI process (like licensing agent) running on a host managed by XAPI initiates a TLS connection to an external service, such as a License Server.
This case benefits from the improvements introduced in this design as well.


# Changes
## Database schema
The *Certificate* class in database is defined to represent general certificates, including trusted certificates.
One existing class field "type" supports the following enumeration values:
* "ca": trusted certificates including both root CA and peer.
* "host": identity certificate of a host for communication with entities outside the pool.
* "host_internal": identity certificate of a host for communication with other pool members.

Two improvements in this design:
* A new value "peer" is introduced in this design so that the existing "ca" now represents trusted root CA only.
The new "peer" will represent trusted peer certificates.

* A new enumeration type "purpose" is introduced to indicate the intended usage of a trusted certificate.
A new *Certificate* class field "purpose" (a set of values of enumeration type "purpose") will be added to represent all applicable purposes of a trusted certificate.
By default, this set is empty which corresponds to the existing "ca" certificates for general purpose.

## API

### pool.install_ca_certificate

This is an existing API to install a trusted certificate into the pool with its arguments being defined as:
* session (ref session_id): reference to a valid session;
* name (string): the name of the certificate;
* cert (string): the certificate in PEM format.

Prior to this design, the API's name parameter represents the certificate file name as persisted on the dom0 file system.
In this design, this API will be deprecated because it exposes implementation details that should remain internal and hidden from users.
The new "pool.install_trusted_certificate" should be used instead.
For the same reason, "pool.uninstall_ca_certificate" will also be deprecated.

### pool.install_trusted_certificate
This is a new API introduced in this design with its arguments being defined as:
* session (ref session_id): reference to a valid session;
* self (ref Pool): reference to the pool;
* ca (boolean): the trusted certificate is a root CA certificate used to verify a chain (true), or a peer certificate used for certificate pinning (false);
* cert (string): the trusted certificate in PEM format;
* purpose (string list): the purposes of the trusted certificate.

This new API is used to install trusted certificate.
When *purpose* is an empty set, it stands for a root CA certificate for general purpose.
The *purpose* can not be an empty set when the *ca* is false, because each peer certificate is specific to a single server and therefore unsuitable for a shared trusted certificate for general purpose.

It returns *void* when succeed. Otherwise, return corresponding API error.

### pool.uninstall_trusted_certificate
This is a new API introduced in this design to uninstall a trusted certificate with its arguments being defined as:
* session (ref session_id): reference to a valid session;
* certificate (ref Certificate): reference to the trusted certificate;
* force (bool): remove the database entry even if the file doesn't exist.

It returns *void* when succeed. Otherwise, return corresponding API error.

### pool.join
Prior to this design, trusted certificates are exchanged between the pool and the joining host during the pre‑join phase.
This design preserves that behavior to ensure the joiner works correctly both before and after joining the pool.

### pool.eject
The trusted certificates will be removed from any host which is being eject from the pool.

### Other APIs of managing trusted certificates
The install/uninstall APIs above are not the only ways of managing the trusted certificates.
A particular API, e.g. "pool.set_wlb_url", may also install the trusted certificate used to validate the WLB server on subsequent TLS connections.
However, regardless of the entry point, all trusted certificates must be represented by a *Certificate* database object and stored in the same way described below as if installed by the install APIs.

## Trust store
The trusted certificates are stored in individual hosts' filesystems.
The existing stores defined in the base design are:
| Name | Filesystem location | User-configurable | Used for |
| ---- | ------------------- | ----------------- | -------- |
| Trusted Default | /etc/stunnel/certs/              | yes (using API) | Certificates that users can install for trusting appliances
| Trusted Pool    | /etc/stunnel/certs-pool/         | no              | Certificates that are managed by the pool for host-to-host communications
| Default Bundle  | /etc/stunnel/xapi-stunnel-ca-bundle.pem | no       | Bundle of certificates that hosts use to verify appliances (in particular WLB), this is kept in sync with "Trusted Default"
| Pool Bundle     | /etc/stunnel/xapi-pool-ca-bundle.pem    | no       | Bundle of certificates that hosts use to verify other hosts on pool communications, this is kept in sync with "Trusted Pool"

Regarding the "User-configurable", when it is "yes", it means a user can only install and remove the file with "name" parameter of "pool.install_ca_certificate" ; when it is "no", it means the user can't install or remove it even via APIs. In any cases, a user can't change the certificate files directly.

When a trusted certificate is being installed via "pool.install_ca_certificate", the trusted certificate will be stored in the "Trusted Default" and "Default Bundle".
This design doesn't change this for backwards compatibility. But the API "pool.install_ca_certificate" will be marked as *deprecated*.

The pool "Trusted Pool" and "Pool Bundle" are for host-to-host TLS communications within a pool. This design doesn't change them.

The stores for the certificates installed via "pool.install_trusted_certificate" are defined as:
| Name | Filesystem location | Used for |
| ---- | ------------------- | -------- |
| Trusted General CA | /etc/trusted-certs/ca-general/     | Trusted root CA certificates that users can install to validate a peer’s identity when establishing a TLS connection for general purpose.
| Trusted Peer | /etc/trusted-certs/peer-\<PURPOSE\>/     | Trusted peer certificates that users can install to validate a peer’s identity when establishing a TLS connection for \<PURPOSE\>.
| Trusted CA | /etc/trusted-certs/ca-\<PURPOSE\>/         | Trusted root CA certificates that users can install to validate a peer’s identity when establishing a TLS connection for \<PURPOSE\>.
| General Bundle  | /etc/trusted-certs/ca-bundle-general.pem    | Bundle of trusted root CA certificates under /etc/trusted-certs/ca-general/ to verify a peer's identity when establishing a TLS connection for general purpose.
| Peer Bundle  | /etc/trusted-certs/peer-bundle-\<PURPOSE\>.pem | Bundle of trusted peer certificates under /etc/trusted-certs/peer-\<PURPOSE\>/ to verify a peer's identity when establishing a TLS connection for \<PURPOSE\>.
| CA Bundle     | /etc/trusted-certs/ca-bundle-\<PURPOSE\>.pem  | Bundle of trusted root CA certificates under /etc/trusted-certs/ca-\<PURPOSE\>/ to verify a peer's identity when establishing a TLS connection for \<PURPOSE\>.

The filesystem location is derived from the \<PURPOSE\>. Each \<PURPOSE\> string corresponds to a predefined value of the "purpose" type in the database, implemented as predefined constants.
The certificate file names under filesystem locations of "Trusted General CA", "Trusted Peer" and "Trusted CA" will be the UUIDs of the *Certificate* objects.

## Precedence order of choosing trust stores
The "Peer Bundle", "CA Bundle", and "Default Bundle" can be directly used when establishing TLS connections.
The endpoint to validate the peer's identity must unambiguously choose only one non-empty bundle from them with the following precedence order:
1. "Peer Bundle"
2. "CA Bundle"
3. "General Bundle"

No more attempts on remaining bundles when validation with the selected one fails (the server certificate is not trusted by the selected bundle).

For example, if "Peer Bundle" doesn't exist and the "CA Bundle" (if not empty) is selected to do the validation, the endpoint should not try with "General Bundle" even when the validation with "CA Bundle" failed.
