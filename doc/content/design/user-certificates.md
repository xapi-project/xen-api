---
title: User-installable host certificates
layout: default
design_doc: true
revision: 2
status: released (8.2)
---

## Introduction

It is often necessary to replace the TLS certificate used to secure
communications to Xenservers hosts, for example to allow a XenAPI user such as
Citrix Virtual Apps and Desktops (CVAD) to validate that the host is genuine
and not impersonating the actual host.

Historically there has not been a supported mechanism to do this, and as a
result users have had to rely on guides written by third parties that show how
to manually replace the xapi-ssl.pem file on a host. This process is
error-prone, and if a mistake is made, can result in an unusable system.
This design provides a fully supported mechanism to allow replacing the
certificates.

## Design proposal

It is expected that an API caller will provide, in a single API call, a private
key, and one or more certificates for use on the host. The key will be provided
in PKCS #8 format, and the certificates in X509 format, both in
base-64-encoded PEM containers.

Multiple certificates can be provided to cater for the case where an
intermediate certificate or certificates are required for the caller to be able
to verify the certificate back to a trusted root (best practice for Certificate
Authorities is to have an 'offline' root, and issue certificates from an
intermediate Certificate Authority). In this situation, it is expected (and
common practice among other tools) that the first certificate provided in the
chain is the host's unique server certificate, and subsequent certificates form
the chain.

To detect mistakes a user may make, certain checks will be carried out on the
provided key and certificate(s) before they are used on the host. If all checks
pass, the key and certificate(s) will be written to the host, at which stage a
signal will be sent to stunnel that will cause it to start serving the new
certificate.

## Certificate Installation

### API Additions

Xapi must provide an API call through Host RPC API to install host
certificates:

```ocaml
let install_server_certificate = call
    ~lifecycle:[Published, rel_stockholm, ""]
    ~name:"install_server_certificate"
    ~doc:"Install the TLS server certificate."
    ~versioned_params:
      [{ param_type=Ref _host; param_name="host"; param_doc="The host"
       ; param_release=stockholm_release; param_default=None}
      ;{ param_type=String; param_name="certificate"
       ; param_doc="The server certificate, in PEM form"
       ; param_release=stockholm_release; param_default=None}
      ;{ param_type=String; param_name="private_key"
       ; param_doc="The unencrypted private key used to sign the certificate, \
                    in PKCS#8 form"
       ; param_release=stockholm_release; param_default=None}
      ;{ param_type=String; param_name="certificate_chain"
       ; param_doc="The certificate chain, in PEM form"
       ; param_release=stockholm_release; param_default=Some (VString "")}
      ]
    ~allowed_roles:_R_POOL_ADMIN
    ()
```

This call should be implemented within xapi, using the already-existing crypto
libraries available to it.

Analogous to the API call, a new CLI call `host-server-certificate-install`
must be introduced, which takes the parameters `certificate`, `key` and
`certificate-chain` - these parameters are expected to be filenames, from which
the key and certificate(s) must be read, and passed to the
`install_server_certificate` RPC call.

The CLI will be defined as:
```ocaml
"host-server-certificate-install",
{
  reqd=["certificate"; "private-key"];
  optn=["certificate-chain"];
  help="Install a server TLS certificate on a host";
  implementation=With_fd Cli_operations.host_install_server_certificate;
  flags=[ Host_selectors ];
};
```

### Validation

Xapi must perform the following validation steps on the provided key and
certificate. If any validation step fails, the API call must return an error
with the specified error code, providing any associated text:

### Private Key

* Validate that it is a pem-encoded PKCS#8 key, use error
`SERVER_CERTIFICATE_KEY_INVALID []` and exposed as
"The provided key is not in a pem-encoded PKCS#8 format."

* Validate that the algorithm of the key is RSA, use error
`SERVER_CERTIFICATE_KEY_ALGORITHM_NOT_SUPPORTED, [<algorithms's ASN.1 OID>]`
and exposed as "The provided key uses an unsupported algorithm."

* Validate that the key length is ≥ 2048, and ≤ 4096 bits, use error
`SERVER_CERTIFICATE_KEY_RSA_LENGTH_NOT_SUPPORTED, [length]` and exposed as
"The provided RSA key does not have a length between 2048 and 4096."

* The library used does not support multi-prime RSA keys, when it's
encountered use error `SERVER_CERTIFICATE_KEY_RSA_MULTI_NOT_SUPPORTED []` and
exposed as "The provided RSA key is using more than 2 primes, expecting only
2"

#### Server Certificate
* Validate that it is a pem-encoded X509 certificate, use error
`SERVER_CERTIFICATE_INVALID []` and exposed as "The provided certificate is not
in a pem-encoded X509."

* Validate that the public key of the certificate matches the public key from
the private key, using error `SERVER_CERTIFICATE_KEY_MISMATCH []` and exposing
it as "The provided key does not match the provided certificate's public key."

* Validate that the certificate is currently valid. (ensure all time
comparisons are done using UTC, and any times presented in errors are using
ISO8601 format):

  * Ensure the certificate's `not_before` date is ≤ NOW
  `SERVER_CERTIFICATE_NOT_VALID_YET, [<NOW>; <not_before>]` and exposed as
  "The provided certificate certificate is not valid yet."

  * Ensure the certificate's `not_after` date is > NOW
  `SERVER_CERTIFICATE_EXPIRED, [<NOW>; <not_after>]` and exposed as "The
  provided certificate has expired."

* Validate that the certificate signature algorithm is SHA-256
`SERVER_CERTIFICATE_SIGNATURE_NOT_SUPPORTED []` and exposed as
"The provided certificate is not using the SHA256 (SHA2) signature algorithm."

#### Intermediate Certificates
* Validate that it is an X509 certificate, use
`SERVER_CERTIFICATE_CHAIN_INVALID []` and exposed as "The provided
intermediate certificates are not in a pem-encoded X509."

### Filesystem Interaction

If validation has been completed successfully, a temporary file must be created
with permissions 0x400 containing the key and certificate(s), in that order,
separated by an empty line.

This file must then be atomically moved to /etc/xensource/xapi-ssl.pem in
order to ensure the integrity of the contents. This may be done using rename
with the origin and destination in the same mount-point.

## Alerting

A daily task must be added. This task must check the expiry date of the first
certificate present in /etc/xensource/xapi-ssl.pem, and if it is within 30
days of expiry, generate a `message` to alert the administrator that the
certificate is due to expire shortly.

The body of the message should contain:
```
<body>
  <message>
    The TLS server certificate is expiring soon
  </message>
  <date>
    <expiry date in ISO8601 'YYYY-MM-DDThh:mm:ssZ' format>`
  </date>
</body>

```

The priority of the message should be based on the number of days to expiry as
follows:

| Number of days | Priority |
| -------------- | -------- |
| 0-7 | 1 |
| 8-14 | 2 |
| 14+ | 3 |

The other fields of the message should be:

| Field | Value |
| ----- | ----- |
| name | HOST_SERVER_CERTIFICATE_EXPIRING |
| class | Host |
| obj-uuid | < Host UUID > |

Any existing `HOST_SERVER_CERTIFICATE_EXPIRING` messages with this host's UUID
should be removed to avoid a build-up of messages.

Additionally, the task may also produce messages for expired server
certificates which must use the name `HOST_SERVER_CERTIFICATE_EXPIRED`.
This kind of message must contain the message "The TLS server certificate has
expired." as well as the expiry date, like the expiring messages.
They also may replace the existing expiring messages in a host.

## Expose Certificate metadata

Currently xapi exposes a CLI command to print the certificate being used to
verify external hosts. We would like to also expose through the API and the
CLI useful metadata about the certificates in use by each host.

The new class is meant to cover server certificates and trusted certificates.

### Schema

A new class, Certificate, will be added with the following schema:

| Field      | Type      | Notes |
| ---------- | --------- | ----- |
| uuid       |           |
| type       | CA        | Certificate trusted by all hosts
|            | Host      | Certificate that the host present sto normal clients
| name       | String    | Name, only present for trusted certificates
| host       | Ref _host | Host where the certificate is installed
| not_before | DateTime  | Date after which the certificate is valid
| not_after  | DateTime  | Date before which the certificate is valid
| fingerprint_sha256 | String    | The certificate's SHA256 fingerprint / hash
| fingerprint_sha1   | String    | The certificate's SHA1 fingerprint / hash

### CLI / API

There are currently-existing CLI parameters for certificates:
`pool-certificate-{install,uninstall,list,sync}`,
`pool-crl-{install,uninstall,list}` and `host-get-server-certificate`.

The new command must show the metadata of installed server certificates in
the pool.
It must be able to show all of them in the same call, and be able to filter
the certificates per-host.

To make it easy to separate it from the previous calls and to reflect that
certificates are a class type in xapi the call will be named `certificate-list`
and it will accept the parameter `host-uuid=<uuid>`.

## Recovery mechanism

In the case a certificate is let to expire TLS clients connecting to the host
will refuse to establish the connection.
This means that the host is going to be unable to be managed using the xapi
API (Xencenter, or a CVAD control plane)

There needs to be a mechanism to recover from this situation.
A CLI command must be provided to install a self-signed certificate, in the
same way it is generated during the setup process at the moment.
The command will be `host-emergency-reset-server-certificate`.
This command is never to be forwarded to another host and will call openssl to
create a new RSA private key

The command must notify stunnel to make sure stunnel uses the newly-created
certificate.

# Miscellaneous

The auto-generated `xapi-ssl.pem` currently contains Diffie-Hellman (DH)
Parameters, specifically 512 bits worth. We no longer support any ciphers which
require DH parameters, so these are no longer needed, and it is acceptable for
them to be lost as part of installing a new certificate/key pair.

The generation should also be modified to avoid creating these for new
installations.
