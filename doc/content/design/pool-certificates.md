---
title: TLS vertification for intra-pool communications
layout: default
design_doc: true
revision: 2
status: released (22.6.0)
---

## Overview

Xenserver has used TLS-encrypted communications between xapi daemons in a pool since its first release.
However it does not use TLS certificates to authenticate the servers it connects to.
This allows possible attackers opportunities to impersonate servers when the pools’ management network is compromised.

In order to enable certificate verification, certificate exchange as well as proper set up to trust them must be provided by xapi.
This is currently done by allowing users to generate, sign and install the certificates themselves; and then enable the Common Criteria mode.
This requires a CA and has a high barrier of entry.

Using the same certificates for intra-host communication creates friction between what the user needs and what the host needs.
Instead of trying to reconcile these two uses with one set of certificates, host will serve two certificates: one for API calls from external clients, which is the one that can be changed by the users; and one that is use for intra-pool communications.
The TLS server in the host can select which certificate to serve depending on the service name the client requests when opening a TLS connection.
This mechanism is called Server Name Identification or SNI in short.

Last but not least the update bearing these changes must not disrupt pool operations while or after being applied.

## Glossary

| Term | Meaning |
| ---- | ------- |
| SNI  | Server Name Identification. This TLS protocol extension allows a server to select a certificate during the initial TLS handshake depending on a client-provided name. This usually allows a single reverse-proxy to serve several HTTPS websites.
| Host certificate | Certificate that a host sends clients when the latter initiate a connection with the former. The clients may close the connection depending on the properties of this certificate and whether they have decided to trust it previously.
| Trusted certificate | Certificate that a computer uses to verify whether a host certificate is valid. If the host certificate's chain of trust does not include a trusted certificate it will be considered invalid.
| Default Certificate | Xenserver hosts present this certificate to clients which do not request an SNI. Users are allowed to install their own custom certificate.
| Pool Certificate | Xenserver hosts present this certificate to clients which request `xapi:pool`as the SNI. They are used for host-to-host communications.
| Common Criteria |  Common Criteria for Information Technology Security Evaluation is a certification on computer security.

# Certificates and Identity management

Currently Xenserver hosts generate self-signed certificates with the IP or FQDN as their subjects, users may also choose to install certificates.
When installing these certificates only the cryptographic algorithms used to generate the certificates (private key and hash) are validated and no properties about them are required.

This means that using user-installed certificates for intra-pool communication may prove difficult as restrictions regarding FQDN and chain validation need to be ensured before enabling TLS certificate checking or the pool communications will break down.

Instead a different certificate is used only for pool communication.
This allows to decouple whatever requirements users might have for the certificates they install to the requirements needed for secure pool communication.
This has several benefits:

* Frees the pool from ensuring a sound hostname resolution on the internal communications.
* Allows the pool to rotate the certificates when it deems necessary. (in particular expiration, or forced invalidation)
* Hosts never share a host certificate, and their private keys never get transmitted.

In general, the project is able to more safely change the parameters of intra-pool communication without disrupting how users use custom certificates.

To be able to establish trust in a pool, hosts must distribute the certificates to the rest of the pool members.
Once that is done servers can verify whether they are connecting to another host in the pool by comparing the server certificate with the certificates in the trust root.
Certificate pinning is available and would allow more stringent checks, but it doesn't seem a necessity: hosts in a pool already share secret that allows them to have full control of the pool.

To be able to select a host certificate depending whether the connections is intra-pool or comes from API clients SNI will be used.
This allows clients to ask for a service when establishing a TLS connection.
This allows the server to choose the certificate they want to offer when negotiating the connection with the client.
The hosts will exploit this to request a particular service when they establish a connection with other hosts in the pool.
When initiating a connection to another host in the pool, a server will create requests for TLS connections with the server_name `xapi:pool` with the `name_type` `DNS`, this goes against RFC-6066 as this `server_name` is not resolvable.
This still works because we control the implementation in both peers of the connection and can follow the same convention.

In addition connections to the WLB appliance will continue to be validated using the current scheme of user-installed CA certificates.
This means that hosts connecting to the appliance will need a special case to only trust user-installed certificated when establishing the connection.
Conversely pool connections will ignore these certificates.
| Name | Filesystem location | User-configurable | Used for |
| ---- | ------------------- | ----------------- | -------- |
| Host Default    | /etc/xensource/xapi-ssl.pem      | yes (using API) | Hosts serve it to normal API clients
| Host Pool       | /etc/xensource/xapi-pool-tls.pem | no              | Hosts serve to clients requesting "xapi:pool" as the SNI
| Trusted Default | /etc/stunnel/certs/              | yes (using API) | Certificates that users can install for trusting appliances
| Trusted Pool    | /etc/stunnel/certs-pool/         | no              | Certificates that are managed by the pool for host-to-host communications
| Default Bundle  | /etc/stunnel/xapi-stunnel-ca-bundle.pem | no       | Bundle of certificates that hosts use to verify appliances (in particular WLB), this is kept in sync with "Trusted Default"
| Pool Bundle     | /etc/stunnel/xapi-pool-ca-bundle.pem    | no       | Bundle of certificates that hosts use to verify other hosts on pool communications, this is kept in sync with "Trusted Pool"

## Cryptography of certificates

The certificates until now have been signed using sha256WithRSAEncryption:

* Pre-8.0 releases use 1024-bit RSA keys.
* 8.0, 8.1 and 8.2 use 2048-bit RSA keys.

The Default Certificates served to API clients will continue to use sha256WithRSAEncryption with 2048-bit RSA keys. The Pool certificates will use the same algorithms for consistency.

The self-signed certificates until now have used a mix of IP and hostname claims:

* All released versions:
  - Subject and issuer have CN FQDN if the hostname is different from localhost, or CN management IP
  - Subject Alternate Names extension contains all the domain names as DNS names
* Next release:
  - Subject and issuer have CN management IP
  - SAN extension contains all domain names as DNS names and the management IP as IP

The Pool certificates do not contain claims about IPs nor hostnames as this may change during runtime and depending on their validity may make pool communication more brittle.
Instead the only claim they have is that their Issuer and their Subject are CN Host UUID, along with a serial number.

Self-signed certificates produced until now have had validity periods of 3650 days (~10 years).
The Pool certificates will have the same validity period.

# Server Components

HTTPS Connections between hosts usually involve the xapi daemons and stunnel processes:

- When a xapi daemon needs to initiate a connection with another host it starts an HTTP connection with a local stunnel process.
- The stunnel processes wrap http connections inside a TLS connection, allowing HTTPS to be used when hosts communicate

This means that stunnel needs to be set up correctly to verify certificates when connecting to other hosts.
Some aspects like CA certificates are already managed, but certificate pinning is not.

# Use Cases

There are several use cases that need to be modified in order correctly manage trust between hosts.

## Opening a connection with a pool host

This is the main use case for the feature, the rest of use cases that need changes are modified to support this one.
Currently a Xenserver host connecting to another host within the pool does not try to authenticate the receiving server when opening a TLS connection.
(The receiving server authenticates the originating server by xapi authentication, see below)

Stunnel will be configured to verify the peer certificate against the CA certificates that are present in the host.
The CA certificates must be correctly set up when a host joins the pool to correctly establish trust.

The previous behaviour for WLB must be kept as the WLB connection _must_ be checked against the user-friendly CA certificates.

## Receiving an incoming TLS connection

All incoming connections authenticate the client using credentials, this does not need the addition of certificates.
(username and password, pool secret)

The hosts must present the certificate file to incoming connections so the client can authenticate them.
This is already managed by xapi, it configures stunnel to present the configured host certificate.
The configuration has to be changed so stunnel responds to SNI requests containing the string `xapi:pool` to serve the internal certificate instead of the client-installed one.

## U1. Host Installation

On xapi startup an additional certificate is created now for pool operations.
It's added to the trusted pool certificates.
The certificate's only claim is the host's UUID.
No IP nor hostname information is kept as the clients only check for the certificate presence in the trust root.

## U2. Pool Join

This use-case is delicate as it is the point where trust is established between hosts.
This is done with a call from the joiner to the pool coordinator where the certificate of the coordinator is not verified.
In this call the joiner transmits its certificate to the coordinator and the coordinator returns a list of the pool members' UUIDs and certificates.
This means that in the policy used is trust on first use.

To deal with parallel pool joins, hosts download all the Pool certificates in the pool from the coordinator after all restarts.

The connection is initiated by a client, just like before, there is no change in the API as all the information needed to start the join is already provided (pool username and password, IP of coordinator)

~~~mermaid

sequenceDiagram
participant clnt as Client
participant join as Joiner
participant coor as Coordinator
participant memb as Member
clnt->>join: pool.join coordinator_ip coordinator_username coordinator_password
join->>coor:login_with_password coordinator_ip coordinator_username coordinator_password

Note over join: pre_join_checks
join->>join: remote_pool_has_tls_enabled = self_pool_has_tls_enabled
alt are different
Note over join: interrupt join, raise error
end
Note right of join: certificate distribution
coor-->>join:
join->>coor: pool.internal_certificate_list_content
coor-->>join:

join->>coor: pool.upload_identity_host_certificate joiner_certificate uuid
coor->>memb: pool.internal_certificates_sync
memb-->>coor:

loop for every <user CA certificate> in Joiner
join->>coor: Pool.install_ca_certitificate <user CA certificate>
coor-->>join:
end

loop for every <user CRL> in Joiner
join->>coor: Pool.install_crl <user CRL>
coor-->>join:
end

join->>coor: host.add joiner
coor-->>join:

join->>join: restart_as_slave
join->>coor: pool.user_certificates_sync
join->>coor: host.copy_primary_host_certs

~~~

## U3. Pool Eject

During pool eject the pool must remove the host certificate of the ejected member from the internal trust root, this must be done by the xapi daemon of the coordinator.

The ejected member will recreate both server certificates to replicate a new installation.
This can be triggered by deleting the certificates and their private keys in the host before rebooting, the current boot scripts automatically generates a new self-signed certificate if the file is not present.
Additionally, both the user and the internal trust roots will be cleared before rebooting as well.

## U4. Pool Upgrade

When a pool has finished upgrading to the version with certificate checking the database reflects that the feature is turned off, this is done as part of the database upgrade procedure in xen-api.
The internal certificate is created on restart.
It is added to the internal trusted certificates directory.
The distribution of certificate will happens when the tls verification is turned on, afterwards.

## U5. Host certificate state inspection

In order to give information about the validity and useful information of installed user-facing certificates to API clients as well as the certificates used for internal purposes, 2 fields are added to certificate records in xapi's datamodel and database:

- type: indicates which of the 3 kind of certificates is the certificate. If it's a user-installed trusted CA certificate, a server certificate served to clients that do not use SNI, and a server certificate served when the SNI xapi:pool is used. The exact values are ca, host and host-internal, respectively.
- name: the human-readable name given by the user. This fields is only present on trusted CA certificates and allows the pool operators to better recognise the certificates.

Additionally, now the _host field contains a null reference if the certificate is a corporate CA (a ca certificate).

The fields will get exposed in the CLI whenever a certificate record is listed, this needs a xapi-cli-server to be modified to show the new field.

## U6. Migrating a VM to another pool

To enable a frictionless migration when pools have tls verification enabled, the host certificate of the host receiving the vm is sent to the sender.
This is done by adding the certificate of the receiving host as well as its pool coordinator to the return value of the function migrate_receive function.
The sender can then add the certificate to the folder of CA certificates that stunnel uses to verify the server in a TLS connection.
When the transaction finishes, whether it fails or succeeds the CA certificate is deleted.

The certificate is stored in a temporary location so xapi can clean up the file when it starts up, in case after the host fences or power cycles while the migration is in progress.

Xapi invokes sparse_dd with the filename correct trusted bundle as a parameter so it can verify the vhd-server running on the other host.

Xapi also invokes xcp-rrdd to migrate the VM metrics.
xcp-rrdd is passed the 2 certificates to verify the remote hosts when sending the metrics.

Clients should not be aware of this change and require no change.

Xapi-cli-server, the server of xe embedded into xapi, connects to the remote coordinator using TLS to be able to initiate the migration.
Currently no verification is done. A certificate is required to initiate the connection to verify the remote server.

In u6.3 and u6.4 no changes seem necessary.

## U7. Change a host's name

The Pool certificates do not depend on hostnames.
Changing the hostnames does not affect TLS certificate verification in a pool.

## U8. Installing a certificate (corporate CA)

Installation of corporate CA can be done with current API.
Certificates are added to the database as CA certificates.

## U9. Resetting a certificate (to self-signed certificate)

This needs a reimplementation of the current API to reset host certificate, this time allowing the operation to happen when the host is not on emergency node and to be able to do it remotely.

## U10. Enabling certificate verification

A new API call is introduced to enable tls certificate verification: Pool.enable_tls_verification.
This is used by the CLI command pool-enable-tls-verification.
The call causes the coordinator of the pool to install the Pool certificates of all the members in its internal trust root.
Then calls the api for each member to install all of these certificates.
After this public key exchange is done, TLS certificate verification is enabled on the members, with the coordinator being the last to enable it.

When there are issues that block enabling the feature, the call returns an error specific to that problem:

* HA must not be enabled, as it can interrupt the procedure when certificates are distributed
* Pool operations that can disrupt the certificate exchange block this operation: These operations are listed in here
* There was an issue with the certificate exchange in the pool.

The coordinator enabling verification last is done to ensure that if there is any issue enabling the coordinator host can still connect to members and rollback the setting.

A new field is added to the pool: tls_verification_enabled. This enables clients to query whether TLS verification is enabled.

## U11. Disabling certificate verification

A new emergency command is added emergency-host-disable-tls-verification.
This command disables tls-verification for the xapi daemon in a host.
This allows the host to communicate with other hosts in the pool.

After that, the admin can regenerate the certificates using the new host-refresh-server-certificate in the hosts with invalid certificates, finally they can reenable tls certificate checking using the call emergency-host-reenable-tls-verification.

The documentation will include instructions for administrators on how to reset certificates and manually installing the host certificates as CA certificates to recover pools.

This means they will not have to disable TLS and compromise on security.

## U12. Being aware of certificate expiry

Stockholm hosts provide alerts 30 days before hosts certificates expire, it must be changed to alert about users' CA certificates expiring.

Pool certificates need to be cycled when the certificate expiry is approaching.
Alerts are introduced to warn the administrator this task must be done, or risk the operation of the pool.
A new API is introduced to create certificates for all members in a pool and replace the existing internal certificates with these.
This call imposes the same requirements in a pool as the pool secret rotation: It cannot be run in a pool unless all the host are online, it can only be started by the coordinator, the coordinator is in a valid state, HA is disabled, no RPU is in progress, and no pool operations are in progress.
The API call is Pool.rotate_internal_certificates.
It is exposed by xe as pool-rotate-internal-certificates.

# Changes

Xapi startup has to account for host changes that affect this feature and modify the filesystem and pool database accordingly.

* Public certificate changed: On first boot, after a pool join and when doing emergency repairs the server certificate record of the host may not match to the contents in the filesystem. A check is to be introduced that detects if the database does not associate a certificate with the host or if the certificate's public key in the database and the filesystem are different. If that's the case the database is updated with the certificate in the filesystem.
* Pool certificate not present: In the same way the public certificate served is generated on startup, the internal certificate must be generated if the certificate is not present in the filesystem.
* Pool certificate changed: On first boot, after a pool join and after having done emergency repairs the internal server certificate record may not match the contents of the filesystem. A check is to be introduced that detects if the database does not associate a certificate with the host or if the certificate's public key in the database and the filesystem are different. This check is made aware whether the host is joining a pool or is on first-boot, it does this by counting the amount of hosts in the pool from the database. In the case where it's joining a pool it simply updated the database record with the correct information from the filesystem as the filesystem contents have been put in place before the restart. In the case of first boot the public part of the certificate is copied to the directory and the bundle for internally-trusted certificates: /etc/stunnel/certs-pool/ and /etc/stunnel/xapi-pool-ca-bundle.pem.

The xapi database records for certificates must be changed according with the additions explained before.

### API

Additions
* Pool.tls_verification_enabled: this is a field that indicates whether TLS verification is enabled.
* Pool.enable_tls_verification: this call is allowed for role _R_POOL_ADMIN. It's not allowed to run if HA is enabled nor pool operations are in progress. All the hosts in the pool transmit their certificate to the coordinator and the coordinator then distributes the certificates to all members of the pool. Once that is done the coordinator tries to initiate a session with all the pool members with TLS verification enabled. If it's successful TLS verification is enabled for the whole pool, otherwise the error COULD_NOT_VERIFY_HOST [member UUID] is emmited.
* TLS_VERIFICATION_ENABLE_IN_PROGRESS is a new error that is produced when trying to do other pool operations while enabling TLS verification is in progress
* Host.emergency_disable_tls_verification: this called is allowed for role _R_LOCAL_ROOT_ONLY: it's an emergency command and acts locally. It forces connections in xapi to stop verifying the peers on outgoing connections. It generates an alert to warn the administrators of this uncommon state.
* Host.emergency_reenable_tls_verification: this call is allowed for role _R_LOCAL_ROOT_ONLY: it's an emergency command and acts locally. It changes the configuration so xapi verifies connections by default after being switched off with the previous command.
* Pool.install_ca_certificate: rename of Pool.certificate_install, add the ca certificate to the database.
* Pool.uninstall_ca_certificate: rename of Pool.certificate_uninstall, removes the certificate from the database.
* Host.reset_server_certificate: replaces Host.emergency_reset_server_certificate, now it's allowed for role _R_POOL_ADMIN. It adds a record for the generated Default Certificate to the database while removing the previous record, if any.
* Pool.rotate_internal_certificates: This call generates new Pool certificates, and substitutes the previous certificates with these. See the certificate expiry section for more details.

Modifications:
* Pool.join: certificates must be correctly distributed. API Error POOL_JOINING_HOST_TLS_VERIFICATION_MISMATCH is returned if the tls_verification of the two pools doesn't match.
* Pool.eject: all certificates must be deleted from the ejected host's filesystem and the ejected host's certificate must be deleted from the pool's trust root.
* Host.install_server_certificate: the certificate type host for the record must be added to denote it's a Standard Certificate.

Deprecations:
* pool.certificate_install
* pool.certificate_uninstall
* pool.certificate_list
* pool.wlb_verify_cert: This setting is superseeded by pool.enable_tls_verification. It cannot be removed, however. When updating from a previous version when this setting is on, TLS connections to WLB must still verify the external host. When the global setting is enabled this setting is ignored.
* host.emergency_reset_server_certificate: host.reset_server_certificate should be used instead as this call does not modify the database.

### CLI

Following API additions:
* pool-enable-tls-verification
* pool-install-ca-certificate
* pool-uninstall-ca-certificate
* pool-internal-certificates-rotation
* host-reset-server-certificate
* host-emergency-disable-tls-verification (emits a warning when verification is off and the pool-level is on)
* host-emergency-reenable-tls-verification

And removals:
* host-emergency-server-certificate

### Feature Flags

This feature needs clients to behave differently when initiating pool joins, to allow them to choose behaviour the toolstack will expose a new feature flag 'Certificate_verification'. This flag will be part of the express edition as it's meant to aid detection of a feature and not block access to it.

### Alerts

Several alerts are introduced:
* POOL_CA_CERTIFICATE_EXPIRING_30, POOL_CA_CERTIFICATE_EXPIRING_14, POOL_CA_CERTIFICATE_EXPIRING_07, POOL_CA_CERTIFICATE_EXPIRED: Similar to host certificates, now the user-installable pool's CA certificates are monitored for expiry dates and alerts are generated about them. The body for this type of message is:

    <body><message>The trusted TLS server certificate {is expiring soon|has expired}.</message><date>20210302T02:00:01Z</date></body>

* HOST_INTERNAL_CERTIFICATE_EXPIRING_30, HOST_INTERNAL_CERTIFICATE_EXPIRING_14, HOST_INTERNAL_CERTIFICATE_EXPIRING_07, HOST_INTERNAL_CERTIFICATE_EXPIRED: Similar to host certificates, the newly-introduced hosts' internal server certificates are monitored for expiry dates and alerts are generated about them. The body for this type of message is:

    <body><message>The TLS server certificate for internal communications {is expiring soon|has expired}.</message><date>20210302T02:00:01Z</date></body>

* TLS_VERIFICATION_EMERGENCY_DISABLED: The host is in emergency mode and is not enforcing tls verification anymore, the situation that forced the disabling must be fixed and the verification enabled ASAP.

    <body><host>HOST-UUID</host></body>

* FAILED_LOGIN_ATTEMPTS: An hourly alert that contains the number of failed attempts and the 3 most common origins for these failed alerts. The body for this type of message is:

    <body>
    <total>35</total>
    <known><username>usr5</username><originator>origin5</originator><ip>5.4.3.2</ip><number>10</number><date>20200922T15:03:13Z</date></known>
    <known><username>usr4</username><useragent>UA</useragent><number>6</number><date>20200922T15:03:13Z</date></known>
    <known><useragent>UA</useragent><ip>4.3.2.1</ip><number>4</number><date>20200922T14:57:11Z</date></known>
    <unknown>10</unknown>
    </body>
