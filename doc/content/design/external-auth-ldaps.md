---
title: Secure LDAP (LDAPS) Support for External Authentication
layout: default
design_doc: true
revision: 1
status: draft
---

## Terminology

| Term | Meaning |
|------|---------|
| AD | Windows Active Directory |
| samba/winbind | Client used in xapi to talk to AD |
| DC | Windows AD domain controller |
| ldap/ldaps | Lightweight Directory Access Protocol / over SSL |
| Joining host | The host joining to a pool |

## 1. Background

To integrate XenServer with AD, XenServer performs LDAP queries in the following use cases:

- **Enable external authentication/Join domain**: Samba LDAP queries DC details
- **Session revalidation**: xapi queries user details (e.g., whether user is still valid, password expired, etc.) to decide whether to destroy a session

Currently XenServer uses plain LDAP queries, which is a concern for some enterprise customers.

## 2. Xapi Database

### 2.1 External Auth Configuration

External auth details are stored in the `host` (table) → `external_auth_configuration` (field). For example:

```
external_auth_configuration: {
    domain: xenrt16718.local,
    user: Administrator,
    workgroup: XENRTXENRT16718,
    netbios_name: genus-35-103d,
    machine_pwd_last_change_time: 1767508709
}
```

A new field `ldaps` (bool, optional) will be added to `external_auth_configuration` field to state whether LDAPS should be used instead of LDAP. If not set, LDAP will be used for backward compatibility.

So the field will look like:

```
external_auth_configuration: {
    domain: xenrt16718.local,
    user: Administrator,
    workgroup: XENRTXENRT16718,
    netbios_name: genus-35-103d,
    machine_pwd_last_change_time: 1767508709,
    ldaps: true
}
```

### 2.2 Certificate

To enforce security, if customer uses self-signed certificate, they need to upload the root CA certificate to XenServer, so XenServer can verify the certificate/public key used talking to DC for LDAPS.

The [trusted-certificates.md](https://github.com/xapi-project/xen-api/blob/master/doc/content/design/trusted-certificates.md) design enhanced the `Certificate` table and introduced a new field `purpose` for security, which limits the certificate only for specific purpose. `ldaps` will be added to `purpose` field as a value for LDAPS.

## 3. Interfaces

### 3.1 pool.enable_external_auth

#### 3.1.1 Interface

To enable external auth, the current API arguments are as follows:

- `pool` (Ref _pool): The pool whose external authentication should be enabled
- `config` (Map (String, String)): A list of key-values containing the configuration data
- `service_name` (String): The name of the service
- `auth_type` (String): The type of authentication (e.g., AD for Active Directory)

For example:

```bash
xe pool-enable-external-auth uuid=<uuid> auth-type=AD service-name=<domain> config:user=<user> config:pass=<pwd>
```

This API signature does not change. Regarding the config map, one new option is added:

- `config:ldaps`: whether LDAPS is required, default to `false`
  - Set `client ldap sasl wrapping` to `ldaps` if true, `seal` otherwise
  - This item will be stored in database in section 2.1

Given `ldaps` default to `false`, this feature is **NOT** enabled until explicitly set.

#### 3.1.2 Error code
Following new error codes added to indicate ldaps enable related error
- AUTH_NO_CERT,  no certs can be used for ldaps, refer to 4.1.2 for certs finding.
- AUTH_INVALID_CERT, found certs, but none of the certs can be used to connect to DC

### 3.2 Set/Get Pool LDAPS Status

#### 3.2.1 pool.external_auth_set_ldaps
#### 3.2.1.1 Interface

User can specify LDAPS during join domain as in 3.1.

For the existing joined domain, user can switch between LDAP and LDAPS with this new API. Args as follows:

- `pool` (Ref _pool): pool to set LDAPS
- `ldaps` (Bool): whether LDAPS is required
- `force` (Bool): whether to set ldaps even when ldaps is currently set

This API will set the `ldaps` in database (Refer to 2.1).

This API performs following sanity check and rejects update if check fails:

- AD has already been enabled
- ldaps has already been enabled without force
- Find proper certificate (Refer to 4.1 for the details)
- Do a `ldaps` query to embedded user `krbtgt` for the joined domain

**Note:**
- This API allow re-entry with `force` to perform an extra `ldaps ping` for debug purpose
- This API will not do the LDAPS query on the trusted domains, as xapi does not have trusted domain details
- The joined domain likely has multiple DCs. LDAPS query tries every DC of the domain. Check pass if LDAPS query succeeds on any DC of the domain. This implies iterate and locate a DC supporting LDAPS (with proper certificate trust setup) before LDAPS query. However, this does not introduce performance problems as the LDAPS query happens in backend and refreshes result into XAPI DB
- Pool coordinator dispatches this API request to every host, and only succeeds if all hosts pass the check
- This API needs to be synced with other APIs. For example, `authenticate_username_password` should fail if this API is performing checking and configuration

This API will refresh `winbind` configuration (Refer to 4.1).

So following xe command can be used to switch between LDAP and LDAPS:

```bash
xe pool-external-auth-set-ldaps uuid=<uuid> ldaps=<true|false>
```

#### 3.2.1.2 Error code
This API may raise following errors
- AUTH_NO_CERT, no certs found to enable ldaps, refer to 4.1.2 for certs finding
- AUTH_INVALID_CERT, found certs, but none of the certs can be used to connect to DC
- AUTH_IS_DISABLED, AD is not enabled
- AUTH_LDAPS_PING_FAILED,  failed to do ldaps query on all DCs with valid certs

#### 3.2.2 Get Pool LDAPS Status

xapi generates a get message for each field automatically. To query the LDAPS status, client only needs to query the get method of `host` (class) → `external-auth-configuration` (field), and parse the result. The example as follows:

```bash
xe host-param-get uuid=<uuid> param-name=external-auth-configuration
```

### 3.3 Install Certificate

If the certificate for LDAPS in DC is signed by a private CA (vs a trusted public CA), user needs to import their Root or Intermediate CA certificate into XenServer.

`pool.install_trusted_certificate` can install the certificate with following parameters, refer to [trusted-certificates.md](https://github.com/xapi-project/xen-api/blob/master/doc/content/design/trusted-certificates.md) for the details:

- `session` (ref session_id): reference to a valid session
- `self` (ref Pool): reference to the pool
- `ca` (boolean): should always be `true` for `ldaps`. xapi should reject this CA otherwise
- `cert` (string): the trusted certificate in PEM format
- `purpose` (string list): the purposes of this cert. It can be one of following:
  - contain `ldaps` if for specific this specific purpose
  - empty set, thus would take as general purpose CA. It will be used for `ldaps` if no `ldaps` specific purpose found

**Note:** If the DCs (of joining domain and trusted domain in use) are signed by different CAs, all the CAs need to be uploaded to XenServer.

## 4. Configuration Item

To enforce LDAPS, following are required:

- Samba needs to be updated to 4.21+ (Already done)
- LDAPS needs to be enabled in smb.conf

### 4.1 Samba Configuration

#### 4.1.1 smb.conf

To enforce LDAPS, xapi just passthrough the configuration to winbind. Following configuration needs to be updated to `/etc/samba/smb.conf`, details refer to [smb.conf](https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html):

```ini
client ldap sasl wrapping = <ldaps | seal>
tls verify peer = ca_and_name_if_available
tls trust system cas = yes
tls cafile = /etc/trusted-certs/ca-bundle-[ldaps|general].pem
tls priority = NONE:+VERS-TLS1.2:+AES-256-GCM:+AES-128-GCM:+AEAD:+ECDHE-RSA:+SIGN-ALL:+GROUP-SECP384R1:+COMP-NULL:%SERVER_PRECEDENCE
```

- Switch between `ldap` and `ldaps` will flip `client ldap sasl wrapping` between `seal` and `ldaps`
- `tls cafile` points to a CA bundle used to verify DC certs. Details refer to 4.1.2
- `tls priority` is following stunnel TLS configuration, this result to TLS 1.2 with cipher suite TLS_ECDHE_RSA_AES_128_GCM_SHA256, TLS_ECDHE_RSA_AES_256_GCM_SHA384, Windows Server 2008R2 and later as DC support it
  - NONE: starts with empty set
  - +VERS-TLS1.2: Enbale TLS 1.2
  - +AES-256-GCM:+AES-128-GCM: Enable AES-256-GCM and AES-128-GCM cipher
  - +AEAD: Enable AEAD MAC, required for GCM
  - +ECDHE-RSA: Enable ECDHE-RSA key exchange
  - +SIGN-ALL: Enable all signature algorithms, this will limit to TLS1.2
  - +GROUP-SECP384R1: Enable secp384r1 curve
  - +COMP-NULL: No compression (required in modern TLS)
  - %SERVER_PRECEDENCE: Server chooses cipher order

#### 4.1.2 Certificate Selection

This design is following [trusted-certificates.md](https://github.com/xapi-project/xen-api/blob/master/doc/content/design/trusted-certificates.md):

- Use `/etc/trusted-certs/ca-bundle-ldaps.pem` if exists
- Fall back to `/etc/trusted-certs/ca-bundle-general.pem` if exists and previous not match
- Report error if none of above match

**Note:** The selection/configuration is only refreshed on following cases:

- xapi (re)start
- `pool.external_auth_set_ldaps` API
- (Re)join domain

### 4.2 Xapi Configuration

#### 4.2.1 winbind-tls-verify-peer

For security, xapi asks winbind to verify CA certificate. `ca_and_name_if_available` is the default.

However, user may want to disable this verification for debug purpose.

`winbind-tls-verify-peer` is introduced for xapi configuration, and the possible values are `no_check`, `ca_only`, `ca_and_name_if_available`, `ca_and_name` and `as_strict_as_possible`.
 The configured value will override  `tls verify peer` value in xapi generated samba configuration. Refer to [smb.conf](https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html) for the details.


**Note:** This item is not intended for public documentation. This is only for debug purpose, or system tuning for specific scenarios from engineering/support team.

#### 4.2.2 ad-warning-message-interval

xapi sends warning message to user with this interval on LDAP query failure. Default to 1 week. Refer to section "Session revalidate" for the details.

## 5. Session Revalidate

xapi LDAP queries domain user status (if user has been added to manage XenServer) at configurable interval, and destroys the session created by domain user if user no longer in healthy status.

However, the LDAP query may fail due to various issues as follows:

- Temporary network issues
- CA certificate is not properly configured, or expired, etc.

Instead of destroying user session for stability, a warning message will be sent to user with the details at configurable interval `ad-warning-message-interval`.

- If no LDAP error, do nothing
- If error happens, send the warning message if:
  - first time see the error through xapi start up (so no need to persist last send time) or
  - `current_time - last_sent_time > winbind_warning_message_interval`

The message is defined as follows:
- name: AD_DC_LDAP_CHECK
- priority: Warning
- cls: `Host
- Body: LDAP(S) query check to `<DC>` of `<domain>` failed from `<host>` of `<pool>`

Note:
- The backend session revalidate check only performs on pool coordinator, thus the backend LDAP(S) query check only on coordinator
- `external_auth_set_ldaps` perform LDAP(S) query check on every host
- All previous AD_DC_LDAP_CHECK warning of a host will be cleaned on a successful LDAP(s) query from that host

## 6. Pool Join/Leave

### 6.1 pool.join

#### 6.1.1 AD Pre-checks

Currently the pool.join pre-check checks the following:

- `external_auth_type`: whether joined AD
- `external_auth_service_name`: whether joined the same domain

The pre-check is good enough, no matter whether `ldaps` is in use, as this ensures host can talk to AD. There are following cases:

- pool with AD, joining host with same AD: check pass as before this design
- pool without AD, joining host without AD: check pass as before this design
- pool without AD, joining host with AD: check failed as before this design
- pool with AD, joining host without AD:
  - LDAPS not enabled: joining host needs to join to same AD as before this design
  - LDAPS enabled: joining host needs to enable AD without certificate check, details refer to 6.1.2

#### 6.1.2 Join Host to Pool with LDAPS Enabled

When joining a host without AD to a pool with LDAPS enabled, the host may not have the (CA) certificate for the domain. It can be trivial to enforce customer to upload the CA certificate to every joining host, thus client would help to orchestrate certificates.

The workflow:

~~~mermaid

sequenceDiagram
participant user as User
participant client as Client
participant join as Joining host
participant coor as Pool Coordinator
participant dc as AD/DC

user->>client: pool.join
Note over client: precheck
alt precheck failed
client-->>user: precheck failed
end

Note over client,coor: sync all ldaps certs
client->>coor: pool.download_trusted_certificate
coor-->>client:
client->>join: pool.install_trusted_certificate
join-->>client:

user->>client: join domain username/password
client->>join: join domain username/password
join->>dc: join domain
dc-->>join:
join-->>client:

client->>join: pool.join
Note over join,coor: join pool ops<br/>certs sync
join-->>client:
client-->>user: pool.join succeed

~~~

**Detailed Steps:**

1. Client find proper `ldaps certs` from pool coordinator as `certs_pool`
   - a. find all certs `ldaps in purpose`
   - b. if no LDAPS certs, find all `general` certs
2. Client find all certs in joining host as `certs_joining_host`
3. Client identify the certs needs to be synced to joining host as `certs_to_sync = certs_pool - certs_joining_host` (certs in `certs_pool`, but not in `certs_joining_host`), the certs fingerprint should be used to identify the certs
4. Client download all `certs_to_sync`, `pool.download_trusted_certificate` from coordinator
5. Client upload all certs to joining pool, `pool.install_trusted_certificate` to joining pool, with the same purpose
6. Client trigger `pool.join` again with domain username and password
7. After pool.join:
   - If pool.join failed, Client call `pool.uninstall_trusted_certificate` on joining host to revert the certs
   - If pool.join succeed, do nothing as pool.join would sync the certs anyway

### 6.2 pool.leave

`pool.disable_external_auth` is called during pool leave, thus the `ldaps` status is cleaned.

This design does not change it.

## 7. Telemetry Support

### 7.1 External Auth Enabled

`host` (table) → `external_auth_type` (field) = `AD`

### 7.2 LDAPS Enabled

`host` (table) → `external_auth_configuration` (field) → `ldaps` (key) = `true`

## References

- [trusted-certificates.md](https://github.com/xapi-project/xen-api/blob/master/doc/content/design/trusted-certificates.md)
- [Samba smb.conf manual](https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html)
