---
title: Pool-wide SSH
layout: default
design_doc: true
revision: 1
status: proposed
---

## Background

The SMAPIv3 plugin architecture requires that storage plugins are able to work
in the absence of xapi. Amongst other benefits, this allows them to be tested
in isolation, are able to be shared more widely than just within the XenServer
community and will cause less load on xapi's database.

However, many of the currently existing SMAPIv1 backends require inter-host
operations to be performed. This is achieved via the use of the Xen-API call
'host.call_plugin', which allows an API user to execute a pre-installed plugin
on any pool member. This is important for operations such as coalesce / snapshot
where the active data path for a VM somewhere in the pool needs to be refreshed
in order to complete the operation. In order to use this, the RPM in which the
SM backend lives is used to deliver a plugin script into /etc/xapi.d/plugins,
and this executes the required function when the API call is made.

In order to support these use-cases without xapi running, a new mechanism needs
to be provided to allow the execution of required functionality on remote hosts.
The canonical method for remotely executing scripts is ssh - the secure shell.
This design proposal is setting out how xapi might manage the public and
private keys to enable passwordless authentication of ssh sessions between all
hosts in a pool.

## Modifications to the host

On firstboot (and after being ejected), the host should generate a
host key (already done I believe), and an authentication key for the
user (root/xapi?).

## Modifications to xapi

Three new fields will be added to the host object:

- ```host.ssh_public_host_key : string```: This is the host key that identifies the host
during the initial ssh key exchange protocol. This should be added to the
'known_hosts' field of any other host wishing to ssh to this host.

- ```host.ssh_public_authentication_key : string```: This field is the public
  key used for authentication when sshing from the root account on that host -
  host A. This can be added to host B's ```authorized_keys``` file in order to
  allow passwordless logins from host A to host B.

- ```host.ssh_ready : bool```: A boolean flag indicating that the configuration
  files in use by the ssh server/client on the host are up to date.

One new field will be added to the pool record:

- ```pool.revoked_authentication_keys : string list```: This field records all
authentication keys that have been used by hosts in the past. It is updated
when a host is ejected from the pool.

### Pool Join

On pool join, the master creates the record for the new host and populates the
two public key fields with values supplied by the joining host. It then sets
the ```ssh_ready``` field on all other hosts to ```false```.

On each host in the pool, a thread is watching for updates to the
```ssh_ready``` value for the local host. When this is set to false, the host
then adds the keys from xapi's database to the appropriate places in the ssh
configuration files and restarts sshd. Once this is done, the host sets the
```ssh_ready``` field to 'true'

### Pool Eject

On pool eject, the host's ssh_public_host_key is lost, but the authetication key is added to a list of revoked keys on the pool object. This allows all other hosts to remove the key from the authorized_keys list when they next sync, which in the usual case is immediately the database is modified due to the event watch thread. If the host is offline though, the authorized_keys file will be updated the next time the host comes online.


## Questions

- Do we want a new user? e.g. 'xapi' - how would we then use this user to execute privileged things? setuid binaries?
- Is keeping the revoked_keys list useful? If we 'control the world' of the authorized_keys file, we could just remove anything that's currently in there that xapi doesn't know about
