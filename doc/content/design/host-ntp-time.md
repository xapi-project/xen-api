---
title: Host NTP and time config API
layout: default
design_doc: true
revision: 1
status: proposed
---

# Host NTP and time

## Background

There are no APIs to config the NTP and timezone on the host. Previously, users
had to login on the host via ssh and configure NTP and time manually. The goal
of this feature is to introduce new APIs in XAPI to support NTP and time
configuration, especially in the scenario that users disable ssh service on the
host. XAPI can also store the NTP and timezone configuration in XAPI DB to provide
cache for the getter APIs.

## Use cases

- User can set time zone of the host via XenAPI
- User can get valid time zones list to set via XenAPI
- User can get the current time zone of the host via XenAPI
- User can set custom NTP servers via XenAPI
- User can get the current custom NTP servers via XenAPI
- User can enable/disable NTP service via XenAPI
- User can get the current NTP service enabled or disabled via XenAPI
- User can set NTP to use DHCP assigned servers via XenAPI
- User can set NTP to use custom servers via XenAPI
- User can set NTP to use default NTP servers via XenAPI
- User can get the DHCP assigned NTP servers via XenAPI
- User can get status of current NTP servers via XenAPI
- User can get NTP sync status via XenAPI
- User can set host’s time when NTP is disabled via XenAPI

## NTP configuration on the host

New fields:  
`host.ntp_mode`, enum host_ntp_mode(DHCP, Custom, Factory, Disabled)  
`host.ntp_custom_servers`, string set

New APIs: `host.set_ntp_mode`, `host.set_ntp_custom_servers`, `host.get_ntp_mode`,
`host.get_ntp_custom_servers`, `host.get_ntp_servers_status`

Abstract the NTP configuration to 4 modes.
- DHCP: set NTP to use DHCP assigned NTP servers 
- Custom: set NTP to use custom NTP servers
- Factory: set NTP to use factory NTP servers
- Disabled: disable NTP service

### DHCP mode

In this mode, NTP uses the DHCP assigned NTP servers as sources.

How the NTP and DHCP interaction?

On the host, dhclient executes `/etc/dhcp/dhclient.d/chrony.sh` to update the
ntp servers when network event happens.
- `chrony.sh` writes ntp servers to `/run/chrony-dhcp/$interface.sources`
- Chonryd include the dir `/run/chrony-dhcp` by `sourcedir /run/chrony-dhcp` in
the conf file
- `chrony.sh` runs `chronyc reload sources` to reload NTP sources

Then NTP sources can be updated automatically in the DHCP mode

How to switch DHCP mode?

Dhclient stores dhcp lease in `/var/lib/xcp/dhclient-$interface.leases`, see
module Dhclient in `/ocaml/networkd/lib/network_utils.ml`.

When switch the NTP mode to DHCP, XAPI
- check ntp server item in the lease and fills it in chrony-dhcp files
- Add the exec permission of `chrony.sh`
- Remove all the NTP source items(Custom or Factory) in chronyd conf
- Restart chronyd

When switch ntp mode from dhcp to others, XAPI
- Remove the chrony-dhcp files
- Remove the exec permission of chrony.sh
- Add NTP source items(Custom or Factory) in chronyd conf
- Restart chronyd

### Custom mode

In this mode, NTP uses `host.ntp_custom_servers` as sources.

When switch the NTP mode to Custom, XAPI
- Remove NTP source items in chronyd conf
- Add `host.ntp_custom_servers` as NTP source items in chronyd conf
- Restart chronyd

When `host.ntp_custom_servers` changes and `host.ntp_mode` is Custom, set chronyd
conf with new custom servers and restart chronyd.

### Factory mode

In this mode, ntp uses `factory-ntp-servers` in XAPI config file. Generally the
factory-ntp-servers will be defined by the product.

### Disabled mode

This mode disables NTP service on the host.

### Others

`host.get_ntp_servers_status` calls `chronyc -c sources` to get ntp servers status.
Output parse:
```
Source mode: '^' = server, '=' = peer, '#' = local clock.
Source state: '*' = current synced, '+' = combined, '-' = not combined,
'?' = unreachable, 'x' = time may be in error, '~' = time too variable
```

## Timezone configuration on the host

New field: `host.timezone`, string

New APIs: `host.set_timezone`, `host.get_timezone`, `host.list_timezones`

The timezone is in IANA timezone database format. Timezone on the host can be
get by `realpath /etc/localtime` which is linked to timezone file under
`/usr/share/zoneinfo/`. It can set by link`/etc/localtime` to
`/usr/share/zoneinfo/<timezone>`. All the valid timezones are actually the files
under `/usr/share/zoneinfo/`.

Comparing to using a fixed UTC offset, the benefit is:
- User-friendly: familiar region names and same with most system facilities.
- Handles daylight saving time (DST) automatically

They are equivalent to the `timedatectl` commands
```
timedatectl set-timezone
timedatectl status | grep "Time zone"
timedatectl list-timezones
```

## Time on the host

New API: `host.get_ntp_synchronized`, `host.set_servertime`

`host.get_ntp_synchronized` shows if the system time is synchronized with NTP
source.

`host.set_servertime` offers an API to set the server time when NTP disabled.
it accepts a RFC3339 datetime format timestamp with timezone, i.e. ends with 'Z'
to represent the UTC or explicit UTC offset like '+05:00'.

## Dbsync and restriction

For the new fields in this doc, on XAPI start, dbsync will get the real host
status and sync to XAPI DB to make the real host status and XAPI DB consistent.
Upgrade case can also be benefited from the dbsync.

If the user changes the config behind XAPI, like modify the chronyd conf directly
via ssh, the real status on the host and XAPI DB come to inconsistent and may
lead to unpredicted result, unless restart XAPI.

## Usage examples

Set NTP DHCP mode and get the status
```python
session.xenapi.host.set_timezone(host_ref, 'UTC')
session.xenapi.host.set_ntp_mode(host_ref, 'DHCP')
session.xenapi.host.get_ntp_synchronized(host_ref)
session.xenapi.host.get_ntp_servers_status(host_ref)
```

Set NTP Custom mode and get the status
```python
session.xenapi.host.set_timezone(host_ref, 'Europe/London')
servers = ['time.server1.com', 'time.server2.com', 'time.server3.com']
session.xenapi.host.set_ntp_custom_servers(host_ref, servers)
session.xenapi.host.set_ntp_mode(host_ref, 'Custom')
session.xenapi.host.get_ntp_synchronized(host_ref)
session.xenapi.host.get_ntp_servers_status(host_ref)
```

Set NTP default mode and get the status
```python
session.xenapi.host.set_ntp_mode(host_ref, 'Factory')
session.xenapi.host.get_ntp_synchronized(host_ref)
session.xenapi.host.get_ntp_servers_status(host_ref)
```

Disable NTP and set server time
```python
session.xenapi.host.set_timezone(host_ref, 'Europe/London')
session.xenapi.host.set_ntp_mode(host_ref, 'Disabled')
session.xenapi.host.set_servertime(host_ref, "20251105T16:11:55Z")
```

## APIs summary

```
host.ntp_mode
host.ntp_custom_servers
host.timezone

host.set_timezone
host.list_timezones
host.get_timezone
host.get_ntp_synchronized
host.set_ntp_mode
host.get_ntp_mode
host.set_ntp_custom_servers
host.get_ntp_custom_servers
host.get_ntp_servers_status
host.set_servertime
```
