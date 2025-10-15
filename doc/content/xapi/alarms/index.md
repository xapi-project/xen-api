+++
title = "How to set up alarms"
linkTitle = "Alarms"
+++

# Introduction

In XAPI, alarms are triggered by a Python daemon located at `/opt/xensource/bin/perfmon`.
The daemon is managed as a systemd service and can be configured by setting parameters in `/etc/sysconfig/perfmon`.

It listens on an internal Unix socket to receive commands. Otherwise, it runs in a loop, periodically requesting metrics from XAPI. It can then be configured to generate events based on these metrics. It can monitor various types of XAPI objects, including `VMs`, `SRs`, and `Hosts`. The configuration for each object is defined by writing an XML string into the object's `other-config` key.

The metrics used by `perfmon` are collected by the `xcp-rrdd` daemon. The `xcp-rrdd` daemon is a component of XAPI responsible for collecting metrics and storing them as Round-Robin Databases (RRDs).

A XAPI plugin also exists, providing the functions `refresh` and `debug_mem`, which send commands through the Unix socket. The `refresh` function is used when an `other-config` key is added or updated; it triggers the daemon to reread the monitored objects so that new alerts are taken into account. The `debug_mem` function logs the objects currently being monitored into `/var/log/user.log` as a dictionary.

# Monitoring and alarms

## Overview

- To get the metrics, `perfmon` requests XAPI by calling: `http://localhost/rrd_updates?session_id=<ref>&start=1759912021&host=true&sr_uuid=all&cf=AVERAGE&interval=60`
- Different consolidation functions can be used like **AVERAGE**, **MIN**, **MAX** or **LAST**. See the details in the next sections for specific objects and how to set it.
- Once retrieve, `perfmon` will check all its triggers and generate alarms if needed.

## Specific XAPI objects
### VMs

- To set an alarm on a VM, you need to write an XML string into the `other-config` key of the object. For example, to trigger an alarm when the CPU usage is higher than 50%, run:
```sh
xe vm-param-set uuid=<UUID> other-config:perfmon='<config> <variable> <name value="cpu_usage"/> <alarm_trigger_level value="0.5"/> </variable> </config>'
```

- Then, you can either wait until the new configuration is read by the `perfmon` daemon or force a refresh by running:
```sh
xe host-call-plugin host-uuid=<UUID> plugin=perfmon fn=refresh
```

- Now, if you generate some load inside the VM and the CPU usage goes above 50%, the `perfmon` daemon will create a message (a XAPI object) with the name **ALARM**. This message will include a _priority_, a _timestamp_, an _obj-uuid_ and a _body_. To list all messages that are alarms, run:
```sh
xe message-list name=ALARM
```

- You will see, for example:
```sh
uuid ( RO)         : dadd7cbc-cb4e-5a56-eb0b-0bb31c102c94
         name ( RO): ALARM
     priority ( RO): 3
        class ( RO): VM
     obj-uuid ( RO): ea9efde2-d0f2-34bb-74cb-78c303f65d89
    timestamp ( RO): 20251007T11:30:26Z
         body ( RO): value: 0.986414
config:
<variable>

        <name value="cpu_usage"/>

        <alarm_trigger_level value="0.5"/>

</variable>
```
- where the _body_ contains all the relevant information: the value that triggered the alarm and the configuration of your alarm.

- When configuring you alarm, your XML string can:
  - have multiple `<variable>` nodes
  - use the following values for child nodes:
       * **name**: what to call the variable (no default)
       * **alarm_priority**: the priority of the messages generated (default '3')
       * **alarm_trigger_level**: level of value that triggers an alarm (no default)
       * **alarm_trigger_sense**:'high' if alarm_trigger_level is a max, otherwise 'low'. (default 'high')
       * **alarm_trigger_period**: num seconds of 'bad' values before an alarm is sent (default '60')
       * **alarm_auto_inhibit_period**: num seconds this alarm disabled after an alarm is sent (default '3600')
       * **consolidation_fn**: how to combine variables from rrd_updates into one value (default is 'average' for 'cpu_usage', 'get_percent_fs_usage' for 'fs_usage', 'get_percent_log_fs_usage' for 'log_fs_usage','get_percent_mem_usage' for 'mem_usage', & 'sum' for everything else)
       * **rrd_regex** matches the names of variables from (xe vm-data-sources-list uuid=$vmuuid) used to compute value (only has defaults for "cpu_usage", "network_usage", and "disk_usage")

- Notice that `alarm_priority` will be the priority of the generated `message`, 0 being low priority.

### SRs

- To set an alarm on an SR object, as with VMs, you need to write an XML string into the `other-config` key of the SR. For example, you can run:
```sh
xe sr-param-set uuid=<UUID> other-config:perfmon='<config><variable><name value="physical_utilisation"/><alarm_trigger_level value="0.8"/></variable></config>'
```
- When configuring you alarm, the XML string supports the same child elements as for VMs

### Hosts

- As with VMs ans SRs, alarms can be configured by writing an XML string into an `other-config` key. For example, you can run:
```sh
xe host-param-set uuid=<UUID> other-config:perfmon=\
  '<config><variable><name value="cpu_usage"/><alarm_trigger_level value="0.5"/></variable></config>'
```

- The XML string can include multiple <variable> nodes allowed
- The full list of supported child nodes is:
    * **name**: what to call the variable (no default)
    * **alarm_priority**: the priority of the messages generated (default '3')
    * **alarm_trigger_level**: level of value that triggers an alarm (no default)
    * **alarm_trigger_sense**: 'high' if alarm_trigger_level is a max, otherwise 'low'. (default 'high')
    * **alarm_trigger_period**: num seconds of 'bad' values before an alarm is sent (default '60')
    * **alarm_auto_inhibit_period**:num seconds this alarm disabled after an alarm is sent (default '3600')
    * **consolidation_fn**: how to combine variables from rrd_updates into one value (default is 'average' for 'cpu_usage' & 'sum' for everything else)
    * **rrd_regex** matches the names of variables from (xe host-data-source-list uuid=<UUID>) used to compute value (only has defaults for "cpu_usage", "network_usage", "memory_free_kib" and "sr_io_throughput_total_xxxxxxxx") where that last one ends with the first eight characters of the SR UUID)

- As a special case for SR throughput, it is also possible to configure a Host by writing XML into the `other-config` key of an SR connected to it. For example:
```sh
xe sr-param-set uuid=$sruuid other-config:perfmon=\
  '<config><variable><name value="sr_io_throughput_total_per_host"/><alarm_trigger_level value="0.01"/></variable></config>'
```
- This only works for that specific variable name, and `rrd_regex` must not be specified.
- Configuration done directly on the host (variable-name, sr_io_throughput_total_xxxxxxxx) takes priority.

## Which metrics are available?

- Accepted name for metrics are:
  - **cpu_usage**: matches RRD metrics with the pattern `cpu[0-9]+`
  - **network_usage**: matches RRD metrics with the pattern `vif_[0-9]+_[rt]x`
  - **disk_usage**: match RRD metrics with the pattern `vbd_(xvd|hd)[a-z]+_(read|write)`
  - **fs_usage**, **log_fs_usage**, **mem_usage** and **memory_internal_free** do not match anything by default.
- By using `rrd_regex`, you can add your own expressions. To get a list of available metrics with their descriptions, you can call the `get_data_sources` method for [VM](https://xapi-project.github.io/new-docs/xen-api/classes/vm/), for [SR](https://xapi-project.github.io/new-docs/xen-api/classes/sr/) and also for [Host](https://xapi-project.github.io/new-docs/xen-api/classes/host/).
- A python script is provided at the end to get data sources. Using the script we can, for example, see:
```sh
# ./get_data_sources.py --vm 5a445deb-0a8e-c6fe-24c8-09a0508bbe21

List of data sources related to VM 5a445deb-0a8e-c6fe-24c8-09a0508bbe21
cpu0                         | CPU0 usage
cpu_usage                    | Domain CPU usage
memory                       | Memory currently allocated to VM
memory_internal_free         | Memory used as reported by the guest agent
memory_target                | Target of VM balloon driver
...
vbd_xvda_io_throughput_read  | Data read from the VDI, in MiB/s
...
```
- You can then set up an alarm when the data read from a VDI exceeds a certain level by doing:
```
xe vm-param-set uuid=5a445deb-0a8e-c6fe-24c8-09a0508bbe21 \
  other-config:perfmon='<config><variable> \
  <name value="disk_usage"/> \
  <alarm_trigger_level value="10"/> \
  <rrd_regex value="vbd_xvda_io_throughput_read"/> \
  </variable> </config>'
```
- Here is the script that allows you to get data sources:
```python
#!/usr/bin/env python3

import argparse
import sys
import XenAPI


def pretty_print(data_sources):
    if not data_sources:
        print("No data sources.")
        return

    # Compute alignment for something nice
    max_label_len = max(len(data["name_label"]) for data in data_sources)

    for data in data_sources:
        label = data["name_label"]
        desc = data["name_description"]
        print(f"{label:<{max_label_len}} | {desc}")


def list_vm_data(session, uuid):
    vm_ref = session.xenapi.VM.get_by_uuid(uuid)
    data_sources = session.xenapi.VM.get_data_sources(vm_ref)
    print(f"\nList of data sources related to VM {uuid}")
    pretty_print(data_sources)


def list_host_data(session, uuid):
    host_ref = session.xenapi.host.get_by_uuid(uuid)
    data_sources = session.xenapi.host.get_data_sources(host_ref)
    print(f"\nList of data sources related to Host {uuid}")
    pretty_print(data_sources)


def list_sr_data(session, uuid):
    sr_ref = session.xenapi.SR.get_by_uuid(uuid)
    data_sources = session.xenapi.SR.get_data_sources(sr_ref)
    print(f"\nList of data sources related to SR {uuid}")
    pretty_print(data_sources)


def main():
    parser = argparse.ArgumentParser(
        description="List data sources related to VM, host or SR"
    )
    parser.add_argument("--vm", help="VM UUID")
    parser.add_argument("--host", help="Host UUID")
    parser.add_argument("--sr", help="SR UUID")

    args = parser.parse_args()

    # Connect to local XAPI: no identification required to access local socket
    session = XenAPI.xapi_local()

    try:
        session.xenapi.login_with_password("", "")
        if args.vm:
            list_vm_data(session, args.vm)
        if args.host:
            list_host_data(session, args.host)
        if args.sr:
            list_sr_data(session, args.sr)
    except XenAPI.Failure as e:
        print(f"XenAPI call failed: {e.details}")
        sys.exit(1)
    finally:
        session.xenapi.session.logout()


if __name__ == "__main__":
    main()
```

