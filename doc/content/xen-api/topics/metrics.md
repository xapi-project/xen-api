---
title: Metrics
layout: default
---

[xcp-rrdd](https://github.com/xapi-project/xen-api/ocaml/xcp-rrdd)
records statistics about the host and the VMs running on top.
The metrics are stored persistently for long-term access and analysis of
historical trends.
Statistics are stored in [RRDs](http://oss.oetiker.ch/rrdtool/) (Round Robin
Databases).
RRDs are fixed-size structures that store time series with decreasing time
resolution: the older the data point is, the longer the timespan it represents.
'Data sources' are sampled every few seconds and points are added to
the highest resolution RRD. Periodically each high-frequency RRD is
'consolidated' (e.g. averaged) to produce a data point for a lower-frequency
RRD.

RRDs are resident on the host on which the VM is running, or the pool
coordinator when the VM is not running.
The RRDs are backed up every day.

Granularity
-----------

Statistics are persisted for a maximum of one year, and are stored at
different granularities.
The average and most recent values are stored at intervals of:

- five seconds for the past ten minutes
- one minute for the past two hours
- one hour for the past week
- one day for the past year

RRDs are saved to disk as uncompressed XML. The size of each RRD when
written to disk ranges from 200KiB to approximately 1.2MiB when the RRD
stores the full year of statistics.

By default each RRD contains only averaged data to save storage space.
To record minimum and maximum values in future RRDs, set the Pool-wide flag

```sh
xe pool-param-set uuid= other-config:create_min_max_in_new_VM_RRDs=true
```

Downloading
===========

Statistics can be downloaded over HTTP in XML or JSON format, for example
using `wget`.
See [rrddump](http://oss.oetiker.ch/rrdtool/doc/rrddump.en.html) and
[rrdxport](http://oss.oetiker.ch/rrdtool/doc/rrdxport.en.html) for information
about the XML format.
The JSON format has the same structure as the XML.
Parameters are appended to the URL following a question mark (?) and separated
by ampersands (&).
HTTP authentication can take the form of a username and password or a session
token in a URL parameter.

Statistics may be downloaded all at once, including all history, or as
deltas suitable for interactive graphing.

Downloading statistics all at once
----------------------------------

To obtain a full dump of RRD data for a host use:

```sh
wget  http://hostname/host_rrd?session_id=OpaqueRef:43df3204-9360-c6ab-923e-41a8d19389ba"
```

where the session token has been fetched from the server using the API.

For example, using Python's [XenAPI](https://pypi.org/project/XenAPI/) library:

```python
import XenAPI
username = "root"
password = "actual_password"
url = "http://hostname"
session = XenAPI.Session(url)
session.xenapi.login_with_password(username, password, "1.0", "session_getter")
session._session
```

A URL parameter is used to decide which format to return: XML is returned by
default, adding the parameter `json` makes the server return JSON.
Starting from xapi version 23.17.0, the server uses the HTTP header `Accept`
to decide which format to return.
When both formats are accepted, for example, using `*/*`; JSON is returned.
Of interest are the clients wget and curl which use this accept header value,
meaning that when using them the default behaviour will change and the accept
header needs to be overridden to make the server return XML.
The content type is provided in the reponse's headers in these newer versions.

The XML RRD data is in the format used by rrdtool and looks like this:

```xml
<?xml version="1.0"?>
<rrd>
  <version>0003</version>
  <step>5</step>
  <lastupdate>1213616574</lastupdate>
  <ds>
    <name>memory_total_kib</name>
    <type>GAUGE</type>
    <minimal_heartbeat>300.0000</minimal_heartbeat>
    <min>0.0</min>
    <max>Infinity</max>
    <last_ds>2070172</last_ds>
    <value>9631315.6300</value>
    <unknown_sec>0</unknown_sec>
  </ds>
  <ds>
   <!-- other dss - the order of the data sources is important
        and defines the ordering of the columns in the archives below -->
  </ds>
  <rra>
    <cf>AVERAGE</cf>
    <pdp_per_row>1</pdp_per_row>
     <params>
      <xff>0.5000</xff>
    </params>
    <cdp_prep> <!-- This is for internal use -->
      <ds>
        <primary_value>0.0</primary_value>
        <secondary_value>0.0</secondary_value>
        <value>0.0</value>
        <unknown_datapoints>0</unknown_datapoints>
      </ds>
      ...other dss - internal use only...
    </cdp_prep>
    <database>
     <row>
        <v>2070172.0000</v>  <!-- columns correspond to the DSs defined above -->
        <v>1756408.0000</v>
        <v>0.0</v>
        <v>0.0</v>
        <v>732.2130</v>
        <v>0.0</v>
        <v>782.9186</v>
        <v>0.0</v>
        <v>647.0431</v>
        <v>0.0</v>
        <v>0.0001</v>
        <v>0.0268</v>
        <v>0.0100</v>
        <v>0.0</v>
        <v>615.1072</v>
     </row>
     ...
  </rra>
  ... other archives ...
</rrd>

```

To obtain a full dump of RRD data of a VM with uuid `x`:

```sh
wget "http://hostname/vm_rrd?session_id=<token>&uuid=x"
```

Note that it is quite expensive to download full RRDs as they contain
lots of historical information. For interactive displays clients should
download deltas instead.


Downloading deltas
------------------

To obtain an update of all VM statistics on a host, the URL would be of
the form:

```sh
wget "https://hostname/rrd_updates?session_id=<token>&start=<secondsinceepoch>"
```

This request returns data in an rrdtool `xport` style XML format, for every VM
resident on the particular host that is being queried.
To differentiate which column in the export is associated with which VM, the
`legend` field is prefixed with the UUID of the VM.

An example `rrd_updates` output:

```xml
<xport>
  <meta>
    <start>1213578000</start>
    <step>3600</step>
    <end>1213617600</end>
    <rows>12</rows>
    <columns>12</columns>
    <legend>
      <entry>AVERAGE:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry> <!-- nb - each data source might have multiple entries for different consolidation functions -->
      <entry>AVERAGE:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>AVERAGE:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
      <entry>MIN:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry>
      <entry>MIN:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>MIN:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
      <entry>MAX:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry>
      <entry>MAX:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>MAX:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
      <entry>LAST:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry>
      <entry>LAST:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>LAST:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
    </legend>
  </meta>
  <data>
    <row>
      <t>1213617600</t>
      <v>0.0</v> <!-- once again, the order or the columns is defined by the legend above -->
      <v>0.0282</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0201</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0445</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0243</v>
      <v>209715200.0000</v>
    </row>
   ...
  </data>
</xport>
```

To obtain host updates too, use the query parameter `host=true`:

```sh
wget "http://hostname/rrd_updates?session_id=<token>&start=<secondssinceepoch>&host=true"
```

The step will decrease as the period decreases, which means that if you
request statistics for a shorter time period you will get more detailed
statistics.

To download updates containing only the averages, or minimums or maximums,
add the parameter `cf=AVERAGE|MIN|MAX` (note case is important) e.g.

```sh
wget "http://hostname/rrd_updates?session_id=<token>&start=0&cf=MAX"
```

To request a different update interval, add the parameter `interval=seconds` e.g.

```sh
wget "http://hostname/rrd_updates?session_id=<token>&start=0&interval=5"
```
