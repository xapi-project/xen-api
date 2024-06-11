---
title: RRDD plugin protocol v2
layout: default
design_doc: true
revision: 1
status: released (7.0)
revision_history:
- revision_number: 1
  description: Initial version
---

Motivation
----------

rrdd plugins currently report datasources via a shared-memory file, using the
following format:

```
DATASOURCES
000001e4
dba4bf7a84b6d11d565d19ef91f7906e
{
  "timestamp": 1339685573,
  "data_sources": {
    "cpu-temp-cpu0": {
      "description": "Temperature of CPU 0",
      "type": "absolute",
      "units": "degC",
      "value": "64.33"
      "value_type": "float",
    },
    "cpu-temp-cpu1": {
      "description": "Temperature of CPU 1",
      "type": "absolute",
      "units": "degC",
      "value": "62.14"
      "value_type": "float",
    }
  }
}
```

This format contains four main components:

* A constant header string

`DATASOURCES`

This should always be present.

* The JSON data length, encoded as hexadecimal

`000001e4`

* The md5sum of the JSON data

`dba4bf7a84b6d11d565d19ef91f7906e`

* The JSON data itself, encoding the values and metadata associated with the
reported datasources.

### Example
```
{
  "timestamp": 1339685573,
  "data_sources": {
    "cpu-temp-cpu0": {
      "description": "Temperature of CPU 0",
      "type": "absolute",
      "units": "degC",
      "value": "64.33"
      "value_type": "float",
    },
    "cpu-temp-cpu1": {
      "description": "Temperature of CPU 1",
      "type": "absolute",
      "units": "degC",
      "value": "62.14"
      "value_type": "float",
    }
  }
}
```

The disadvantage of this protocol is that rrdd has to parse the entire JSON
structure each tick, even though most of the time only the values will change.

For this reason a new protocol is proposed.

Protocol V2
-----------

|value|bits|format|notes|
|-----|----|------|-----|
|header string        |(string length)*8|string|"DATASOURCES" as in the V1 protocol                                                     |
|data checksum        |32               |int32 |binary-encoded crc32 of the concatenation of the encoded timestamp and datasource values|
|metadata checksum    |32               |int32 |binary-encoded crc32 of the metadata string (see below)                                 |
|number of datasources|32               |int32 |only needed if the metadata has changed - otherwise RRDD can use a cached value         |
|timestamp            |64               |int64 |Unix epoch                                                                              |
|datasource values    |n * 64           |int64 \| double |n is the number of datasources exported by the plugin, type dependent on the setting in the metadata for value_type [int64\|float]  |
|metadata length      |32               |int32 |                                                                                        |
|metadata             |(string length)*8|string|                                                                                        |

All integers/double are bigendian. The metadata will have the same JSON-based format as
in the V1 protocol, minus the timestamp and `value` key-value pair for each
datasource.

| field | values | notes | required |
|-------|--------|-------|----------|
|description|string|Description of the datasource|no|
|owner|host \| vm \| sr|The object to which the data relates|no, default host|
|value_type|int64 \| float|The type of the datasource|yes|
|type|absolute \| derive \| gauge|The type of measurement being sent. Absolute for counters which are reset on reading, derive stores the derivative of the recorded values (useful for metrics which continually increase like amount of data written since start), gauge for things like temperature|no, default absolute|
|default|true \| false|Whether the source is default enabled or not|no, default false|
|units|<TBD>|The units the data should be displayed in|no|
|min||The minimum value for the datasource|no, default -infinity|
|max||The maximum value for the datasource|no, default +infinity|


### Example
```
{
  "datasources": {
    "memory_reclaimed": {
      "description":"Host memory reclaimed by squeezed",
      "owner":"host",
      "value_type":"int64",
      "type":"absolute",
      "default":"true",
      "units":"B",
      "min":"-inf",
      "max":"inf"
    },
    "memory_reclaimed_max": {
      "description":"Host memory that could be reclaimed by squeezed",
      "owner":"host",
      "value_type":"int64",
      "type":"absolute",
      "default":"true",
      "units":"B",
      "min":"-inf",
      "max":"inf"
    },
    {
    "cpu-temp-cpu0": {
      "description": "Temperature of CPU 0",
      "owner":"host",
      "value_type": "float",
      "type": "absolute",
      "default":"true",
      "units": "degC",
      "min":"-inf",
      "max":"inf"
    },
    "cpu-temp-cpu1": {
      "description": "Temperature of CPU 1",
      "owner":"host",
      "value_type": "float",
      "type": "absolute",
      "default":"true",
      "units": "degC",
      "min":"-inf",
      "max":"inf"
    }
  }
}
```

The above formatting is not required, but added here for readability.

Reading algorithm
-----------------

```
if header != expected_header:
    raise InvalidHeader()
if data_checksum == last_data_checksum:
    raise NoUpdate()
if data_checksum != crc32(encoded_timestamp_and_values):
    raise InvalidChecksum()
if metadata_checksum == last_metadata_checksum:
    for datasource, value in cached_datasources, values:
        update(datasource, value)
else:
    if metadata_checksum != crc32(metadata):
        raise InvalidChecksum()
    cached_datasources = create_datasources(metadata)
    for datasource, value in cached_datasources, values:
        update(datasource, value)
```

This means that for a normal update, RRDD will only have to read the header plus
the first (16 + 16 + 4 + 8 + 8*n) bytes of data, where n is the number of
datasources exported by the plugin. If the metadata changes RRDD will have to
read all the data (and parse the metadata).

n.b. the timestamp reported by plugins is not currently used by RRDD - it uses
its own global timestamp.
