---
title: RRDD plugin protocol v3
layout: default
design_doc: true
revision: 1
status: proposed
revision_history:
- revision_number: 1
  description: Initial version
---

Motivation
----------

rrdd plugins protocol v2 report datasources via shared-memory file, however it
has various limitations :
 - metrics are unique by their names, thus it is not possible cannot have
  several metrics that shares a same name (e.g vCPU usage per vm)
 - only number metrics are supported, for example we can't expose string
  metrics (e.g CPU Model)

Therefore, it implies various limitations on plugins and limits
[OpenMetrics](https://openmetrics.io/) support for the metrics daemon.

Moreover, it may not be practical for plugin developpers and parser implementations :
 - json implementations may not keep insersion order on maps, which can cause
   issues to expose datasource values as it is sensitive to the order of the metadata map
 - header length is not constant and depends on datasource count, which complicates parsing
 - it still requires a quite advanced parser to convert between bytes and numbers according to metadata

A simpler protocol is proposed, based on OpenMetrics binary format to ease plugin and parser implementations.

Protocol V3
-----------

For this protocol, we still use a shared-memory file, but significantly change the structure of the file.

|     value      |        bits        | format |                             notes
| -------------- | ------------------ | ------ | ------------------------------------------------------------
| header string  |      12*8=96       | string | "OPENMETRICS1" which is one byte longer than "DATASOURCES", intentionally made at 12 bytes for alignment purposes
| data checksum  |         32         | uint32 | Checksum of the concatenation of the rest of the header (from timestamp) and the payload data
| timestamp      |         64         | uint64 | Unix epoch
| payload length |         32         | uint32 | Payload length
| payload data   | 8*(payload length) | binary | OpenMetrics encoded metrics data (protocol-buffers format)

All values are big-endian.

The header size is constant (28 bytes) that implementation can rely on (read
the entire header in one go, simplify usage of memory mapping).

As opposed to protocol v2 but alike protocol v1, metadata is included along
metrics in OpenMetrics format.

`owner` attribute for metric should be exposed using a OpenMetrics label instead (named `owner`).

Multiple metrics that shares the same name should be exposed under the same
Metric Family and be differenciated by labels (e.g `owner`).

Reading algorithm
-----------------

```python
if header != expected_header:
    raise InvalidHeader()
if data_checksum == last_data_checksum:
    raise NoUpdate()
if timestamp == last_timestamp:
    raise NoUpdate()
if data_checksum != crc32(concat_header_end_payload):
    raise InvalidChecksum()

metrics = parse_openmetrics(payload_data)

for family in metrics:
    if family_exists(family):
        update_family(family)
    else
        create_family(family)

track_removed_families(metrics)
```