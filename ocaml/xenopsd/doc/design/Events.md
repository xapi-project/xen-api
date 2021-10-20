
- ids rather than data; inherently coalescable
- blocking poll + async operations implies a client needs 2 connections
- coarse granularity
- similarity and differences with: XenAPI, event channels, xenstore watches

https://github.com/xapi-project/xen-api/blob/30cc9a72e8726d1e7501cd01ddb27ced6d53b9be/ocaml/xapi/xapi_xenops.ml#L1467
