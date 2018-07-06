v3.1.0 2017-06-14
-----------------

- Port build to Jbuilder.

v3.0.0
------

Adapt to MirageOS 3 CHANNEL interface:

- use `result` instead of exceptions
- hide `read_until` as an internal implementation
- remove `read_stream` from external interface as it is
  difficult to combine Lwt_stream and error handling.

v1.1.1 2016-10-20
-----------------

- port to topkg and odig conventions

v1.1.0 2016-06-28
-----------------

- don't call `close` on `Eof`
- add `read_exactly`
- add LICENSE
- add conflict with old versions of TCP/IP
