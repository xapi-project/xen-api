# Lwt_log: Lwt-friendly logging library (deprecated)

This library is deprecated in favor of the [`Logs_lwt`][logs_lwt] module of
[logs][logs].

See [`lwt_log_core.mli`][mli] for documentation.

To install, `opam install lwt_log`.

The library is split into two ocamlfind packages. The "basic" `lwt_log`
includes Unix log destination support, such as files and syslog, and
`Lwt_daemon`. `lwt_log.core` is the pure-OCaml part of `lwt_log`, suitable for
targeting JavaScript in the browser, or elsewhere where Unix is not available.



[logs_lwt]: http://erratique.ch/software/logs/doc/Logs_lwt.html
[logs]: http://erratique.ch/software/logs
[mli]: https://github.com/aantron/lwt_log/blob/master/src/core/lwt_log_core.mli
