(lang dune 3.0)
(formatting (enabled_for ocaml))
(using menhir 2.0)

(generate_opam_files true)

(name "xapi")
(source (github xapi-project/xen-api))
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(authors "xen-api@lists.xen.org")
(maintainers "Xapi project maintainers")
(homepage "https://xapi-project.github.io/")

(package
  (name zstd)
)

(package
  (name xapi-rrdd-plugin)
)

(package
  (name xml-light2)
)

(package
  (name xen-api-client-lwt)
)


(package
  (name xen-api-client-async)
)

(package
  (name xen-api-client)
)

(package
  (name xe)
)

(package
  (name xapi-xenopsd-xc)
)

(package
  (name xapi-xenopsd-simulator)
)

(package
  (name xapi-xenopsd-cli)
)

(package
  (name xapi-xenopsd)
)

(package
  (name xapi-types)
)

(package
  (name xapi-tracing)
  (depends
   ocaml
   dune
   (alcotest :with-test)
   re
   uri
   (uuid :with-test)
   (xapi-log (= :version))
   (xapi-stdext-threads (= :version))
  )
  (synopsis "Allows to instrument code to generate tracing information")
  (description "This library provides modules to allow gathering runtime traces.")
)

(package
  (name xapi-tracing-export)
  (depends
   ocaml
   cohttp-posix
   dune
   cohttp
   rpclib
   ppx_deriving_rpc
   (xapi-log (= :version))
   (xapi-open-uri (= :version))
   (xapi-stdext-threads (= :version))
   (xapi-stdext-unix (= :version))
   (xapi-tracing (= :version))
   (zstd (= :version))
  )
  (synopsis "Export traces in multiple protocols and formats")
  (description "This library export traces is able to push traces to http endpoints or generate compressed tarballs in the filesystem.")
)

(package
  (name xapi-storage-script)
)

(package
  (name xapi-storage-cli)
)

(package
  (name xapi-storage)
)

(package
  (name xapi-squeezed)
)

(package
  (name xapi-schema)
)

(package
  (name rrdd-plugin)
  (synopsis "A plugin library for the xapi performance monitoring daemon")
  (description "This library allows one to expose a datasource which can then be sampled by the performance monitoring daemon.")
  (depends
   ocaml
   astring
   rpclib
   (rrd-transport (= :version))
   (xapi-forkexecd (= :version))
   (xapi-stdext-pervasives (= :version))
   (xapi-stdext-std (= :version))
   (xapi-stdext-threads (= :version))
   (xapi-stdext-unix (= :version))
   (xapi-idl (= :version))
   xenstore_transport
  )
)

(package
  (name xapi-rrdd)
  (synopsis "Performance monitoring daemon for xapi")
  (description "This daemon monitors 'datasources' i.e. time-varying values such as performance counters and records the samples in RRD archives. These archives can be used to examine historical performance trends.")
  (depends
   (ocaml (>= "4.02.0"))
   dune-build-info
   (alcotest :with-test)
   astring
   (gzip (= :version))
   (http-lib (= :version))
   inotify
   io-page
   mtime
   ppx_deriving_rpc
   rpclib
   systemd
   (ezxenstore (= :version))
   (uuid (= :version))
   xapi-backtrace
   (xapi-idl (= :version))
   (xapi-rrd (= :version))
   (xapi-stdext-threads (= :version))
   (xapi-stdext-unix (= :version))
  )
)

(package
  (name xapi-rrd-transport-utils)
  (synopsis "Shared-memory protocols for exposing performance counters")
  (description "VMs running on a Xen host can use this library to expose performance counters which can be sampled by the xapi performance monitoring daemon.")
  (authors "John Else")
  (depends
   ocaml
   cmdliner
   (rrd-transport (= :version))
   (xapi-idl (= :version))
   (xapi-rrd (= :version))
  )
)

(package
  (name xapi-open-uri)
)

(package
  (name xapi-networkd)
)

(package
  (name xapi-nbd)
)

(package
  (name xapi-log)
)

(package
  (name xapi-idl)
)

(package
  (name xapi-forkexecd)
)

(package
  (name xapi-expiry-alerts)
)

(package
  (name xapi-datamodel)
)

(package
  (name xapi-consts)
)

(package
  (name xapi-compression)
)

(package
  (name xapi-client)
)

(package
  (name xapi-cli-protocol)
)

(package
  (name xapi)
)

(package
  (name wsproxy)
)

(package
  (name vhd-tool)
)

(package
  (name vhd-format)
)

(package
  (name vhd-format-lwt)
)

(package
  (name varstored-guard)
)

(package
  (name uuid)
)

(package
  (name stunnel)
)

(package
  (name sexpr)
)

(package
  (name safe-resources)
)

(package
  (name rrddump)
)

(package
  (name rrdd-plugins)
)

(package
  (name rrd2csv)
)

(package
  (name rrd-transport)
)

(package
  (name pciutil)
)

(package
  (name message-switch-async)
)

(package
  (name message-switch-lwt)
)

(package
  (name message-switch-core)
  (synopsis "A simple store-and-forward message switch")
  (description "The switch stores messages in queues with well-known names. Clients use a simple HTTP protocol to enqueue and dequeue messages.")
  (depends
    astring
    (cohttp (>= "0.21.1"))
    ppx_deriving_rpc
    ppx_sexp_conv
    rpclib
    sexplib
    (xapi-log (= :version))
    (xapi-stdext-threads (= :version))
    (odoc :with-doc)
  )
)

(package
  (name message-switch-cli)
)

(package
  (name message-switch-unix)
)

(package
  (name message-switch)
)

(package
  (name http-lib)
)

(package
  (name gzip)
)

(package
  (name forkexec)
)

(package
  (name ezxenstore)
)

(package
  (name cohttp-posix)
)

(package
  (name xapi-rrd)
)

(package
  (name xapi-inventory)
)

(package
  (name xapi-stdext-date)
  (synopsis "Xapi's standard library extension, Dates")
  (depends
    (ocaml (>= 4.12))
    (alcotest :with-test)
    astring
    base-unix
    ptime
    (odoc :with-doc)
  )
)

(package
  (name xapi-stdext-encodings)
  (synopsis "Xapi's standard library extension, Encodings")
  (depends
    (ocaml (>= 4.13.0))
    (alcotest (and (>= 0.6.0) :with-test))
    (odoc :with-doc)
    (bechamel :with-test)
    (bechamel-notty :with-test)
    (notty :with-test)
  )
)

(package
  (name xapi-stdext-pervasives)
  (synopsis "Xapi's standard library extension, Pervasives")
  (depends
    (ocaml (>= 4.08))
    logs
    (odoc :with-doc)
    xapi-backtrace
  )
)

(package
  (name xapi-stdext-std)
  (synopsis "Xapi's standard library extension, Stdlib")
  (depends
    (ocaml (>= 4.08.0))
    (alcotest :with-test)
    (odoc :with-doc)
  )
)

(package
  (name xapi-stdext-threads)
  (synopsis "Xapi's standard library extension, Threads")
  (depends
    ocaml
    base-threads
    base-unix
    (odoc :with-doc)
    (xapi-stdext-pervasives (= :version))
  )
)

(package
  (name xapi-stdext-unix)
  (synopsis "Xapi's standard library extension, Unix")
  (depends
    (ocaml (>= 4.12.0))
    base-unix
    (fd-send-recv (>= 2.0.0))
    (odoc :with-doc)
    xapi-backtrace
    (xapi-stdext-pervasives (= :version))
  )
)

(package
  (name xapi-stdext-zerocheck)
  (synopsis "Xapi's standard library extension, Zerocheck")
  (depends
    ocaml
    (odoc :with-doc)
  )
)
