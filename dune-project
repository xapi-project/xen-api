(lang dune 3.15)

(formatting (enabled_for ocaml))
(using menhir 2.0)

(cram enable)
(implicit_transitive_deps false)
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
  (name clock)
  (synopsis "Xapi's library for managing time")
  (authors "Jonathan Ludlam" "Pau Ruiz Safont")
  (depends
    (ocaml (>= 4.12))
    (alcotest :with-test)
    astring
    mtime
    ptime
    (xapi-log (= :version))
    (qcheck-core :with-test)
    (qcheck-alcotest :with-test)
  )
)

(package
  (name xapi-rrdd-plugin)
)

(package
  (name xml-light2)
)

(package
  (name xapi-sdk)
  (license "BSD-2-Clause")
  (synopsis "Xen API SDK generation code")
  (depends
   (alcotest :with-test)
   astring
   mustache
   (xapi-datamodel (= :version))
   (xapi-stdext-unix (and (= :version) :with-test))
  )
  (allow_empty)
)
(package
  (name xen-api-client-lwt)
)


(package
  (name xen-api-client-async)
)

(package
  (name xen-api-client)
  (synopsis "Xen-API client library for remotely-controlling a xapi host")
  (authors "David Scott" "Anil Madhavapeddy" "Jerome Maloberti" "John Else" "Jon Ludlam" "Thomas Sanders" "Mike McClurg")
  (depends
   (alcotest :with-test)
   astring
   (cohttp (>= "0.22.0"))
   re
   rpclib
   uri
   (uuid (= :version))
   (xapi-client (= :version))
   (xapi-idl (= :version))
   (xapi-rrd (= :version))
   (xapi-types (= :version))
   xmlm
  )
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
   (alcotest :with-test)
   astring
   (gzip (= :version))
   (http-lib (= :version))
   inotify
   io-page
   mtime
   ppx_deriving_rpc
   rpclib
   (ezxenstore (= :version))
   (uuid (= :version))
   xapi-backtrace
   (xapi-idl (= :version))
   (xapi-rrd (= :version))
   (xapi-stdext-threads (= :version))
   (xapi-stdext-unix (= :version))
   xapi-tracing
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
  (authors "Jon Ludlam")
  (synopsis "The XCP networking daemon")
  (depends
    (alcotest :with-test)
    astring
    base-threads
    (forkexec (= :version))
    (http-lib (= :version))
    mtime
    netlink
    re
    rpclib
    (xapi-idl (= :version))
    xapi-inventory
    (xapi-stdext-pervasives (= :version))
    (xapi-stdext-std (= :version))
    (xapi-stdext-threads (= :version))
    (xapi-stdext-unix (= :version))
    xapi-test-utils
    (xen-api-client (= :version))
  )
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
  (synopsis "Sub-process control service for xapi")
  (description "This daemon creates and manages sub-processes on behalf of xapi.")
  (depends
   astring
   (forkexec (= :version))
   (uuid (= :version))
   (xapi-stdext-unix (= :version))
  )
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
  (synopsis "The toolstack daemon which implements the XenAPI")
  (description "This daemon exposes the XenAPI and is used by clients such as 'xe' and 'XenCenter' to manage clusters of Xen-enabled hosts.")
  (depends
    alcotest ; needed for the quicktest binary
    angstrom
    base-threads
    base64
    cdrom
    conf-pam
    (crowbar :with-test)
    ctypes
    ctypes-foreign
    domain-name
    (ezxenstore (= :version))
    (fmt :with-test)
    hex
    (http-lib (and :with-test (= :version))) ; the public library is only used for testing
    ipaddr
    mirage-crypto
    mirage-crypto-pk
    (mirage-crypto-rng (>= "0.11.0"))
    (message-switch-unix (= :version))
    mtime
    opentelemetry-client-ocurl
    pci
    (pciutil (= :version))
    ppx_deriving_rpc
    ppx_sexp_conv
    ppx_deriving
    psq
    qcheck-alcotest
    rpclib
    (rrdd-plugin (= :version))
    rresult
    sexpr
    sha
    (stunnel (= :version))
    tar
    tar-unix
    (uuid (= :version))
    x509
    (xapi-client (= :version))
    (xapi-cli-protocol (= :version))
    (xapi-consts (= :version))
    (xapi-datamodel (= :version))
    (xapi-expiry-alerts (= :version))
    (xapi-idl (= :version))
    (xapi-inventory (= :version))
    (xapi-log (= :version))
    (xapi-stdext-date (= :version))
    (xapi-stdext-pervasives (= :version))
    (xapi-stdext-std (= :version))
    (xapi-stdext-threads (= :version))
    (xapi-stdext-unix (= :version))
    (xapi-stdext-zerocheck (= :version))
    (xapi-test-utils :with-test)
    (xapi-tracing (= :version))
    (xapi-types (= :version))
    (xapi-xenopsd (= :version))
    (xml-light2 (= :version))
    yojson
    (zstd (= :version))
  )
)

(package
  (name wsproxy)
  (synopsis "Websockets proxy for VNC traffic")
  (authors "Jon Ludlam" "Marcello Seri")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (alcotest :with-test)
    (base64 (>= "3.1.0"))
    fmt
    logs
    (lwt (>= "3.0.0"))
    re
    uuid
    (qcheck-core :with-test)
  )
)

(package
  (name vhd-tool)
  (synopsis "Manipulate .vhd files")
  (tags ("org.mirage" "org:xapi-project"))
  (depends
    (alcotest-lwt :with-test)
    cohttp-lwt
    conf-libssl
    (cstruct (>= "3.0.0"))
    (ezxenstore (= :version))
    (forkexec (= :version))
    io-page
    lwt
    nbd-unix
    ppx_cstruct
    ppx_deriving_rpc
    re
    rpclib
    sha
    tar
    (vhd-format (= :version))
    (vhd-format-lwt (= :version))
    (xapi-idl (= :version))
    (xapi-log (= :version))
    (xen-api-client-lwt (= :version))
    xenstore
    xenstore_transport
  )
)

(package
  (name vhd-format)
)

(package
  (name vhd-format-lwt)
  (synopsis "Lwt interface to read/write VHD format data")
  (description "A pure OCaml library to read and write
[vhd](http://en.wikipedia.org/wiki/VHD_(file_format)) format data, plus a
simple command-line tool which allows vhd files to be interrogated,
manipulated, format-converted and streamed to and from files and remote
servers.

This package provides an Lwt compatible interface to the library.")
  (authors "Jon Ludlam" "Dave Scott")
  (maintainers "Dave Scott <dave@recoil.org>")
  (tags ("org:mirage" "org:xapi-project"))
  (homepage "https://github.com/mirage/ocaml-vhd")
  (source (github mirage/ocaml-vhd))
  (depends
    (ocaml (and (>= "4.02.3") (< "5.0.0")))
    (alcotest :with-test)
    (alcotest-lwt :with-test)
    (cstruct (< "6.1.0"))
    (lwt (>= "3.2.0"))
    (mirage-block (>= "2.0.1"))
    (vhd-format (= :version))
    (io-page (and :with-test (>= "2.4.0")))
  )
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
  (synopsis "Shared-memory protocols for exposing system metrics")
  (description "VMs running on a Xen host can use this library to expose performance counters which can be sampled by xapi's metric daemon.")
  (authors "John Else")
  (depends
    (alcotest :with-test)
    astring
    cstruct
    crc
    yojson
    (xapi-idl (= :version))
    (xapi-rrd (= :version))
    (odoc :with-doc)
  )
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
  (synopsis "A simple store-and-forward message switch")
  (description "The switch stores messages in queues with well-known names. Clients use a simple HTTP protocol to enqueue and dequeue messages.")
  (depends
    base-threads
    cohttp
    (message-switch-core (= :version))
    ppx_deriving_rpc
    rpclib
    (xapi-stdext-threads (= :version))
  )
)

(package
  (name message-switch)
)

(package
  (name http-lib)
  (synopsis "An HTTP required used by xapi")
  (description "This library allows xapi to perform varios activities related to the HTTP protocol.")
  (depends
    (alcotest :with-test)
    astring
    (base64 (>= "3.1.0"))
    rpclib
    (safe-resources(= :version))
    sha
    (stunnel (= :version))
    (uuid (= :version))
    xapi-backtrace
    (xapi-idl (= :version))
    (xapi-log (= :version))
    (xapi-stdext-date (= :version))
    (xapi-stdext-pervasives (= :version))
    (xapi-stdext-threads (= :version))
    (xapi-tracing (= :version))
    (xml-light2 (= :version))
    (odoc :with-doc)
  )
)

(package
  (name gzip)
)

(package
  (name forkexec)
  (synopsis "Process-spawning library")
  (description "Client and server library to spawn processes.")
  (depends
    base-threads
    (fd-send-recv (>= "2.0.0"))
    ppx_deriving_rpc
    rpclib
    (uuid (= :version))
    (xapi-log (= :version))
    (xapi-stdext-pervasives (= :version))
    (xapi-stdext-unix (= :version))
    (xapi-tracing (= :version))
  )
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
  (authors "Jonathan Ludlam")
  (depends
    (clock (= :version))
    ptime
  )
)

(package
  (name xapi-stdext-encodings)
  (synopsis "Xapi's standard library extension, Encodings")
  (authors "Jonathan Ludlam")
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
  (authors "Jonathan Ludlam")
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
  (authors "Jonathan Ludlam")
  (depends
    base-threads
    base-unix
    (odoc :with-doc)
    (xapi-stdext-pervasives (= :version))
    (mtime :with-test)
    (xapi-stdext-unix (= :version))
  )
)

(package
  (name xapi-stdext-unix)
  (synopsis "Xapi's standard library extension, Unix")
  (authors "Jonathan Ludlam")
  (depends
    (ocaml (>= 4.12.0))
    (alcotest :with-test)
    base-unix
    (bisect_ppx :with-test)
    (fd-send-recv (>= 2.0.0))
    fmt
    (mtime (and (>= 2.0.0) :with-test))
    (logs :with-test)
    (qcheck-core (and (>= 0.21.2) :with-test))
    (odoc :with-doc)
    xapi-backtrace
    unix-errno
    (xapi-stdext-pervasives (= :version))
    polly
  )
)

(package
  (name xapi-stdext-zerocheck)
  (synopsis "Xapi's standard library extension, Zerocheck")
  (authors "Jonathan Ludlam")
  (depends
    (odoc :with-doc)
  )
)
