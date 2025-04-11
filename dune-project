(lang dune 3.15)

(formatting (enabled_for ocaml))
(using menhir 2.0)
(using directory-targets 0.1)
(opam_file_location inside_opam_directory)

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
    fmt
    mtime
    ptime
    (xapi-log (= :version))
    (qcheck-core :with-test)
    (qcheck-alcotest :with-test)
  )
)

(package
  (name tgroup)
  (depends
    xapi-log
    xapi-stdext-unix)
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
   (fmt :with-test)
   mustache
   (xapi-datamodel (= :version))
   (xapi-stdext-unix (and (= :version) :with-test))
   (xapi-test-utils :with-test)
  )
  (allow_empty)
)
(package
  (name xen-api-client-lwt)
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
  (name xapi-types)
)

(package
  (name xapi-tracing)
  (depends
   ocaml
   dune
   (alcotest :with-test)
   (fmt :with-test)
   ppx_deriving_yojson
   re
   uri
   (uuid :with-test)
   (xapi-log (= :version))
   (xapi-stdext-threads (= :version))
   yojson
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
   ptime
   result
   rresult
   rpclib
   ppx_deriving_rpc
   uri
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
   xenstore
   xenstore_transport
  )
)

(package
  (name xapi-open-uri)
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
  (name xapi-debug)
  (synopsis "Debugging tools for XAPI")
  (description "Tools installed into the non-standard /opt/xensource/debug location")
  (depends
    alcotest
    angstrom
    astring
    base64
    cmdliner
    cohttp
    cstruct
    ctypes
    domain-name
    fd-send-recv
    fmt
    hex
    integers
    ipaddr
    logs
    magic-mime
    mirage-crypto
    mirage-crypto-pk
    mirage-crypto-rng
    mtime
    pci
    polly
    ppx_deriving
    ppx_deriving_rpc
    ppx_sexp_conv
    psq
    ptime
    qcheck-alcotest
    qcheck-core
    re
    result
    rpclib
    rrdd-plugin
    rresult
    sexplib
    sexplib0
    sha
    tar
    tar-unix
    uri
    uuidm
    uutf
    x509
    xapi-backtrace
    xapi-log
    xapi-types
    xapi-stdext-pervasives
    xapi-stdext-unix
    xen-api-client
    xen-api-client-lwt
    xenctrl
    xenstore_transport
    xmlm
    yojson
  )
)

(package
  (name xapi-tools)
  (synopsis "Various daemons and CLI applications required by XAPI")
  (description "Includes message-switch, xenopsd, forkexecd, ...")
  (depends
    astring
    base64
    cmdliner
    cstruct-unix
    fmt
    logs
    lwt
    mtime
    netlink
    qmp
    re
    result
    rpclib
    rresult
    uri
    xenctrl
    xmlm
    yojson
    ; can't use '= version' here yet,
    ; 'xapi-tools' will have version ~dev, not 'master' like all the others
    ; because it is not in xs-opam yet
    rrd-transport
    rrdd-plugin
    xapi-tracing-export
    xen-api-client
    (alcotest :with-test)
    (ppx_deriving_rpc :with-test)
    (qcheck-core :with-test)
    (xapi-test-utils :with-test)
    (xenstore_transport :with-test)
  )
)

(package
  (name xapi)
  (synopsis "The toolstack daemon which implements the XenAPI")
  (description "This daemon exposes the XenAPI and is used by clients such as 'xe' and 'XenCenter' to manage clusters of Xen-enabled hosts.")
  (depends
    (ocaml (>= 4.09))
    (alcotest :with-test)
    angstrom
    astring
    base-threads
    base64
    bechamel
    (bos :with-test)
    cdrom
    (clock (= :version))
    cmdliner
    cohttp
    conf-pam
    (crowbar :with-test)
    cstruct
    ctypes
    ctypes-foreign
    domain-name
    (ezxenstore (= :version))
    fmt
    fd-send-recv
    hex
    (http-lib (and :with-test (= :version))) ; the public library is only used for testing
    integers
    ipaddr
    logs
    magic-mime
    mirage-crypto
    mirage-crypto-pk
    (mirage-crypto-rng (>= "0.11.0"))
    (message-switch-unix (= :version))
    mtime
    opentelemetry-client-ocurl
    pci
    (pciutil (= :version))
    polly
    ppx_deriving_rpc
    ppx_sexp_conv
    ppx_deriving
    psq
    ptime
    qcheck-alcotest
    qcheck-core
    re
    result
    rpclib
    (rrdd-plugin (= :version))
    rresult
    sexpr
    sexplib
    sexplib0
    sha
    (stunnel (= :version))
    tar
    tar-unix
    uri
    tgroup
    (uuid (= :version))
    uutf
    uuidm
    x509
    xapi-backtrace
    (xapi-client (= :version))
    (xapi-cli-protocol (= :version))
    (xapi-consts (= :version))
    (xapi-datamodel (= :version))
    (xapi-expiry-alerts (= :version))
    (xapi-idl (= :version))
    (xapi-inventory (= :version))
    (xapi-log (= :version))
    (xapi-stdext-pervasives (= :version))
    (xapi-stdext-std (= :version))
    (xapi-stdext-threads (= :version))
    (xapi-stdext-unix (= :version))
    (xapi-stdext-zerocheck (= :version))
    (xapi-test-utils :with-test)
    (xapi-tracing (= :version))
    (xapi-tracing-export (= :version))
    (xapi-types (= :version))
    (xen-api-client-lwt (= :version))
    xenctrl ; for quicktest
    xenstore_transport
    xmlm
    (xml-light2 (= :version))
    yojson
    (zstd (= :version))
  )
)

(package
  (name vhd-tool)
  (synopsis "Manipulate .vhd files")
  (tags ("org.mirage" "org:xapi-project"))
  (depends
    (alcotest-lwt :with-test)
    astring
    bigarray-compat
    cmdliner
    cohttp
    cohttp-lwt
    conf-libssl
    (cstruct (>= "3.0.0"))
    (ezxenstore (= :version))
    (forkexec (= :version))
    io-page
    lwt
    lwt_ssl
    nbd
    nbd-unix
    ppx_cstruct
    ppx_deriving_rpc
    re
    result
    rpclib
    ssl
    sha
    tar
    uri
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
    (ocaml (>= "4.10.0"))
    (alcotest :with-test)
    (alcotest-lwt (and :with-test (>= "1.0.0")))
    (bigarray-compat (>= "1.1.0"))
    (cstruct (>= "6.0.0"))
    cstruct-lwt
    (fmt :with-test)
    (lwt (>= "3.2.0"))
    (mirage-block (>= "3.0.0"))
    (rresult (>= "0.7.0"))
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
  (synopsis "Library used by xapi to herd stunnel processes")
  (description "This library allows xapi to configure, launch and terminate stunnel processes that act as clients and servers.")
  (depends
    astring
    (forkexec (= :version))
    (safe-resources (= :version))
    (uuid (= :version))
    (xapi-consts (= :version))
    xapi-inventory
    (xapi-log (= :version))
    (xapi-stdext-pervasives (= :version))
    (xapi-stdext-threads (= :version))
    (xapi-stdext-unix (= :version))
    (odoc :with-doc)
  )
)

(package
  (name sexpr)
)

(package
  (name safe-resources)
)

(package
  (name rrd-transport)
  (synopsis "Shared-memory protocols for exposing system metrics")
  (description "VMs running on a Xen host can use this library to expose performance counters which can be sampled by xapi's metric daemon.")
  (authors "John Else")
  (depends
    (alcotest :with-test)
    astring
    bigarray-compat
    cstruct
    crc
    (fmt :with-test)
    rpclib
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
    sexplib0
    uri
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
    (clock (= :version))
    fmt
    ipaddr
    mtime
    ppx_deriving_rpc
    (qcheck-core :with-test)
    rpclib
    (safe-resources(= :version))
    sha
    (stunnel (= :version))
    tgroup
    uri
    (uuid (= :version))
    xapi-backtrace
    (xapi-idl (= :version))
    (xapi-log (= :version))
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
    astring
    base-threads
    (fd-send-recv (>= "2.0.0"))
    ppx_deriving_rpc
    rpclib
    (uuid (= :version))
    xapi-backtrace
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
    ambient-context
    base-threads
    base-unix
    (alcotest :with-test)
    (clock (= :version))
    (fmt :with-test)
    mtime
    tgroup
    (xapi-log (= :version))
    (xapi-stdext-pervasives (= :version))
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
    astring
    base-unix
    (bisect_ppx :with-test)
    (clock (and (= :version) :with-test))
    (fd-send-recv (>= 2.0.0))
    fmt
    integers
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
