(lang dune 2.7)
(formatting (enabled_for ocaml))
(name xapi-stdext)

(generate_opam_files true)

(source (github xapi-project/stdext))
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(authors "Jonathan Ludlam")
(maintainers "Xapi project maintainers")

(package
  (name xapi-stdext)
  (synopsis "Xapi's standard library extension")
  (description "Dummy package that enables the usage of dune-release")
  (depends
    (xapi-stdext-date (= :version))
    (xapi-stdext-encodings (= :version))
    (xapi-stdext-pervasives (= :version))
    (xapi-stdext-std (= :version))
    (xapi-stdext-threads (= :version))
    (xapi-stdext-unix (= :version))
    (xapi-stdext-zerocheck (= :version))
  )
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
    ocaml
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
    ocaml
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
    ocaml
    base-unix
    (fd-send-recv (>= 2.0.0))
    (odoc :with-doc)
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
