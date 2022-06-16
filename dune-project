(lang dune 1.11)
(using fmt 1.2 (enabled_for ocaml))
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
    ocaml
    (dune (>= 1.11))
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
    (dune (>= 1.11))
    (alcotest :with-test)
    (odoc :with-doc)
  )
)

(package
  (name xapi-stdext-pervasives)
  (synopsis "Xapi's standard library extension, Pervasives")
  (depends
    ocaml
    (dune (>= 1.11))
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
    (dune (>= 1.11))
    (alcotest :with-test)
    (odoc :with-doc)
  )
)

(package
  (name xapi-stdext-threads)
  (synopsis "Xapi's standard library extension, Threads")
  (depends
    ocaml
    (dune (>= 1.11))
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
    (dune (>= 1.11))
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
    (dune (>= 1.11))
    (odoc :with-doc)
  )
)
