open Containergen
let () =
  Generate.stdout @@
  Dockerfile.(
    Opam.opam_from ()
    @@@ [ Command.run [Opam.install ["opam-monorepo"]]])
