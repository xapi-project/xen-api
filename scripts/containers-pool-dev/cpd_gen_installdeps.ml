open Containergen

let () =
  match Sys.argv |> Array.to_list with
  | _argv0 :: repository :: commit :: packages ->
      Generate.stdout @@
      Dockerfile.(
      Depexts.install ~repository ~commit ~packages
      @@
      Opam.install_deps ())
  | _ ->
      invalid_arg "Needs 2 arguments"
