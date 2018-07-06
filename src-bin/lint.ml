(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let lint () pkg_file ignore_pkg lints =
  begin
    let pkg = Topkg_care.Pkg.v pkg_file in
    OS.Dir.current ()
    >>= fun dir -> Topkg_care.Pkg.lint ~ignore_pkg pkg ~dir lints
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let lints =
  let test = [ "custom", `Custom;
               "std-files", `Std_files;
               "meta", `Meta;
               "opam", `Opam;
               "deps", `Deps; ]
  in
  let doc = strf "Test to perform. $(docv) must be one of %s. If unspecified
                  all tests are performed." (Arg.doc_alts_enum test)
  in
  let test = Arg.enum test in
  let docv = "TEST" in
  Arg.(value & pos_all test Topkg_care.Pkg.lint_all & info [] ~doc ~docv)

let ignore_pkg =
  let doc = "Ignore package description file." in
  Arg.(value & flag & info ["i"; "ignore-pkg"] ~doc)

let doc = "Check package distribution consistency and conventions"
let sdocs = Manpage.s_common_options
let exits = Term.exit_info 1 ~doc:"on lint failure" :: Cli.exits
let man_xrefs = [`Main; `Cmd "distrib"]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command makes tests on a package distribution or
        source repository. It checks that standard files exist, that
        ocamlfind META files pass the ocamlfind lint test, that opam package
        files pass the opam lint test and that the opam dependencies are
        consistent with those of the build system.";
    `P "Linting is automatically performed on distribution generation, see
        topkg-distrib(1) for more details." ]

let cmd =
  Term.(pure lint $ Cli.setup $ Cli.pkg_file $ ignore_pkg $ lints),
  Term.info "lint" ~doc ~sdocs ~exits ~man ~man_xrefs


(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
