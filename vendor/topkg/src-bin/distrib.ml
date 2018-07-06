(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let lint_distrib pkg ~dir =
  Logs.app (fun m -> m "@.Linting distrib in %a" Fpath.pp dir);
  Topkg_care.Pkg.lint pkg ~dir Topkg_care.Pkg.lint_all

let build_distrib pkg ~dir skip_tests =
  Logs.app (fun m -> m "@.Building package in %a" Fpath.pp dir);
  let tests = if skip_tests then Cmd.empty else Cmd.(v "--tests" % "true") in
  let args = Cmd.(v "--dev-pkg" % "false" % "--vcs" % "false" %% tests) in
  let out = OS.Cmd.out_string in
  Topkg_care.Pkg.build pkg ~dir ~args ~out >>= function
  | (_, (_, `Exited 0)) ->
      Logs.app (fun m -> m "%a package builds" Topkg_care.Pp.status `Ok); Ok 0
  | (stdout, _) ->
      Logs.app (fun m -> m "%s@\n%a package builds"
                   stdout Topkg_care.Pp.status `Fail); Ok 1

let test_distrib pkg ~dir =
  Logs.app (fun m -> m "@.Running package tests in %a" Fpath.pp dir);
  let out = OS.Cmd.out_string in
  Topkg_care.Pkg.test pkg ~dir ~args:Cmd.empty ~out >>= function
  | (_, (_, `Exited 0)) ->
      Logs.app (fun m -> m "%a package tests"
                   Topkg_care.Pp.status `Ok); Ok 0
  | (stdout, _) ->
      Logs.app (fun m -> m "%s@\n%a package tests"
                   stdout Topkg_care.Pp.status `Fail); Ok 1

let check_archive pkg ar ~skip_lint ~skip_build ~skip_tests =
  Topkg_care.Archive.untbz ~clean:true ar
  >>= fun dir -> (if skip_lint then Ok 0 else lint_distrib pkg ~dir)
  >>= fun c0 -> (if skip_build then Ok 0 else build_distrib pkg ~dir skip_tests)
  >>= fun c1 -> (if skip_tests || skip_build then Ok 0 else
                 test_distrib pkg ~dir)
  >>= fun c2 -> match c0 + c1 + c2 with
  | 0 -> OS.Dir.delete ~recurse:true dir >>= fun () -> Ok 0
  | n -> Ok 1

let warn_if_vcs_dirty ()=
  Cli.warn_if_vcs_dirty "The distribution archive may be inconsistent."

let log_footprint pkg archive =
  Topkg_care.Pkg.name pkg
  >>= fun name -> Topkg_care.Pkg.version pkg
  >>= fun version -> Topkg.Vcs.get ()
  >>= fun repo -> Topkg.Vcs.commit_id repo ~dirty:false ~commit_ish:"HEAD"
  >>= fun commit_ish ->
  Logs.app
    (fun m -> m "@.@[<v>@[Distribution for %a@ %a@]@,@[Commit %a@]@,\
                 @[Archive %a@]@]"
        Topkg_care.Pp.name name Topkg_care.Pp.version version
        Topkg_care.Pp.commit commit_ish Topkg_care.Pp.path archive);
  Ok ()

let log_wrote_archive ar =
  Logs.app (fun m -> m "Wrote archive %a" Topkg_care.Pp.path ar); Ok ()

let distrib
    () pkg_file opam build_dir name version keep_dir skip_lint skip_build
    skip_tests
  =
  begin
    let pkg = Topkg_care.Pkg.v ?name ?version ?build_dir ?opam pkg_file in
    Topkg_care.Pkg.distrib_archive pkg ~keep_dir
    >>= fun ar -> log_wrote_archive ar
    >>= fun () -> check_archive pkg ar ~skip_lint ~skip_build ~skip_tests
    >>= fun errs -> log_footprint pkg ar
    >>= fun () -> warn_if_vcs_dirty ()
    >>= fun () -> Ok errs
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let keep_build_dir =
  let doc = "Keep the distribution build directory after successful archival."
  in
  Arg.(value & flag & info ["keep-build-dir"] ~doc)

let skip_lint =
  let doc = "Do not lint the archive distribution." in
  Arg.(value & flag & info ["skip-lint"] ~doc)

let skip_build =
  let doc = "Do not try to build the package from the archive." in
  Arg.(value & flag & info ["skip-build"] ~doc)

let skip_tests =
  let doc = "Do not try to build and run the package tests from the archive.
             Implied by $(b,--skip-build)."
  in
  Arg.(value & flag & info ["skip-tests"] ~doc)

let doc = "Create a package distribution archive"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let envs =
  [ Term.env_info "TOPKG_BZIP2" ~doc:"The $(b,bzip2) tool to use to compress the
    archive. Gets the archive on stdin and must output the result on
    standard out.";
    Term.env_info "TOPKG_TAR" ~doc:"The $(b,tar) tool to use to unarchive a tbz
    archive (archive creation itself is handled by topkg)."; ]

let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command creates a package distribution
        archive in the build directory of the package.  The generated
        archive should be bit-wise reproducible. There are however a few
        caveats, see the section about this further down.";
    `P "More detailed information about the archive creation process and its
        customization can be found in topkg's API documentation.";
    `P "Once the archive is created it is unpacked in the build directory,
        linted and the package is built using the package description
        contained in the archive. The build will use the default package
        configuration so it may fail in the current environment
        without this necessarily implying an actual problem with the
        distribution; one should still worry about it though.
        These checks can be prevented by using the $(b,--skip-lint) and
        $(b,--skip-build) options.";
    `S "REPRODUCIBLE DISTRIBUTION ARCHIVES";
    `P "Given the package name, the HEAD commit identifier
        and the version string, the $(tname) command should always
        generate the same archive.";
    `P "More precisely, files are added to the archive using a well
        defined order on path names. Their file permissions are either
        0o775 for directories and files that are executable for the user
        in the HEAD repository checkout or 0o664 for those that are not.
        Their modification times are set to the commit date (note that if
        git is used, git-log(1) shows the author date which may not
        coincide). No other file metadata is recorded.";
    `P "This should ensure that the resulting archive is bit-wise
        identical regardless of the context in which it is
        created. However this may fail for one or more of the
        following reasons:";
    `I ("Non-reproducible distribution massage", "The package
         distribution massaging hook relies on external factors
         that are not captured by the source repository checkout.
         For example external data files, environment variables, etc.");
    `I ("File paths with non US-ASCII characters",
        "If these paths are encoded in UTF-8, different file systems
         may return the paths with different Unicode normalization
         forms which could yield different byte serializations in the
         archive (note that this could be lifted at the cost of a
         dependency on Uunf).");
    `I ("The bzip2 utility", "The archive is compressed using the bzip2 utility.
         Reproducibility relies on bzip2 to be a reproducible function
         across platforms.");
    `I ("Topkg changes", "Topkg could change its distribution procedure in
         the future, for example to correct bugs."); ]

let cmd =
  Term.(pure distrib $ Cli.setup $ Cli.pkg_file $ Cli.dist_opam $
        Cli.build_dir $ Cli.dist_name $ Cli.dist_version $ keep_build_dir $
        skip_lint $ skip_build $ skip_tests),
  Term.info "distrib" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs

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
