(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let unixy_path p =
  (* ocamlbuild doesn't like Windows paths it seems. Try to do our best here. *)
  let volume, p = Fpath.split_volume p in
  volume ^ (String.concat ~sep:"/" (Fpath.segs p))

let copy_assets src_dir dst_dir =
  let copy_asset dst_dir file = match Fpath.get_ext file with
  | ".css" | ".svg" | ".svgz" | ".png" | ".jpeg" | ".gif" | ".woff" | ".ttf"
  | ".otf" | ".eot" ->
      begin OS.File.exists file >>= function
      | false -> Ok ()
      | true ->
          OS.File.read file
          >>= fun cont -> OS.File.write Fpath.(dst_dir / filename file) cont
      end
      |> Logs.on_error_msg ~use:(fun () -> ())
  | _ -> ()
  in
  OS.Dir.exists src_dir >>= function
  | false -> Ok ()
  | true  ->
      OS.Dir.contents src_dir
      >>= fun files -> List.iter (copy_asset dst_dir) files; Ok ()

let copy_odig_css doc_dir dst_dir =
  OS.File.exists Fpath.(doc_dir / "style.css") >>= function
  | true -> Ok ()
  | false ->
      let get_odig_etc = Cmd.(v "opam" % "config" % "var" % "odig:etc") in
      OS.Cmd.(run_out get_odig_etc |> to_string) >>= function
      | "#undefined" (* no comment *) -> Ok ()
      | etcdir ->
          Fpath.of_string etcdir >>= fun etcdir ->
          OS.File.read Fpath.(etcdir / "ocamldoc.css")
          >>= fun cont -> OS.File.write Fpath.(dst_dir / "style.css") cont

let doc_build_args pkg_name build_dir dev target =
  let verb = Cli.propagate_verbosity_to_pkg_file () in
  let pkg_name = Cmd.(v "--pkg-name" % pkg_name) in
  let build_dir = Cmd.(v "--build-dir" % Cmd.p build_dir) in
  let target = unixy_path target in
  let doc_flags = ["-docflags"; "-colorize-code,-charset,utf-8"; target ] in
  let raws = Cmd.of_list ~slip:"--raw" doc_flags in
  Cmd.(verb %% pkg_name %% build_dir %% raws)

let build_doc pkg pkg_name build_dir dev =
  let out = OS.Cmd.to_stdout in
  let doc_dir = Fpath.v "doc" in
  let odocl = Fpath.(doc_dir / (if dev then "dev.odocl" else "api.odocl")) in
  Ok Fpath.(set_ext ".docdir" odocl / "index.html")
  >>= fun target -> Ok (doc_build_args pkg_name build_dir dev target)
  >>= fun args -> OS.Dir.current ()
  >>= fun dir -> Topkg_care.Pkg.build pkg ~dir ~args ~out
  >>= fun () -> Ok Fpath.(build_dir // parent target)
  >>= fun dst_dir -> copy_assets doc_dir dst_dir
  >>= fun () -> copy_odig_css doc_dir dst_dir
  >>= fun () -> Ok dst_dir

let browser_reload reload ~background ~browser dir =
  OS.Dir.current ()
  >>= fun cwd -> Ok Fpath.(cwd // dir)
  >>= fun abs_dir -> match not (reload || background) with
  | true -> Ok abs_dir
  | false ->
      let uri = strf "file://%s" Fpath.(to_string abs_dir) in
      Webbrowser.reload ~background ~prefix:true ?browser uri
      >>= fun () -> Ok abs_dir

let doc_cmd () pkg_file name build_dir dev reload background browser =
  begin
    let pkg = Topkg_care.Pkg.v ?build_dir ?name pkg_file in
    Topkg_care.Pkg.name pkg
    >>= fun pkg_name -> Topkg_care.Pkg.build_dir pkg
    >>= fun build_dir -> build_doc pkg pkg_name build_dir dev
    >>= fun docdir -> browser_reload reload ~background ~browser docdir
    >>= fun abs_docdir ->
    Logs.app (fun m ->
        m "Generated %s doc in %a"
          (if dev then "dev" else "API") Topkg_care.Pp.path abs_docdir);
    Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let reload_browser =
  let doc = "Open an URI of the documentation directory or reload an
             existing browser tab that holds a sub-page of the documentation."
  in
  Arg.(value & flag & info ["r"; "reload-browser"] ~doc)

let dev =
  let doc = "Build the development documentation." in
  Arg.(value & flag & info ["d"; "dev"] ~doc)

let doc = "Build the package's API documentation"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command builds the package's API documentation. Use
        the option $(b,-r) to open or refresh the documentation in
        a WWW browser (see $(b,--browser) for details).";
    `P "$(b,WARNING.) The way this command works is at the
        moment very ad-hoc and ocamlbuild specific. It will
        change in the future.";
    `P "Current support relies on having a doc/ directory at the root of the
        distribution. The ocamlbuild file doc/api.odocl defines the API
        documentation and the doc/dev.odocl the development documentation.
        The directory can also hold CSS, PNG, JPEG, GIF, SVG, WOFF, TTF, OTF
        files that are copied over to the generated documentation directory.";
    `P "The package's build system is invoked via `--raw` with the
        ocamlbuild documentation targets.";
    `P "If the doc/ directory has no doc/style.css file but odig(1) is
        installed, its ocamldoc stylesheet is used." ]

let cmd =
  Term.(pure doc_cmd $ Cli.setup $ Cli.pkg_file $ Cli.pkg_name $ Cli.build_dir
        $ dev $ reload_browser $ Webbrowser_cli.background $
        Webbrowser_cli.browser),
  Term.info "doc" ~doc ~sdocs ~exits ~man ~man_xrefs

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
