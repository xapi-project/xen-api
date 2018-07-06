(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let absolute path = OS.Dir.current () >>| fun cwd -> Fpath.(cwd // path)

let gen_doc dir =
  let do_doc () =
    OS.Cmd.run Cmd.(v "topkg" % "doc")
    >>| fun () -> Fpath.(dir / "_build" / "doc" / "api.docdir")
  in
  R.join @@ OS.Dir.with_current dir do_doc ()

let publish_doc pkg =
  Topkg_care.Pkg.distrib_file pkg
  >>= fun distrib_file -> Topkg_care.Pkg.publish_msg pkg
  >>= fun msg -> Topkg_care.Archive.untbz ~clean:true distrib_file
  >>= fun dir -> gen_doc dir
  >>= fun docdir -> absolute docdir
  >>= fun docdir -> Topkg_care.Delegate.publish_doc pkg ~msg ~docdir

let publish_distrib pkg =
  Topkg_care.Pkg.distrib_file pkg
  >>= fun distrib_file -> Topkg_care.Pkg.publish_msg pkg
  >>= fun msg -> absolute distrib_file
  >>= fun archive -> Topkg_care.Delegate.publish_distrib pkg ~msg ~archive

let publish_alt pkg kind =
  Topkg_care.Pkg.distrib_file pkg
  >>= fun distrib_file -> Topkg_care.Pkg.publish_msg pkg
  >>= fun msg -> absolute distrib_file
  >>= fun archive -> Topkg_care.Delegate.publish_alt pkg ~kind ~msg ~archive

let publish ()
    pkg_file build_dir name version opam delegate change_log distrib_uri
    distrib_file publish_msg publish_artefacts
  =
  begin
    let publish_artefacts = match publish_artefacts with
    | [] -> None
    | v -> Some v
    in
    let pkg = Topkg_care.Pkg.v ?name ?version ?build_dir ?opam ?delegate
        ?change_log ?distrib_uri ?distrib_file ?publish_msg
        ?publish_artefacts pkg_file
    in
    let publish_artefact acc artefact =
      acc >>= fun acc -> match artefact with
      | `Doc -> publish_doc pkg
      | `Distrib -> publish_distrib pkg
      | `Alt kind -> publish_alt pkg kind
    in
    Topkg_care.Pkg.publish_artefacts pkg
    >>= fun todo -> List.fold_left publish_artefact (Ok ()) todo
    >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let artefacts =
  let alt_prefix = "alt-" in
  let parser = function
  | "do" | "doc" -> `Ok `Doc
  | "di" | "dis" | "dist" | "distr" | "distri" | "distrib" -> `Ok `Distrib
  | s when String.is_prefix alt_prefix s ->
      begin match String.(with_range ~first:(length alt_prefix) s) with
      | "" -> `Error ("`alt-' alternative artefact kind is missing")
      | kind -> `Ok (`Alt kind)
      end
  | s -> `Error (strf "`%s' unknown publication artefact" s)
  in
  let printer ppf = function
  | `Doc -> Fmt.string ppf "doc"
  | `Distrib -> Fmt.string ppf "distrib"
  | `Alt a -> Fmt.pf ppf "alt-%s" a
  in
  let artefact = parser, printer in
  let doc = strf "The artefact to publish. $(docv) must be one of `doc`,
                  `distrib` or `alt-$(i,KIND)`. If absent, the set of
                  default publication artefacts is determined by the
                  package description."
  in
  Arg.(value & pos_all artefact [] & info [] ~doc ~docv:"ARTEFACT")

let doc = "Publish package distribution archives and derived artefacts"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let envs =
  [ Term.env_info "TOPKG_DELEGATE" ~doc:"The package delegate to use, see
    topkg-delegate(7)."; ]

let man_xrefs = [`Main; `Cmd "distrib" ]
let man =
  [ `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]... [$(i,ARTEFACT)]...";
    `S Manpage.s_description;
    `P "The $(tname) command publishes package distribution archives
        and other artefacts via the package delegate. See topkg-delegate(7) for
        more details.";
    `P "Artefact publication always relies on a distribution archive having
        been generated before with topkg-distrib(1).";
    `S "ARTEFACTS";
    `I ("$(b,distrib)",
        "Publishes a distribution archive on the WWW.");
    `I ("$(b,doc)",
        "Publishes the documentation of a distribution archive on the WWW.");
    `I ("$(b,alt)-$(i,KIND)",
        "Publishes the alternative artefact of kind $(i,KIND) of
         a distribution archive. The semantics of alternative artefacts
         is left to the delegate, it could be anything, an email,
         a pointless tweet, a feed entry etc. See topkg-delegate(7) for
         more details."); ]

let cmd =
  Term.(pure publish $ Cli.setup $ Cli.pkg_file $ Cli.build_dir $
        Cli.dist_name $ Cli.dist_version $ Cli.dist_opam $
        Cli.delegate $ Cli.change_log $ Cli.dist_uri $ Cli.dist_file $
        Cli.publish_msg $ artefacts),
  Term.info "publish" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs

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
