(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Targets *)

let opam_doc_field =
  "doc", `Opam "doc", "doc opam file field"

let opam_homepage_field =
  "homepage", `Opam "homepage", "homepage opam file field"

let opam_issues_field =
  "issues", `Opam "bug-reports", "bug-reports opam file field"

let opam_repo_field =
  "repo", `Opam "dev-repo", "dev-repo opam file field"

let topkg_api =
  "topkg-api", `Uri "%%PKG_DOC%%", "topkg's API docs"

let ocaml_man =
  "ocaml-man", `Uri "http://caml.inria.fr/pub/docs/manual-ocaml/",
  "OCaml manual"

let ocaml_issues =
  "ocaml-issues", `Uri "http://caml.inria.fr/mantis/", "OCaml issue tracker"

let ocamlbuild_man =
  "ocamlbuild-man",
  `Uri "https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc",
  "OCamlbuild manual"

let opam_man =
  "opam-man", `Uri "http://opam.ocaml.org/doc/Manual.html", "opam manual"

let packages =
  "packages", `Uri "http://opam.ocaml.org/packages/", "OCaml opam repository"

let planet =
  "planet", `Uri "https://ocaml.org/community/planet/", "OCaml Planet"

let temptation =
  "temptation",
  `Uri "https://www.\x2568\x2561\x2573\x256B\x2565\x256C\x256C.org", ""

let caml_list =
  "caml-list", `Uri "http://news.gmane.org/gmane.comp.lang.caml.inria",
  "Main OCaml mailing list"

let weekly_news =
  "weekly-news", `Uri "http://alan.petitepomme.net/cwn/", "OCaml Weekly News"

let targets =
  [ opam_doc_field; opam_homepage_field; opam_issues_field; opam_repo_field;
    topkg_api; ocaml_man; ocaml_issues; ocamlbuild_man; opam_man; packages;
    planet; temptation; caml_list; weekly_news; ]

let parse_target, max_target_len =
  let add (acc, len) (t, v, _) = (t, v) :: acc, max len (String.length t) in
  let index, max = List.fold_left add ([], 0) targets in
  fst (Cmdliner.Arg.enum index) (* This gives us trie lookup *) , max

(* opam field uris *)

let opam_field_uri opam field =
  Topkg_care.Opam.File.fields opam
  >>= fun fields -> match String.Map.find field fields with
  | Some (uri :: _) -> Ok uri
  | Some [] -> R.error_msgf "%a: field %s is empty" Fpath.pp opam field
  | None -> R.error_msgf "%a: field %s is undefined" Fpath.pp opam field

(* Browse command *)

let browse () pkg_file opam browser prefix background target =
  begin
    let uri = match parse_target target with
    | `Ok (`Uri uri) -> Ok uri
    | `Ok (`Opam field) ->
        let pkg = Topkg_care.Pkg.v ?opam pkg_file in
        Topkg_care.Pkg.opam pkg >>= fun opam -> opam_field_uri opam field
    | `Error msg ->
        let uri_prefixes = ["http://"; "https://"; "file://"] in
        if List.exists (fun p -> String.is_prefix p target) uri_prefixes
        then Ok target
        else Error (`Msg msg)
    in
    uri
    >>= fun uri -> Webbrowser.reload ~background ~prefix ?browser uri
    >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let target =
  let doc = "Target to browse, see above for the list of targets." in
  Arg.(value & pos 0 string "homepage" & info [] ~doc ~docv:"TARGET or URI")

let doc = "Browse the package's WWW links"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [ `Main ]
let man =
  let target acc (t, _, doc) =
    if doc = "" then acc else
    let pad = String.v ~len:(max_target_len - String.length t) (fun _ -> ' ') in
    `Pre (strf "%s$(b,%s) %s" pad t doc) :: `Noblank :: acc
  in
  [ `S Manpage.s_description;
    `P "The $(tname) command opens or reloads URIs mentioned in the
        opam file in a WWW browser. A few other useful logical target are
        provided and arbitrary file, http or https schemed URIs can also
        be specified as the target.";
    `Blocks (List.(tl @@ rev @@ fold_left target [] targets)); ]

let cmd =
  Term.(pure browse $ Cli.setup $ Cli.pkg_file $ Cli.opam $
        Webbrowser_cli.browser $ Webbrowser_cli.prefix $
        Webbrowser_cli.background $ target),
  Term.info "browse" ~doc ~sdocs ~exits ~man ~man_xrefs

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
