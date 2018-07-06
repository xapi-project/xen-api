(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Actions *)

let show change_log last last_version no_pager =
  let text =
    if not (last || last_version) then OS.File.read change_log else
    (Topkg_care.Text.change_log_file_last_entry change_log
     >>= fun (v, (h, t)) -> Ok (if last_version then v else strf "%s\n%s" h t))
  in
  text
  >>= fun text -> Topkg_care.Text.find_pager ~don't:(no_pager || last_version)
  >>= function
  | None -> Logs.app (fun m -> m "%s" text); Ok ()
  | Some pager -> OS.Cmd.(in_string text |> run_in pager)

let commit change_log =
  let change_log = Fpath.to_string change_log in
  Topkg.Vcs.get ()
  >>= fun repo -> Topkg.Vcs.file_is_dirty repo change_log
  >>= function
  | true -> Topkg.Vcs.commit_files repo ~msg:"Update change log." [change_log]
  | false ->
      Logs.app (fun m -> m "No changes to commit in %s" change_log); Ok ()

(* Command *)

let log () pkg_file change_log action last last_version no_pager =
  begin
    let pkg = Topkg_care.Pkg.v ?change_log pkg_file in
    Topkg_care.Pkg.change_log pkg
    >>= fun change_log -> match action with
    | `Show -> show change_log last last_version no_pager >>= fun () -> Ok 0
    | `Edit -> Topkg_care.Text.edit_file change_log
    | `Commit -> commit change_log >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  let action = [ "show", `Show; "edit", `Edit; "commit", `Commit] in
  let doc = strf "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let cmd = Arg.enum action in
  Arg.(value & pos 0 cmd `Show & info [] ~doc ~docv:"ACTION")

let no_pager =
  let doc = "Do not pipe the output into a pager. This automatically
             happens if the TERM environment variable is 'dumb' or undefined."
  in
  Arg.(value & flag & info ["no-pager"] ~doc)

let last =
  let doc = "Show only the change log of the last version. Extracted as the
             first marked up section of the change log."
  in
  Arg.(value & flag & info ["l"; "last"] ~doc)

let last_version =
  let doc = "Show only the version string of the last version. Extracted as
             the first token of the title of the first marked up section of
             the change log. Implies $(b,--no-pager).";
  in
  Arg.(value & flag & info ["t"; "last-version"] ~doc)

let doc = "Show and edit the package's change log"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let envs =
  [ Term.env_info "EDITOR" ~doc:"The editor used to edit the change log.";
    Term.env_info "PAGER" ~doc:"The pager used to consult the change log.";
    Term.env_info "TERM" ~doc:"See option $(b,--no-pager)." ]

let man_xrefs = [ `Main; `Cmd "publish"; `Cmd "tag"; `Cmd "opam"; ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command shows, edits and commits
        the package's change log.";
    `S "CHANGE LOG FORMAT";
    `P "To be able to extract the version and changes of the last distribution,
        a well defined change log format is assumed. Not abiding to the
        format is not catastrophic but may hinder or derail some facilities
        provided by topkg.";
    `P "The format assumes that the change log is written either in Markdown
        (default or .md extension) or Asciidoc (.asciidoc or .adoc extension).
        A change log is a list of marked up sections. A section is
        a header of any level until the next header at the same level or
        the end of file. For example here are two Markdown sections:";
    `Pre "\
v2.0.0
------
### New features
etc.
### Breaking changes
etc.

v1.6.0 1995-09-12
-----------------
etc.";
    `P "The first marked up section in the file is taken as being the
        change log for the last distribution; use $(b,topkg log -l)
        to check that it is parsed correctly. This is used by topkg-publish(1)
        and topkg-opam(1) to enrich distribution publication.";
    `P "The first token of the first section header title is taken as being the
        version string of the distribution; use $(b,topkg log -t) to check
        that it is parsed correctly. It is used by topkg-tag(1) to tag the
        source repository.";
    `S "ACTIONS";
    `I ("$(b,show) (default)", "shows the package's change log.");
    `I ("$(b,edit)", "edit the package's change log.");
    `I ("$(b,commit)", "commit changes made to the package's change log to the
                        VCS.") ]

let cmd =
  Term.(pure log $ Cli.setup $ Cli.pkg_file $ Cli.change_log $ action $
        last $ last_version $ no_pager),
  Term.info "log" ~doc ~sdocs ~exits ~envs ~man

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
