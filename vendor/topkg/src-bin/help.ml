(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let topkg_manual = "Topkg manual"
let version = "%%VERSION%%"

(* Help manuals *)

open Cmdliner

let see_also ~cmds =
  let cmds = (Astring.String.concat ~sep:"(1), " ("topkg" :: cmds)) ^ "(1)" in
  [ `S Manpage.s_see_also; `P cmds ]

let release =
  ("TOPKG-RELEASE", 7, "", version, topkg_manual),
  [ `S Manpage.s_name;
    `P "topkg-release - How to release a (topkg) package";
    `S Manpage.s_description;
    `P "The basic release script is the following. Each step is
        refined and explained with more details below.";
    `Pre "\
topkg browse issues # Review remaining outstanding issues
topkg status        # Review the changes since last version
topkg log edit      # Write the release notes
topkg log commit    # Commit the release notes
topkg tag           # Tag the distribution with a version
topkg distrib       # Create the distribution archive
topkg publish       # Publish it on the WWW with its documentation
topkg opam pkg      # Create an opam package
topkg opam submit   # Submit it to OCaml's opam repository";
    `P "The last four steps can be performed via a single invocation
        to topkg-bistro(1).";
    `S "BASIC CHECKS";
    `P "First have a look at the outstanding issues the package may have
        by checking the issue tracker.";
    `Pre "topkg browse issues";
    `P "If the package's delegate supports issue tracker interaction
        (see topkg-delegate(7)), you can consult them directly in the
        terminal with:";
    `Pre "topkg issue list";
    `P "Basic checks are performed on the distribution archive when it is
        created, but save time by catching errors early. Hence test that
        your source repository lints and that it builds in the current build
        environment and that the package tests pass.";
    `Pre "\
topkg lint
topkg build # Check out the generated opam install file too
topkg test";
    `S "WRITE THE RELEASE NOTES";
    `P "Carefully write the release notes in the package's change log, these
        are essential time savers for users of the package. It may help to
        consult the list of changes that were committed since the last VCS
        version tag with:";
    `Pre "topkg status";
    `P "You can then write the release notes and commit them to the VCS with:";
    `Pre "\
topkg log edit
topkg log commit";
    `P "The next step is simplified if the change log follows a certain
        format, see topkg-log(1) for details.";
    `P "The last two commands mentioned perform no magic, it is entirely up
        to you to use them or not. The first one simply opens the change log
        of the package in your \\$EDITOR and the second one commits it to
        your VCS with a dull, canned, commit message.";
    `S "VCS TAG THE RELEASE";
    `P "Here again topkg provides a magic-less command that will simply
        extract the latest version tag from the package's change log
        and tag the VCS HEAD commit with it:";
    `Pre "topkg tag";
    `P "This will only work if the change log follows a certain format,
        see tokpg-log(1) for details. You can check the extracted tag is
        the one you wish before with:";
    `Pre "topkg log -t";
    `P "If you do not want to rely on topkg's broken extraction algorithms
        just specify it on the command line:";
    `Pre "topkg tag v1.0.1";
    `P "And if you really think topkg does a bad job at this, simply
        use your VCS directly to tag a release.";
    `S "CREATE THE DISTRIBUTION ARCHIVE AND PUBLISH IT";
    `P "Now that the release is tagged in your VCS, generate a distribution
        archive for it in the build directory with:";
    `Pre "topkg distrib";
    `P "This uses the source tree of the HEAD commit for creating a
        distribution in the build directory. The distribution version
        string is the VCS tag description (e.g.  git-describe(1)) of
        the HEAD commit. Alternatively it can be specified on the command
        line.";
    `P "If everything went well you can now publish the distribution and
        its documentation on the WWW. The exact actions that happen here
        depend on the package's delegate, see topkg-delegate(7) for more
        details.";
    `Pre "topkg publish";
    `P "The distribution is now public. It may already have been picked up
        by other systems hence do not try to alter the archive and
        republish it with a different bit-stream after that point (if
        you are tempted to do this please consider taking a functional
        programming course). At worst 410 the archive from
        the WWW. But in most cases, if there is a problem with the
        archive, simply leave it there and publish a new one with an
        increased patch version number.";
    `S "SUBMIT TO OCAML'S OPAM REPOSITORY";
    `P "The following steps still need the distribution archive created in
        the preceeding step to be in the build directory. If that's no
        longer the case but nothing moved in your VCS, you can simply
        invoke $(b,topkg distrib), it should produce a bit-wise identical
        archive. If the VCS moved checkout the distribution commit to
        regenerate the archive or provide, in the subsequent commands,
        the archive manually via the $(b,--dist-file) option, see
        topkg-opam(1) for details.";
    `P "To add the package to OCaml's opam repository, start by creating an
        opam package description in the build directory with:";
    `Pre "topkg opam pkg";
    `P "then simply submit it to the opam repository with:";
    `Pre "topkg opam submit";
    `P "The latter does nothing more than invoking opam-publish-submit(1) on
        the package description generated earlier.";
    `P "Congratulations. You are done. Ditch your computer.";
    `S "TROUBLESHOOTING";
    `P "Here are a few troubleshooting scenarios and possible resolution.";
    `I ("Before publishing",
        "Anything that happens before the $(b,topkg publish) step,
         like a failing $(b,topkg distrib), is easy to resolve. Delete the
         version tag of your VCS, a $(b,topkg tag -d) will do, add
         some commits, adjust your release notes and start over.");
    `I ("opam submission build failure",
        "If the build failure is due to a missing constraint, follow the
         instruction of the next item to correct the opam file. If the failure
         is due to a defect in the distribution archive, call it a day and
         start over with a patch version release that corrects the problem.
         Do not try to reuse the version string of the failed release, other
         systems may already have picked up the broken archive.");
    `I ("opam repository maintainer and robot complaints",
        "These pesky but loved maintainers and robots... If they
         complain about certain aspects of your opam submission, you can either
         try to correct it manually from the opam package description found
         in the build directory and reinvoke $(b,topkg opam submit) or edit
         the opam file of the source repository and regenerate the opam Package
         description with $(b,topkg opam pkg) and the $(b,--pkg-opam)
         option. Note that if the VCS moved meanwhile you may have to use
         the various command line options of topkg-opam(1) to make sure
         you point to the right package version and distribution archive.
         In either case you should be aware that there will be a mismatch
         between the opam file in the distribution archive and the one
         you submitted to the opam repository. If this happens to be a
         problem, start over with a new patch version release.");
    `Blocks (see_also ~cmds:[]); ]

let delegate =
  ("TOPKG-DELEGATE", 7, "", version, topkg_manual),
  [ `S Manpage.s_name;
    `P "topkg-delegate - The topkg delegate";
    `S Manpage.s_description;
    `P "The delegate of a package is a program invoked by topkg to perform
        actions that are difficult or not desirable to standardize within
        topkg itself, namely:";
    `I ("$(b,topkg publish)",
        "Publish distribution archives and derived artefacts.");
    `I ("$(b,topkg issue)", "Interact with the package's issue tracker.");
    `P "A sample delegate is provided at the end of this man page.";
    `S "DELEGATE LOOKUP PROCEDURE";
    `P "The delegate used by a package is defined by the first match in the
        following lookup procedure.";
    `I ("1. Command line", "Specified with the $(b,--delegate) option on
         the command line.");
    `I ("2. Package description.", "Specified in the package description file,
         see topkg's API documentation.");
    `I ("3. Environment variable.", "Specified in the TOPKG_DELEGATE
         environment variable.");
    `I ("4. Homepage derived discovery.", "Consult the 'homepage' field of the
        package's opam file, extract the second-level domain of the URI as
        \\$NAME and uses the tool $(b,\\$NAME-topkg-delegate) iff it exists
        in the executable search path. For example if the homepage is
        http://www.example.org/mypackage, an existing
        $(b,example-topkg-delegate) tool will be used.");
    `I ("5. Transitory toy github fallback.", "If the previous step yields
         $(b,github-topkg-delegate) but that it doesn't exist in the
         executable search path. The $(b,toy-github-topkg-delegate) tool
         distributed with topkg-care is used. This tool will disappear in
         the future whenever a good github delegate emerges. Consult
         $(b,toy-github-topkg-delegate --help) for more information.");
    `S "DELEGATE PROTOCOL";
    `P "The delegate is invoked by $(b,topkg) with a request in order to
        finish its own execution. This means that the delegate takes over
        $(b,topkg)'s standard channels and is in charge until the end of
        execution (except on errors, see below).";
    `P "The delegate always gets information as command line arguments with
        file paths arguments given as absolute paths. The first argument is
        always 'ipc' and is followed by a verbosity parameter:";
    `P "my-topkg-delegate ipc $(i,VERB) $(i,ARG) ...";
    `P "$(i,VERB) will be either `quiet', `error', `warning', `info' or
        `debug' and the delegate must adjust its logging level appropriately.
        The remaining arguments are the request, see below for requests
        made by $(b,topkg).";
    `P "The delegate must always exit with one of the following exit codes:";
    `I ("0", "The request is successful."); `Noblank;
    `I ("1", "The request is unsupported."); `Noblank;
    `I ("2", "The request errored.");
    `P "Exit 0 must be returned iff the request could be fulfilled according
        to its semantics.";
    `P "Exit 1 must be returned iff the request arguments cannot be
        understood or if the request is not implemented by the delegate.
        In this case the delegate must produce no output.";
    `P "Exit 2 must be returned iff the request could not be fulfilled
        according to its semantics. In this case it is the delegate's duty
        to provide good error messages for diagnosis on standard output";
    `P "In both non-zero exit codes, it is not the delegate's duty to
        try to save request data. In these cases $(b,topkg) will take over
        again in order to prevent user input data loss. This
        occurs for example on issue creation, so that the issue
        description the user may have input interactively is not
        lost but \"saved\" to standard output.";
    `S "PUBLISH DELEGATION";
    `P "Publish delegation requests have the form:";
    `P "publish $(i,ACTION) $(i,ARG)...";
    `P "The following actions are currently defined.";
    `I ("publish distrib $(i,DISTRIB_URI) $(i,NAME) $(i,VERSION)
         $(i,MSG) $(i,ARCHIVE)",
        "Publish the distribution archive file $(i,ARCHIVE) for the package
         named $(i,NAME) at version $(i,VERSION) with publication
         message $(i,MSG). See topkg API's documentation
         for information about the value of $(i,DISTRIB_URI).");
    `I ("publish doc $(i,DOC_URI) $(i,NAME) $(i,VERSION) $(i,MSG) $(i,DOCDIR)",
        "Publish the documentation directory $(i,DOCDIR) for the package
         named $(i,NAME) at version $(i,VERSION) with publication message
         $(i,MSG). $(i,DOC_URI) has the value of the doc field of the
         package's opam file.");
    `I ("publish alt $(i,DISTRIB_URI) $(i,KIND) $(i,NAME) $(i,VERSION)
         $(i,MSG) $(i,ARCHIVE)",
        "Alternative publication artefact named $(i,KIND). The semantics
         of the action is left to the delegate. The request arguments
         are the same as those of the distrib action.");
    `S "ISSUE DELEGATION";
    `P "Issue delegation requests have the form:";
    `P "issue $(i,ACTION) $(i,ISSUES_URI) $(i,ARG) ...";
    `P "with $(i,ISSUES_URI) the value of the bug-reports field of the
        package's opam file or \"\" if there is no such field.";
    `P "The following actions are currently defined.";
    `I ("issue list $(i,ISSUES_URI)",
        "List open issues on standard output. Each issue should be on its
         own line with the format '$(i,ID) $(i,TITLE)'.");
    `I ("issue show $(i,ISSUES_URI) $(i,ID)",
        "Show details about issue $(i,ID) on standard output.");
    `I ("issue open $(i,ISSUES_URI) $(i,TITLE) $(i,MSG)",
        "Create an issue with title $(i,TITLE) and description $(i,MSG)
         (may be an empty string). If the request is successful the
         delegate should communicate the resulting issue identifier in some
         way on standard output.");
    `I ("issue close $(i,ID) $(i,MSG)",
        "Close issue $(i,ID) with closing message $(i,MSG).");
    `S "SAMPLE UNSUPPORTIVE DELEGATE";
    `P "This delegate script can be used as a blueprint. All requests
        are simply unsupported.";
`Pre "\
#!/usr/bin/env ocaml
#use \"topfind\"
#require \"bos.setup\"
open Bos_setup

let unsupported = Ok 1

let publish = function
| \"distrib\" :: uri :: name :: version :: msg :: archive :: _ ->
    unsupported
| \"doc\" :: uri :: name :: version :: msg :: docdir :: _ ->
    unsupported
| \"alt\" :: kind :: uri :: name :: version :: msg :: archive :: _ ->
    unsupported
| args ->
    unsupported

let issue = function
| \"list\" :: uri :: _ -> unsupported
| \"show\" :: uri :: id :: _ -> unsupported
| \"open\" :: uri :: title :: descr :: _ -> unsupported
| \"close\" :: uri :: id :: msg :: _ -> unsupported
| args -> unsupported

let request = function
| \"publish\" :: args -> publish args
| \"issue\" :: args -> issue args
| args -> unsupported

let main () =
  let doc = \"the unsupportive delegate\" in
  begin match OS.Arg.(parse ~doc ~pos:string ()) with
  | \"ipc\" :: verbosity :: req ->
      Logs.level_of_string verbosity
      >>= fun level -> Logs.set_level level; request req
  | \"ipc\" :: [] ->
      R.error_msg \"malformed delegate request, verbosity is missing\"
  | args ->
      R.error_msgf \"unknown arguments: %s\" (String.concat ~sep:\" \" args)
  end
  |> Logs.on_error_msg ~use:(fun () -> 2)

let () = exit (main ())
";
    `Blocks (see_also ~cmds:["topkg-issue"; "topkg-publish"]); ]

let troubleshoot =
  ("TOPKG-TROUBLESHOOT", 7, "", version, topkg_manual),
  [ `S Manpage.s_name;
    `P "topkg-troubleshoot - A few troubleshooting tips";
    `S Manpage.s_description;
    `P "If you get into trouble try the following to get a better undersanding
        of what is happening.";
    `S "ASK FOR MORE LOGGING";
    `P "Invoke $(b,topkg) with $(b,-v), $(b,-v -v), or use the
        TOPKG_VERBOSITY environment variable; see the $(b,--verbosity)
        option.";
    `P "Messages comming from the $(b,topkg) tool are prefixed
        by 'topkg:' while those comming from the package description are
        prefixed by its base name, usually 'pkg.ml:'.";
    `S "DEBUG THE GENERATED OPAM INSTALL FILE";
    `P "To debug the generated opam install file according to the build
        configuration you don't need to build the package. Use the
        $(b,--dry-run) (or $(b,-d)) option and add a little bit of logging to
        output the build configuration that was determined:";
    `Pre "pkg/pkg.ml build -d -v [OPTION]...";`Noblank;
    `Pre "topkg build -d -v [OPTION]...      # mostly equivalent";
    `S "DEBUG DEV PACKAGE INSTALLS";
     `P "If you need more information about what happens when dev packages
         are installed (VCS pins or VCS packages) in opam, for example the
         actual watermark values, invoke opam as follows:";
    `P "TOPKG_VERBOSITY=debug opam upgrade mypkg -v";
    `S "RELEASE PROCESS TROUBLES";
    `P "See the TROUBLESHOOTING section of topkg-release(7).";
    `Blocks (see_also ~cmds:[]) ]

(* Help command *)

let pages =
  [ "release", release;
    "delegate", delegate;
    "troubleshoot", troubleshoot; ]

let help man_format topic commands = match topic with
| None -> `Help (man_format, None)
| Some topic ->
    let topics = "topics" :: commands @ (List.map fst pages) in
    let topics = List.sort compare topics in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when List.mem t commands -> `Help (man_format, Some t)
    | `Ok t when t = "topics" ->
        Fmt.pr "@[<v>%a@]@." Fmt.(list string) topics;
        `Ok 0
    | `Ok t ->
        let man = try List.assoc t pages with Not_found -> assert false in
        Fmt.pr "%a" (Cmdliner.Manpage.print man_format) man;
        `Ok 0

(* Command line interface *)

open Cmdliner

let topic =
  let doc = "The topic to get help on, `topics' lists the topic." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)

let doc = "Show help about topkg"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [`Main]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command shows help about $(mname).";
    `P "Use `topics' as $(i,TOPIC) to get a list of topics." ]

let cmd =
  Term.(ret (const help $ Term.man_format $ topic $ Term.choice_names)),
  Term.info "help" ~doc ~exits ~man ~man_xrefs

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
