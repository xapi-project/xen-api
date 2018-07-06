(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let extract_version change_log =
  Topkg_care.Text.change_log_file_last_entry change_log
  >>= fun (version, _) -> Ok version

let vcs_tag tag ~commit_ish ~force ~sign ~delete ~msg =
  let msg = match msg with None -> strf "Distribution %s" tag | Some m -> m in
  Topkg.Vcs.get ()
  >>= fun repo -> match delete with
  | true -> Topkg.Vcs.delete_tag repo tag
  | false ->
      Topkg.Vcs.tag repo ~force ~sign ~msg ~commit_ish tag >>| fun () ->
      Logs.app (fun m -> m "Tagged version %a" Topkg_care.Pp.version tag)

let tag () pkg_file change_log tag commit_ish force sign delete msg =
  begin
    let pkg = Topkg_care.Pkg.v ?change_log pkg_file in
    let tag = match tag with
    | Some t -> Ok t
    | None -> Topkg_care.Pkg.change_log pkg >>= fun cl -> extract_version cl
    in
    tag
    >>= fun tag -> vcs_tag tag ~commit_ish ~force ~sign ~delete ~msg
    >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let version =
  let doc = "The version tag to use. If absent, automatically extracted
             from the package's change log; see topkg-log(1) for details."
  in
  Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"VERSION")

let commit =
  let doc = "Commit-ish $(docv) to tag." in
  Arg.(value & opt string "HEAD" & info ["commit"] ~doc ~docv:"COMMIT-ISH")

let msg =
  let doc = "Commit message for the tag. If absent, the message
             'Distribution $(i,VERSION)' is used."
  in
  Arg.(value & opt (some string) None & info ["m"; "message"] ~doc ~docv:"MSG")

let sign =
  let doc = "Sign the tag using the VCS's default signing key." in
  Arg.(value & flag & info ["s"; "sign"] ~doc)

let force =
  let doc = "If the tag exists, replace it rather than fail." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let delete =
  let doc = "Delete the specified tag rather than create it." in
  Arg.(value & flag & info ["d"; "delete"] ~doc)

let doc = "Tag the package's source repository with a version"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [ `Main; `Cmd "log" ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command tags the package's VCS HEAD commit with a
        version. If the version is not specified on the command line it is
        automatically extracted from the package's change log; use
        $(b,topkg log -t) to check the extracted value." ]

let cmd =
  Term.(pure tag $ Cli.setup $ Cli.pkg_file $ Cli.change_log $
        version $ commit $ force $ sign $ delete $ msg),
  Term.info "tag" ~doc ~sdocs ~exits ~man ~man_xrefs

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
