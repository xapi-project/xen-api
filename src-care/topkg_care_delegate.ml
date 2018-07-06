(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Running the delegate *)

let run_delegate pkg args =
  let verbosity = Logs.level_to_string (Logs.level ()) in
  Topkg_care_pkg.delegate pkg
  >>= fun del -> Ok Cmd.(del % "ipc" % verbosity %% args)
  >>= fun cmd -> OS.Cmd.run_status cmd
  >>= function
  | `Exited 0 -> Ok ()
  | `Exited 1 ->
      R.error_msgf "Action unsupported by delegate %a" Cmd.pp del
  | (`Exited n | `Signaled n) ->
      R.error_msgf "Delegate %a errored with %d" Cmd.pp del n

(* Publish request *)

let publish_distrib p ~msg ~archive =
  Topkg_care_pkg.name p
  >>= fun name -> Topkg_care_pkg.version p
  >>= fun version -> Topkg_care_pkg.distrib_uri p
  >>= fun distrib_uri ->
  run_delegate p Cmd.(v "publish" % "distrib" % distrib_uri %
                      name % version % msg % p archive)

let publish_doc p ~msg ~docdir =
  let doc_uri p = Topkg_care_pkg.opam_field_hd p "doc" >>= function
  | None -> Ok ""
  | Some uri -> Ok uri
  in
  Topkg_care_pkg.name p
  >>= fun name -> Topkg_care_pkg.version p
  >>= fun version -> doc_uri p
  >>= fun doc_uri ->
  run_delegate p Cmd.(v "publish" % "doc" % doc_uri % name % version % msg %
                      p docdir)

let publish_alt p ~kind ~msg ~archive =
  Topkg_care_pkg.name p
  >>= fun name -> Topkg_care_pkg.version p
  >>= fun version -> Topkg_care_pkg.distrib_uri p
  >>= fun distrib_uri ->
  run_delegate p Cmd.(v "publish" % "alt" % distrib_uri % kind %
                      name % version % msg % p archive)

let publish_in_git_branch ~remote ~branch ~name ~version ~docdir ~dir =
  let pp_distrib ppf (name, version) =
    Fmt.pf ppf "%a %a"
      Topkg_care_text.Pp.name name Topkg_care_text.Pp.version version
  in
  let log_publish_result msg distrib dir =
    Logs.app (fun m -> m "%s %a@ in@ directory@ %a@ of@ gh-pages@ branch"
                 msg pp_distrib distrib Fpath.pp dir)
  in
  let cp src dst =
    let dst_is_root = Fpath.is_current_dir dst in
    let src =
      if dst_is_root then Fpath.to_dir_path src else Fpath.rem_empty_seg src
    in
    (* FIXME we lost Windows friends here, fix bos #30 *)
    OS.Cmd.run Cmd.(v "cp" % "-R" % p src % p dst)
  in
  let delete dir =
    if not (Fpath.is_current_dir dir) then OS.Dir.delete ~recurse:true dir else
    let delete acc p = acc >>= fun () -> OS.Path.delete ~recurse:true p in
    let gitdir = Fpath.v ".git" in
    let not_git p = not (Fpath.equal p gitdir) in
    OS.Dir.contents dir
    >>= fun files -> List.fold_left delete (Ok ()) (List.filter not_git files)
  in
  let git_for_repo r = Cmd.of_list (Topkg.Cmd.to_list @@ Topkg.Vcs.cmd r) in
  let replace_dir_and_push docdir dir =
    let msg = strf "Update %s doc to %s." name version in
    Topkg.Vcs.get ()
    >>= fun repo -> Ok (git_for_repo repo)
    >>= fun git -> OS.Cmd.run Cmd.(git % "checkout" % branch)
    >>= fun () -> delete dir
    >>= fun () -> cp docdir dir
    >>= fun () -> Topkg.Vcs.is_dirty repo
    >>= function
    | false -> Ok false
    | true ->
        OS.Cmd.run Cmd.(git % "add" % p dir)
        >>= fun () -> OS.Cmd.run Cmd.(git % "commit" % "-m" % msg)
        >>= fun () -> OS.Cmd.run Cmd.(git % "push")
        >>= fun () -> Ok true
  in
  if not (Fpath.is_rooted ~root:Fpath.(v ".") dir)
  then
    R.error_msgf "%a directory is not rooted in the repository or not relative"
      Fpath.pp dir
  else
  let clonedir = Fpath.(parent docdir / strf "%s-%s.pubdoc" name version) in
  OS.Dir.delete ~recurse:true clonedir
  >>= fun () -> Topkg.Vcs.get ()
  >>= fun repo -> Topkg.Vcs.clone repo ~dir:(Fpath.to_string clonedir)
  >>= fun () -> OS.Dir.with_current clonedir (replace_dir_and_push docdir) dir
  >>= fun res -> res
  >>= function
  | false (* no changes *) ->
      log_publish_result "No documentation changes for" (name, version) dir;
      Ok ()
  | true ->
      let push_spec = strf "%s:%s" branch branch in
      Ok (git_for_repo repo) >>= fun git ->
      OS.Cmd.run Cmd.(git % "push" % remote % push_spec)
      >>= fun () -> OS.Dir.delete ~recurse:true clonedir
      >>= fun () ->
      log_publish_result "Published documentation for" (name, version) dir;
      Ok ()

(* Issue requests *)

let issues_uri p = Topkg_care_pkg.opam_field p "dev-repo" >>| function
| None | Some [] -> ""
| Some (u :: _) -> u

let issue_list p =
  issues_uri p >>= fun issues_uri ->
  run_delegate p Cmd.(v "issue" % "list" % issues_uri)

let issue_show p ~id =
  issues_uri p >>= fun issues_uri ->
  run_delegate p Cmd.(v "issue" % "show" % issues_uri % "id")

let issue_open p ~title ~body =
  issues_uri p >>= fun issues_uri ->
  match run_delegate p Cmd.(v "issue" % "open" % issues_uri % title % body) with
  | Ok _ as v -> v
  | Error _ as e ->
      let pp_body ppf = function "" -> () | body -> Fmt.pf ppf "@,@,%s" body in
      Logs.app (fun m -> m "@[<v>Your open issue message was:@,---@,%s%a@]"
                   title pp_body body);
      e

let issue_close p ~id ~msg =
  issues_uri p >>= fun issues_uri ->
  match run_delegate p Cmd.(v "issue" % "close" % issues_uri % id % msg) with
  | Ok _ as v -> v
  | Error _ as e ->
      Logs.app (fun m -> m "@[<v>Your closing message was:@,---@,%s@]" msg);
      e

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
