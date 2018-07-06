(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let get_id = function
| Some id -> Ok id
| None -> R.error_msgf "No issue ID specified"

let get_issue_msg ~info = function
| Some "" -> Ok None
| Some msg -> Ok (Some (String.cuts ~sep:"\n" msg))
| None ->
    let is_msg s = not (String.is_prefix "#" s) in
    let rec rem_white_prefix = function
    | l :: ls when String.for_all Char.Ascii.is_white l -> rem_white_prefix ls
    | ls -> ls
    in
    OS.File.tmp "topkg-issue-msg-%s"
    >>= fun f -> OS.File.write f info
    >>= fun () -> Topkg_care.Text.edit_file f
    >>= function
    | 0 ->
        OS.File.read f >>= fun m ->
        let msg = List.filter is_msg (String.cuts ~sep:"\n" m) in
        begin match rem_white_prefix msg with
        | [] -> Ok None
        | lines -> Ok (Some lines)
        end
    | n ->
        Logs.err (fun m -> m "Editor exited with non-zero error code.");
        Ok None

(* Actions *)

let issue_show pkg ~id =
  get_id id >>= fun id -> Topkg_care.Delegate.issue_show pkg ~id

let issue_open pkg msg =
  let open_info =
    "\n\
     # Please enter an issue description. The first non-blank line will be\n\
     # the issue title and the rest the issue description. Lines starting\n\
     # with '#' will be ignored. An empty description aborts the action."
  in
  get_issue_msg ~info:open_info msg >>= function
  | None ->
      Logs.app (fun m -> m "Open issue aborted due to empty issue message.");
      Ok ();
  | Some lines ->
      let title, body = match lines with
      | title :: body -> title, String.(trim @@ concat ~sep:"\n" body)
      | [] -> assert false
      in
      Topkg_care.Delegate.issue_open pkg ~title ~body

let issue_close pkg ~id msg =
  let close_info =
    "\n\
     # Please enter a closing message. Lines starting with '#' will\n\
     # be ignored. An empty message aborts the action."
  in
  get_id id
  >>= fun id -> get_issue_msg ~info:close_info msg
  >>= function
  | None ->
      Logs.app
        (fun m -> m "Close issue %s aborted due to empty issue message." id);
      Ok ()
  | Some lines ->
      let msg = String.(trim @@ concat ~sep:"\n" lines) in
      Topkg_care.Delegate.issue_close pkg ~id ~msg

(* Command *)

let issue () pkg_file opam delegate action id msg =
  begin
    let pkg = Topkg_care.Pkg.v ?opam ?delegate pkg_file in
    let ret = match action with
    | `List -> Topkg_care.Delegate.issue_list pkg
    | `Show -> issue_show pkg ~id
    | `Open -> issue_open pkg msg
    | `Close -> issue_close pkg ~id msg
    in
    ret >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  let action = ["list",`List; "show",`Show; "open",`Open; "close",`Close;] in
  let doc = strf "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let cmd = Arg.enum action in
  Arg.(value & pos 0 cmd `List & info [] ~doc ~docv:"ACTION")

let id =
  let doc = "An issue ID of the package's issue tracker." in
  Arg.(value & pos 1 (some string) None & info [] ~doc ~docv:"ID")

let msg =
  let doc = "For $(b,open) and $(b,close), $(docv) is the issue message.
             Prevents the interactive prompt for the message."
  in
  let docv = "MSG" in
  Arg.(value & opt (some string) None & info ["m"; "message"] ~doc ~docv)

let doc = "Interact with the package's issue tracker"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let envs =
  [ Term.env_info "EDITOR" ~doc:"The editor used to edit issue messages.";
    Term.env_info "TOPKG_DELEGATE" ~doc:"The package delegate to use, see
    topkg-delegate(7)." ]

let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]... [$(i,ACTION)]...";
    `S Manpage.s_description;
    `P "The $(tname) command interacts with the package's issue
        tracker via the package delegate. See topkg-delegate(7) for more
        details.";
    `P "To consult the issues in a WWW browser invoke
        $(b,topkg browse issues), no delegate is needed for this.";
    `S "ACTIONS";
    `I ("$(b,list) (default)",
        "List open issues.");
    `I ("$(b,show) $(i,ID)",
        "Show information about issue $(i,ID).");
    `I ("$(b,open)",
        "Open a new issue.");
    `I ("$(b,close) $(i,ID)",
        "Close issue $(i,ID).") ]

let cmd =
  Term.(pure issue $ Cli.setup $ Cli.pkg_file $ Cli.opam $ Cli.delegate $
        action $ id $ msg),
  Term.info "issue" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs


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
