(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup
open Cmdliner

(* Converters and arguments *)

let path_arg = Arg.conv Fpath.(of_string, pp)

let pkg_file =
  let doc = "Use $(docv) as the package description file." in
  let docv = "FILE" in
  Arg.(value & opt path_arg (Fpath.v "pkg/pkg.ml") &
       info ["pkg-file"] ~docs:Manpage.s_common_options ~doc ~docv)

let pkg_name =
  let doc = "The name $(docv) of the opam package. If absent provided
             by the package description."
  in
  let docv = "PKG_NAME" in
  Arg.(value & opt (some string) None & info ["n"; "pkg-name"] ~doc ~docv)

let opam =
  let doc = "The opam file to use. If absent uses the default opam file
             mentioned in the package description."
  in
  let docv = "FILE" in
  Arg.(value & opt (some path_arg) None & info ["opam"] ~doc ~docv)

let dist_name =
  let doc = "The name $(docv) of the package to use for the package
             distribution. If absent, provided by the package description."
  in
  let docv = "NAME" in
  Arg.(value & opt (some string) None & info ["dist-name"] ~doc ~docv)

let dist_version =
  let doc = "The version string to use for the package distribution.
             If absent, provided by the VCS tag description of the
             HEAD commit."
  in
  let docv = "VERSION" in
  Arg.(value & opt (some string) None & info ["dist-version"] ~doc ~docv)

let dist_file =
  let doc = "The package distribution archive. If absent the file
             $(i,BUILD_DIR)/$(i,NAME)-$(i,VERSION).tbz (see options
             $(b,--build-dir), $(b,--dist-name) and $(b,--dist-version))."
  in
  let docv = "FILE" in
  Arg.(value & opt (some path_arg) None & info ["dist-file"] ~doc ~docv)

let dist_opam =
  let doc = "opam file to use for the distribution. If absent uses the opam
             file mentioned in the package description that corresponds to
             the distribution package name $(i,NAME) (see option
             $(b,--dist-name))."
  in
  let docv = "FILE" in
  Arg.(value & opt (some path_arg) None & info ["dist-opam"] ~doc ~docv)

let dist_uri =
  let doc = "The distribution archive URI on the WWW. If absent, provided by the
             package description."
  in
  let docv = "URI" in
  Arg.(value & opt (some string) None & info ["dist-uri"] ~doc ~docv)

let readme =
  let doc = "The readme to use. If absent, provided by the package
             description."
  in
  let docv = "FILE" in
  Arg.(value & opt (some path_arg) None & info ["readme"] ~doc ~docv)

let change_log =
  let doc = "The change log to use. If absent, provided by the package
             description."
  in
  let docv = "FILE" in
  Arg.(value & opt (some path_arg) None & info ["change-log"] ~doc ~docv)

let delegate =
  let doc = "The delegate tool $(docv) to use. If absent, see topkg-delegate(7)
             for the lookup procedure."
  in
  let docv = "TOOL" in
  let to_cmd = function None -> None | Some s -> Some (Cmd.v s) in
  Term.(const to_cmd $
        Arg.(value & opt (some string) None & info ["delegate"] ~doc ~docv))

let build_dir =
  let doc = "Specifies the build directory $(docv). If absent, provided by the
             package description."
  in
  let docv = "BUILD_DIR" in
  Arg.(value & opt (some path_arg) None & info ["build-dir"] ~doc ~docv)

let publish_msg =
  let doc = "The publication message $(docv). Defaults to the change
             log of the last version (see $(b,topkg log -l))."
  in
  let docv = "MSG" in
  Arg.(value & opt (some string) None & info ["m"; "message"] ~doc ~docv)

(* Terms *)

let logs_to_topkg_log_level = function
| None -> None
| Some Logs.App -> Some (Topkg.Log.App)
| Some Logs.Error -> Some (Topkg.Log.Error)
| Some Logs.Warning -> Some (Topkg.Log.Warning)
| Some Logs.Info -> Some (Topkg.Log.Info)
| Some Logs.Debug -> Some (Topkg.Log.Debug)

let setup style_renderer log_level cwd =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Topkg.Log.set_level (logs_to_topkg_log_level log_level);
  Logs.set_level log_level;
  Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ());
  Logs.info (fun m -> m "topkg %%VERSION%% running");
  match cwd with
  | None -> `Ok ()
  | Some dir ->
      match OS.Dir.set_current dir with
      | Ok () -> `Ok ()
      | Error (`Msg m) -> `Error (false, m) (* use cmdliner evaluation error *)

let setup =
  let style_renderer =
    let env = Arg.env_var "TOPKG_COLOR" in
    Fmt_cli.style_renderer ~docs:Manpage.s_common_options ~env ()
  in
  let log_level =
    let env = Arg.env_var "TOPKG_VERBOSITY" in
    Logs_cli.level ~docs:Manpage.s_common_options ~env ()
  in
  let cwd =
    let doc = "Change to directory $(docv) before doing anything." in
    let docv = "DIR" in
    Arg.(value & opt (some path_arg) None & info ["C"; "pkg-dir"]
           ~docs:Manpage.s_common_options ~doc ~docv)
  in
  Term.(ret (const setup $ style_renderer $ log_level $ cwd))

(* Verbosity propagation. *)

let propagate_verbosity_to_pkg_file () = match Logs.level () with
| None -> Cmd.(v "-q")
| Some Logs.Info -> Cmd.(v "-v")
| Some Logs.Debug -> Cmd.(v "-v" % "-v")
| Some _ -> Cmd.empty

(* Error handling *)

let warn_if_vcs_dirty msg =
  Topkg.Vcs.get ()
  >>= fun repo -> Topkg.Vcs.is_dirty repo
  >>= function
  | false -> Ok ()
  | true ->
      Logs.warn
        (fun m -> m "The repo is %a. %a" Topkg_care.Pp.dirty () Fmt.text msg);
      Ok ()

let handle_error = function
| Ok 0 -> if Logs.err_count () > 0 then 3 else 0
| Ok n -> n
| Error _ as r -> Logs.on_error_msg ~use:(fun _ -> 3) r

let exits =
  Term.exit_info 3 ~doc:"on indiscriminate errors reported on stderr." ::
  Term.default_exits

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
