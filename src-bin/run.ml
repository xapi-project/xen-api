(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let pp_exec = Fmt.(quote string)

let blacklist = [ ".so"; ".cmxs" ] (* Don't try to run these kind of files *)

let exec_match exec p =
  OS.File.exists p >>= function
  | false -> Ok false
  | true ->
      OS.Path.Mode.get p >>= fun mode ->
      if mode land 0o111 = 0 then Ok false else
      let p_base, ext = Fpath.split_ext p in
      let p_base = Fpath.to_string p_base in
      let p = Fpath.to_string p in
      Ok (not (List.mem ext blacklist) &&
          (String.is_suffix ~affix:exec p_base ||
           String.is_suffix ~affix:exec p))

let find_exec exec dir =
  let ambiguous l =
    R.error_msgf "Ambiguous matches for %a, could match any of %a"
      pp_exec exec Fmt.(list ~sep:(unit ", ") Fpath.pp) l
  in
  OS.Dir.exists dir >>= function
  | false -> R.error_msgf "Build directory %a does not exist" Fpath.pp dir
  | true ->
      let elements = `Sat (exec_match exec) in
      OS.Dir.fold_contents ~elements (fun p ps -> p :: ps) [] dir
      >>= function
      | [p0] -> Ok p0
      | [p0; p1] as l ->
          let p0, ext0 = Fpath.split_ext p0 in
          let p1, ext1 = Fpath.split_ext p1 in
          if Fpath.equal p0 p1 && ext0 = ".native" || ext1 = ".native"
          then Ok (Fpath.add_ext ".native" p0)
          else ambiguous l
      | [] ->
          R.error_msgf "No matches for %a in build directory %a"
            Fmt.(quote string) exec Fpath.pp dir
      | l ->
          ambiguous l

let run () pkg_file build_dir exec args =
  let pkg = Topkg_care.Pkg.v pkg_file ?build_dir in
  begin
    Topkg_care.Pkg.build_dir pkg
    >>= fun build_dir -> find_exec exec build_dir
    >>= fun exec -> Ok Cmd.(v (p exec) %% of_list args)
    >>= fun cmd -> OS.Cmd.run_status cmd
    >>= function
    | `Exited 0 -> Ok 0
    | status ->
        Logs.err (fun m -> m "run %a %a"
                           Cmd.dump cmd OS.Cmd.pp_status status);
        Ok 1
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let args =
  let doc = "Arguments given to the executable. If options are being
             passed, needs to be specified after a -- token so that the
             command line options do not get interpreted by the $(tname)
             command itself."
  in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let exec =
  let doc = "Executable name or path suffix, with or without its
             extension. If multiple executable match the specification
             the command errors except if two paths match and differ
             only by their .byte and .native file extension. In the latter
             case the .native path is used."
  in
  let docv = "EXEC" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let doc = "Run built executables"
let sdocs = Manpage.s_common_options
let exits = Term.exit_info 1 ~doc:"on run non-zero status exit." :: Cli.exits
let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]... $(i,EXEC) \
        [-- [$(i,ARG)]...]]";
    `S Manpage.s_description;
    `P "The $(tname) command runs executable files found
        in the build directory.";
    `P "$(b,WARNING) The way this command works is subject to change
        in the future." ]

let cmd =
  Term.(pure run $ Cli.setup $ Cli.pkg_file $ Cli.build_dir $ exec $ args),
  Term.info "run" ~doc ~sdocs ~exits ~man ~man_xrefs

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
