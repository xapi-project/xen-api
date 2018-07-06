(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* Help *)

let exec = match Array.length Sys.argv with
| 0 -> Filename.basename Sys.executable_name
| n -> Filename.basename Sys.argv.(0)

let usage () = Topkg_string.strf "Usage: %s COMMAND [OPTION]..." exec
let try_help () =
  Topkg_string.strf "%s\nTry `%s --help' for more information." (usage ()) exec

let pp_help ppf () =
  let prf = Format.fprintf in
  prf ppf "@[<v>@,";
  prf ppf "Commands:@,";
  prf ppf "  build [OPTION]...@,";
  prf ppf "      @[Build the package, see `Build options' below.@]@,";
  prf ppf "  test [OPTION]... [TEST]... [-- [ARG]...]@,";
  prf ppf "      @[Run all or the given built package tests, \
                   see `Test options' below.@]@,";
  prf ppf "  clean [OPTION]...@,";
  prf ppf "      @[Clean the package build, see `Clean options' below.@]@,";
  prf ppf "  help@,";
  prf ppf "      @[Show this help.@]@,";
  prf ppf "  ipc VERBOSITY [ARG]...@,";
  prf ppf "      @[Interprocess communication with topkg care tools.@]@,@,";
  prf ppf "Options:@,";
  prf ppf "  -h, -help, --help@,";
  prf ppf "      Show this help.@,";
  prf ppf "  -q, --quiet@,";
  prf ppf "      Be quiet. Takes over -v.@,";
  prf ppf "  -v, --verbose (absent=warning or TOPKG_VERBOSITY env)@,";
  prf ppf "      Increase verbosity. Repeatable, more than twice is useless.@,";
  prf ppf "  --version@,";
  prf ppf "      Show topkg's version information.@,@,";
  prf ppf "Build options:@,";
  prf ppf "  -d, --dry-run@,";
  prf ppf "      @[Do not run build instructions,@ only@ determine@ and@ \
                   write@ the@ opam@ install@ file.@]@,";
  prf ppf "  -r ARG, --raw ARG (repeatable)@,";
  prf ppf "      @[Do not run build instructions or write@ the@ opam@ \
                   install@ file,@ only@ invoke@ the build@ system@ with@ \
                   the given ARG argument.@]@,@,";
  prf ppf "  @[%a@]@,@," Topkg_conf.pp_keys_cli_opts ();
  prf ppf "Test options:@,";
  prf ppf "  --build-dir BUILD_DIR (absent=discovered)@,";
  prf ppf "      @[Specifies the build directory BUILD_DIR.@]@,";
  prf ppf "  -l, --list@,";
  prf ppf "      @[Do not run the tests, list them.@]@,@,";
  prf ppf "Clean options:@,";
  prf ppf "  --build-dir BUILD_DIR (absent=discovered)@,";
  prf ppf "      @[Specifies the build directory BUILD_DIR.@]@,";
  prf ppf "  -n NAME, --pkg-name NAME  (absent=discovered)@,";
  prf ppf "      @[The name NAME of the package (and hence the opam \
                  install file).@ If absent provided by the package@ \
                  description.@]@,";
  ()

(* Commands *)

let help_cmd pkg =
  let pr = Format.printf in
  let name = Topkg_pkg.name pkg in
  pr "%s's %s - Describes the %s package.@." name exec name;
  pr "%s@." (usage ());
  pr "%a@." pp_help ();
  Ok 0

let version_cmd pkg = print_endline "topkg %%VERSION%%"; Ok 0

let build_cmd pkg kind args =
  let log_conf c =
    Topkg_log.info (fun m -> m "Build configuration:@\n%a" Topkg_conf.dump c)
  in
  let adjust_pkg_to_conf pkg c =
    let name = Topkg_conf.pkg_name c in
    let build_dir = Topkg_conf.build_dir c in
    Topkg_pkg.with_name_and_build_dir pkg ~name ~build_dir
  in
  let pkg_name = Topkg_pkg.name pkg in
  let build_dir = Topkg_pkg.build_dir pkg in
  Topkg_conf.of_cli_args ~pkg_name ~build_dir args
  >>= fun c -> Ok (log_conf c; adjust_pkg_to_conf pkg c)
  >>= fun pkg -> Topkg_pkg.build pkg ~kind c `Host_os

let test_cmd pkg name build_dir list tests args =
  let pkg = Topkg_pkg.with_name_and_build_dir ?name ?build_dir pkg in
  Topkg_pkg.test pkg list tests args

let clean_cmd pkg name build_dir =
  let pkg = Topkg_pkg.with_name_and_build_dir ?name ?build_dir pkg in
  Topkg_pkg.clean pkg `Host_os

let ipc_cmd pkg args =
  Topkg_ipc.write_answer (Topkg_cmd.of_list args) pkg >>= fun () -> Ok 0

let run_cmd pkg cmd args = match cmd with
| `Help -> help_cmd pkg
| `Version -> version_cmd pkg
| `Build kind -> build_cmd pkg kind args
| `Test (pkg_name, bdir, list, tests, args) ->
    test_cmd pkg pkg_name bdir list tests args
| `Clean (pkg_name, bdir) -> clean_cmd pkg pkg_name bdir
| `Ipc -> ipc_cmd pkg args

(* Cli interface *)

let default_verb = Topkg_log.level ()
let incr_verb = function
| Some Topkg_log.Warning -> Some Topkg_log.Info
| Some Topkg_log.Info -> Some Topkg_log.Debug
| v -> v

let is_opt s = Topkg_string.(is_prefix "-" s || is_prefix "--" s)

let parse_cli_help_version_verbosity args =
  let is_help = function "-h" | "--help" | "-help" -> true | _ -> false in
  let is_verb = function "-v" | "--verbose" -> true | _ -> false in
  let is_quiet = function "-q" | "--quiet" -> true | _ -> false in
  let is_version = function "--version" -> true | _ -> false in
  let rec loop cmd verb acc = function
  | a :: args when is_help a -> loop `Help verb acc args
  | a :: args when is_verb a -> loop cmd (incr_verb verb) acc args
  | a :: args when is_quiet a -> loop cmd None acc args
  | a :: args when is_version a ->
      let cmd = if cmd = `Help then `Help else `Version in
      loop cmd verb acc args
  | ("--" :: _ | [] as rest) -> cmd, verb, List.rev (List.rev_append rest acc)
  | a :: args -> loop cmd verb (a :: acc) args
  in
  match args with
  | "help" :: args -> loop `Help default_verb [] args
  | args -> loop `Cmd default_verb [] args

let parse_build_args args =
  let rec loop dry_run raws acc = function
  | ("-r" | "--raw" as opt) :: args ->
      if args = [] then R.error_msgf "option `%s': missing argument" opt else
      loop dry_run (List.hd args :: raws) acc (List.tl args)
  | ("-d" | "--dry-run") :: args ->
      loop true raws acc args
  | a :: args ->
      loop dry_run raws (a :: acc) args
  | [] ->
      let args = List.rev acc in
      match dry_run, raws with
      | false, [] -> Ok (`Build, args)
      | false, raws -> Ok (`Raw (List.rev raws), args)
      | true, [] -> Ok (`Dry_run, args)
      | true, _ ->
          R.error_msg "option `--dry-run' and `--raw' are mutually exclusive"
  in
  loop false [] [] args

let parse_test_args args =
  let rec loop pkg_name build_dir list tests = function
  | ("--" :: args) ->
      Ok (pkg_name, build_dir, list, List.rev tests,
          Some (Topkg_cmd.of_list args))
  | [] -> Ok (pkg_name, build_dir, list, List.rev tests, None)
  | "--build-dir" :: bdir :: args -> loop pkg_name (Some bdir) list tests args
  | ("-l" | "--list") :: args -> loop pkg_name build_dir true tests args
  | ("-n" | "--pkg-name") :: n :: args ->
      loop (Some n) build_dir list tests args
  | a :: args ->
      if is_opt a then R.error_msgf "unknown option `%s'" a else
      loop pkg_name build_dir list (a :: tests) args
  in
  loop None None false [] args

let parse_clean_args args =
  let rec loop pkg_name build_dir = function
  | "--build-dir" :: bdir :: args -> loop pkg_name (Some bdir) args
  | ("-n" | "--pkg-name") :: n :: args -> loop (Some n) build_dir args
  | [] -> Ok (pkg_name, build_dir)
  | a :: args ->
      R.error_msgf "don't know what to do with `%s'" a
  in
  loop None None args

let parse_ipc_args args =
  begin match args with
  | [] -> R.error_msg "missing verbosity and IPC arguments"
  | verbosity :: args ->
      Topkg_log.level_of_string verbosity
      >>= fun verbosity -> match args with
      | [] -> R.error_msg "no IPC arguments specified"
      | args -> Ok (verbosity, args)
  end
  |> R.reword_error_msg ~replace:true (fun e -> R.msgf "ipc: %s" e)

let parse_cli () =
  let args = List.tl (Array.to_list Sys.argv) in
  begin match args with
  | "ipc" :: args -> (* args may be data so don't interpret anything *)
      parse_ipc_args args >>= fun (verb, args) -> Ok (`Ipc, verb, args)
  | args ->
      match parse_cli_help_version_verbosity args with
      | `Help, _, _ as cmd -> Ok cmd
      | `Version, _, _ as cmd -> Ok cmd
      | `Cmd, verbosity, args ->
          match args with
          | "build" :: args ->
              parse_build_args args >>= fun (kind, args) ->
              Ok (`Build kind, verbosity, args)
          | "test" :: args ->
              parse_test_args args
              >>= fun (pkg_name, bdir, list, tests, args) ->
              Ok (`Test (pkg_name, bdir, list, tests, args), verbosity, [])
          | "clean" :: args ->
              parse_clean_args args >>= fun (pkg_name, bdir) ->
              Ok (`Clean (pkg_name, bdir), verbosity, [])
          | cmd :: _ -> R.error_msgf "Unknown command '%s'" cmd
          | [] -> R.error_msg "No command specified"
  end
  |> R.reword_error_msg ~replace:true (fun e -> R.msgf "%s\n%s" e (try_help ()))

(* Main *)

let check_log ret =
  let msg = format_of_string "Package description has %d %s, see log above." in
  let log kind count =
    if count > 0 then match kind with
    | `Errs -> Topkg_log.err (fun m -> m msg count "error(s)")
    | `Warns -> Topkg_log.warn (fun m -> m msg count "warning(s)")
  in
  let errs = Topkg_log.err_count () in
  let warns = Topkg_log.warn_count () in
  log `Errs errs;
  log `Warns warns;
  Ok (if ret + errs > 0 then 1 else 0)

let setup_log_level level =
  Topkg_log.set_level level;
  Topkg_log.info (fun m -> m "topkg %%VERSION%%, running main");
  Ok ()

let main pkg =
  begin
    parse_cli ()
    >>= fun (cmd, log_level, args) -> setup_log_level log_level
    >>= fun () -> run_cmd pkg cmd args
    >>= fun ret -> match cmd with
    | `Build _ -> (check_log ret)
    | _ -> Ok ret
  end
  |> Topkg_log.on_error_msg ~use:(fun () -> 1)

(* Main execution handling.

   The call to [describe] runs the [main] function unless prevented by
   a previous call to [disable].

   We install an [at_exit] function that checks whether either [main]
   ran or [disable] was called and report an error message if that is
   not the case; this is what happens, for example, in case of syntax
   error in the package description. Unfortunately because of
   http://caml.inria.fr/mantis/view.php?id=7178 we cannot return with
   a non-zero exit code at that point. We could by raising an exception
   but it gets confusing, see http://caml.inria.fr/mantis/view.php?id=7253. *)

let must_run_main = ref true
let disable () = must_run_main := false

let pkg = ref None
let describe
    ?delegate ?readmes ?licenses ?change_logs ?metas ?opams ?lint_files
    ?lint_custom ?distrib ?publish ?build name installs
  =
  match !pkg with
  | Some _ -> invalid_arg "Topkg.Pkg.describe already called once"
  | None ->
      let p =
        Topkg_pkg.v ?delegate ?readmes ?licenses ?change_logs ?metas ?opams
          ?lint_files ?lint_custom ?distrib ?publish ?build name installs
      in
      pkg := Some p;
      if !must_run_main then (must_run_main := false; exit (main p)) else ()

let check_something_useful_happened () =
  if !must_run_main then
    Topkg_log.err (fun m -> m "%a" Topkg_string.pp_text
        "No package description found. A syntax error may have \
         occured or did you forget to call Topkg.Pkg.describe ?")

let () = at_exit check_something_useful_happened

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
