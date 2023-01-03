open Goblint_lib
open GobConfig
open Goblintutil
open Maingoblint
open Prelude
open Printf

(** the main function *)
let main () =
  try
    Cilfacade.init ();
    Maingoblint.parse_arguments ();

    (* Timing. *)
    Maingoblint.reset_stats ();
    if get_bool "dbg.timing.enabled" then (
      let tef_filename = get_string "dbg.timing.tef" in
      if tef_filename <> "" then
        Goblint_timing.setup_tef tef_filename;
      Timing.Default.start {
        cputime = true;
        walltime = true;
        allocated = true;
        count = true;
        tef = true;
      };
      Timing.Program.start {
        cputime = false;
        walltime = false;
        allocated = false;
        count = false;
        tef = true;
      }
    );

    handle_extraspecials ();
    GoblintDir.init ();

    if get_bool "dbg.verbose" then (
      print_endline (localtime ());
      print_endline Goblintutil.command_line;
    );
    let file = lazy (Fun.protect ~finally:GoblintDir.finalize preprocess_parse_merge) in
    if get_bool "server.enabled" then (
      let file =
        if get_bool "server.reparse" then
          None
        else
          Some (Lazy.force file)
      in
      Server.start file
    )
    else (
      let file = Lazy.force file in
      let changeInfo =
        if GobConfig.get_bool "incremental.load" || GobConfig.get_bool "incremental.save" then
          diff_and_rename file
        else
          Analyses.empty_increment_data ()
      in
      if get_bool "ana.autotune.enabled" then AutoTune.chooseConfig file;
      file |> do_analyze changeInfo;
      do_html_output ();
      do_gobview ();
      do_stats ();
      Goblint_timing.teardown_tef ();
      if !verified = Some false then exit 3 (* verifier failed! *)
    )
  with
  | Exit ->
    do_stats ();
    Goblint_timing.teardown_tef ();
    exit 1
  | Sys.Break -> (* raised on Ctrl-C if `Sys.catch_break true` *)
    do_stats ();
    (* Printexc.print_backtrace BatInnerIO.stderr *)
    eprintf "%s\n" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted by SIGINT (Ctrl-C)!"));
    Goblint_timing.teardown_tef ();
    exit 131 (* same exit code as without `Sys.catch_break true`, otherwise 0 *)
  | Timeout ->
    do_stats ();
    eprintf "%s\n" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted because it reached the set timeout of " ^ get_string "dbg.timeout" ^ " or was signalled SIGPROF!"));
    Goblint_timing.teardown_tef ();
    exit 124

(* We do this since the evaluation order of top-level bindings is not defined, but we want `main` to run after all the other side-effects (e.g. registering analyses/solvers) have happened. *)
let () = at_exit main
