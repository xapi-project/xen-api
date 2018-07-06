open Expect_test_common.Std

module List = ListLabels

module Test_outcome = struct
  type t =
    { file_digest             : File.Digest.t
    ; location                : File.Location.t
    ; expectations            : Expectation.Raw.t list
    ; saved_output            : (File.Location.t * string) list
    ; trailing_output         : string
    ; upon_unreleasable_issue : Expect_test_config.Upon_unreleasable_issue.t
    }
end

let tests_run : Test_outcome.t list ref = ref []

let protect ~finally ~f =
  match f () with
  | x           -> finally (); x
  | exception e -> finally (); raise e
;;

module Current_file = struct
  let current = ref None

  let set ~absolute_filename =
    match !current with
    | None -> current := Some absolute_filename
    | Some _ ->
      failwith "Expect_test_collector.set: already set"
  ;;

  let unset () =
    match !current with
    | Some _ -> current := None
    | None ->
      failwith "Expect_test_collector.unset: not set"
  ;;

  let get () =
    match !current with
    | Some fn -> fn
    | None ->
      failwith "Expect_test_collector.get: not set"
  ;;
end

module Make(C : Expect_test_config.S) = struct
  let ( >>= ) t f = C.IO.bind t ~f
  let return = C.IO.return

  module C = struct
    include C
    let flush () =
      (* Always flush [Pervasives.stdout] and [Pervasives.stderr]. *)
      Pervasives.flush Pervasives.stdout;
      Pervasives.flush Pervasives.stderr;
      C.flush ()
  end

  module Instance : sig
    type t

    val save_output : t -> File.Location.t -> unit C.IO.t

    val exec :
      file_digest    : File.Digest.t ->
      location       : File.Location.t ->
      expectations   : Expectation.Raw.t list ->
      f              : (t -> unit C.IO.t) ->
      unit
  end = struct
    module Running : sig
      type t
      val create : unit -> t
      val get_outputs_and_cleanup : t -> (File.Location.t * string) list  * string
      val save_output : t -> File.Location.t -> unit
    end = struct
      type t =
        { mutable saved : (File.Location.t * int) list
        ; chan          : out_channel
        ; filename      : File.Name.t
        }

      external before_test
        : output:out_channel -> stdout:out_channel -> stderr:out_channel -> unit
        = "expect_test_collector_before_test"
      external after_test
        : stdout:out_channel -> stderr:out_channel -> unit
        = "expect_test_collector_after_test"
      external pos_out : out_channel -> int = "caml_out_channel_pos_fd"

      let get_position () = pos_out stdout
      ;;

      let create () =
        let filename = Filename.temp_file "expect-test" "output" in
        let chan = open_out filename in
        before_test ~output:chan ~stdout ~stderr;
        { chan
        ; filename = File.Name.of_string filename
        ; saved    = []
        }
      ;;

      let extract_output ic len =
        let s = really_input_string ic len in
        if not (Check_backtraces.contains_backtraces s) then
          s
        else
          let cr_prefix =
            Expect_test_config.Upon_unreleasable_issue.comment_prefix
              C.upon_unreleasable_issue
          in
          Printf.sprintf "\n\
                          (* %sexpect_test_collector: This test expectation appears to \
                          contain a backtrace.\n\
                         \   This is strongly discouraged as backtraces are fragile.\n\
                         \   Please change this test to not include a backtrace. *)\n\
                          \n\
                          %s" cr_prefix s

      let get_outputs_and_cleanup t =
        let last_ofs = get_position () in
        after_test ~stdout ~stderr;
        close_out t.chan;

        let fname = File.Name.relative_to ~dir:(File.initial_dir ()) t.filename in
        protect ~finally:(fun () -> Sys.remove fname) ~f:(fun () ->
          let ic = open_in fname in
          protect ~finally:(fun () -> close_in ic) ~f:(fun () ->
            let ofs, outputs =
              List.fold_left (List.rev t.saved) ~init:(0, [])
                ~f:(fun (ofs, acc) (loc, next_ofs) ->
                  let s = extract_output ic (next_ofs - ofs) in
                  (next_ofs, ((loc, s) :: acc)))
            in
            let trailing_output = extract_output ic (last_ofs - ofs) in
            (List.rev outputs, trailing_output)))
      ;;

      let save_output running location =
        let pos = get_position () in
        running.saved <- (location, pos) :: running.saved

    end

    type state = Running of Running.t | Ended
    type t = { mutable state : state }

    let current_test : (File.Location.t * t) option ref = ref None

    let () =
      Caml.at_exit (fun () ->
        match !current_test with
        | None | Some (_, { state = Ended }) -> ()
        | Some (loc, { state = Running running }) ->
          let blocks, trailing = Running.get_outputs_and_cleanup running in
          Printf.eprintf "File %S, line %d, characters %d-%d:\n\
                          Error: program exited while expect test was running!\n\
                          Output captured so far:\n%!"
            (File.Name.to_string loc.filename)
            loc.line_number
            (loc.start_pos - loc.line_start)
            (loc.end_pos   - loc.line_start);
          List.iter blocks ~f:(fun (_, s) -> Printf.eprintf "%s%!" s);
          Printf.eprintf "%s%!" trailing)

    let rec final_flush ?(count=0) k =
      let max_attempts = 10 in
      C.flush () >>= fun () ->
      if C.flushed () then
        k ~append:""
      else if count = max_attempts then
        k ~append:(Printf.sprintf
                     "\nSTOPPED COLLECTING OUTPUT AFTER %d FLUSHING ATTEMPS\n\
                      THERE MUST BE A BACKGROUND JOB PRINTING TO STDOUT\n"
                     max_attempts)
      else
        final_flush ~count:(count + 1) k

    let exec ~file_digest ~location ~expectations ~f =
      let running = Running.create () in
      let t = { state = Running running } in
      current_test := Some (location, t);
      let finally () =
        current_test := None;
        C.run (fun () ->
          final_flush (fun ~append ->
            t.state <- Ended;
            let saved_output, trailing_output = Running.get_outputs_and_cleanup running in
            tests_run :=
              { file_digest
              ; location
              ; expectations
              ; saved_output
              ; trailing_output = trailing_output ^ append
              ; upon_unreleasable_issue = C.upon_unreleasable_issue
              } :: !tests_run;
            return ()))
      in
      protect ~finally ~f:(fun () ->
        C.run (fun () ->
          f t))
    ;;

    let save_output t location =
      match t.state with
      | Running running ->
        C.flush () >>= fun () ->
        Running.save_output running location;
        return ()
      | Ended ->
        Printf.ksprintf failwith
          !"Expect_test_collector.Instance.save_output called after test has ended \
            (loc = %{sexp:File.Location.t})"
          location
    ;;
  end

  let run
        ~file_digest
        ~(location:File.Location.t)
        ~absolute_filename:defined_in
        ~description
        ~tags
        ~expectations
        ~inline_test_config
        f
    =
    Ppx_inline_test_lib.Runtime.test
      ~config:inline_test_config
      ~descr:(match description with None -> "" | Some s -> ": " ^ s)
      ~tags
      ~filename:(File.Name.to_string location.filename)
      ~line_number:location.line_number
      ~start_pos:(location.start_pos - location.line_start)
      ~end_pos:(location.end_pos   - location.line_start)
      (fun () ->
         let registering_tests_for = Current_file.get () in
         if defined_in <> registering_tests_for then
           Printf.ksprintf failwith
             "Trying to run an expect test from the wrong file.\n\
              - test declared at %s:%d\n\
              - trying to run it from %s\n"
             defined_in location.line_number registering_tests_for
         else begin
           (* To avoid capturing not-yet flushed data of the stdout buffer *)
           C.run C.flush;
           Instance.exec ~file_digest ~location ~expectations ~f;
           true
         end
      );
  ;;
end

let tests_run () =
  (* We prepend tests when we encounter them, so reverse the list to reinstate order *)
  List.rev !tests_run
