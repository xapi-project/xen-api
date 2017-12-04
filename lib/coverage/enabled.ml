(** This module sets up the env variable for bisect_ppx which describes
 *  where log files are written.
 *)

module D=Debug.Make(struct let name="coverage" end)
let prefix = "org.xen.xapi.coverage"

module Bisect = struct
  let bisect_file = "BISECT_FILE"
  let dump jobid =
    let bisect_prefix = Unix.getenv bisect_file in
    (* dump coverage information in same location as it would normally
       get dumped on exit, except also embed the jobid to make it easier to group.
       Relies on [open_temp_file] generating a unique filename given a prefix/suffix to
       avoid clashes with filenames created at exit by bisect itself. *)
    let tmp, ch = Filename.open_temp_file
                    ~temp_dir:(Filename.dirname bisect_prefix)
                    (Filename.basename bisect_prefix)
                    (Printf.sprintf ".%Ld.out" jobid) in
    try
      Bisect.Runtime.dump_counters_exn ch;
      D.debug "Saved coverage data to %s" tmp;
      close_out_noerr ch;
      (* Keep file - will be collected by XenRT *)
      tmp
    with e ->
      Sys.remove tmp;
      D.warn "Failed to save coverage: %s" (Printexc.to_string e);
      raise e

  let reset () =
    Bisect.Runtime.reset_counters ();
    D.debug "Coverage counters reset"

  let init_env name =
    let (//)    = Filename.concat in
    let tmpdir  = Filename.get_temp_dir_name () in
    try
      ignore (Sys.getenv bisect_file)
    with Not_found ->
      Unix.putenv bisect_file (tmpdir // Printf.sprintf "bisect-%s-" name)

  let process body =
    match Stringext.split ~on:' ' body with
    | ["reset"] -> reset (); ""
    | ["dump"; jobid] -> jobid |> Int64.of_string |> dump
    | _ -> failwith body

  let init name =
    init_env name;
    let queue_name = prefix ^ "." ^ name in
    let (_:Thread.t) =
      Thread.create (Protocol_unix.Server.listen ~process
                       ~switch:!Xcp_client.switch_path ~queue:queue_name) () in
    D.debug "Started coverage API thread on %s" queue_name;
    ()
end

module Dispatcher = struct
  let self = prefix ^ ".dispatch"
  open Protocol_unix

  let rpc_ignore_err ~t ~body queue =
    D.debug "Dispatching %s to %s" body queue;
    match Client.(rpc ~t ~queue ~body () |> error_to_msg) with
    | `Ok x -> x
    | `Error (`Msg e) ->
       D.info "Failed to get coverage data from %s: %s" queue e;
       ""

  let string_of_result = function
    | `Ok s -> s
    | `Error (`Msg e) ->
       D.info "Failed to get coverage data: %s" e;
       "ERROR"

  let process = fun body ->
    let open Message_switch.Mresult in
    D.debug "Coverage dispatcher received %s" body;
    let result = begin
        Client.connect ~switch:!Xcp_client.switch_path () >>= fun t ->
        Client.list ~t ~prefix ~filter:`Alive () >>= fun queues ->
        queues |>
          (* filter out ourselves *)
          List.filter (fun q -> self <> q) |>

          (* best-effort: collect and return all non-failed results, log errors *)
          List.rev_map (rpc_ignore_err ~t ~body) |>

          (* multiple return values converted to a single string, suitable for use in a command like:
          mv $(message-cli call org.xen.xapi.coverage.dispatch --timeout 60 --body 'dump {jobid}') /tmp/coverage/
           *)
          String.concat " " |>
          ok
      end |> Client.error_to_msg |> string_of_result in
    D.debug "Coverage dispatcher replying to '%s': %s" body result;
    result

  let init () =
    (* receives command and dispatches to all other coverage message queues *)
    let (_:Thread.t) =
      Thread.create (Protocol_unix.Server.listen ~process
                       ~switch:!Xcp_client.switch_path ~queue:self) () in
    D.debug "Started coverage API dispatcher on %s" self;
    ()
end

(** [init name] sets up coverage profiling for binary [name]. You could 
 *  use [Sys.argv.(0)] for [name].
 *)
let init name =
  D.info "About to initialize coverage runtime";
  Bisect.init name;
  D.info "Coverage runtime initialized"

let dispatcher_init name =
  Dispatcher.init ()
