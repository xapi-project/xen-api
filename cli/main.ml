open Protocol
open Protocol_unix

let project_url = "http://github.com/djs55/message_switch"

open Cmdliner

module Common = struct
	type t = {
        verbose: bool;
        debug: bool;
        port: int;
	} with rpc

	let make verbose debug port =
		{ verbose; debug; port }

	let to_string x = Jsonrpc.to_string (rpc_of_t x)
end

let _common_options = "COMMON OPTIONS"

(* Options common to all commands *)
let common_options_t = 
  let docs = _common_options in 
  let debug = 
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in 
    Arg.(last & vflag_all [false] [verbose]) in 
  let port = 
    let doc = Printf.sprintf "Specify port to connect to the message switch." in
    Arg.(value & opt int 8080 & info ["port"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ port)


(* Help sections common to all commands *)
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Commands *)

let diagnostics common_opts =
  let c = IO.connect common_opts.Common.port in
  let _ = Connection.rpc c (In.Login (Protocol_unix.whoami ())) in
  match Connection.rpc c In.Diagnostics with
  | Error e -> `Error(true, Printexc.to_string e)
  | Ok raw ->
    print_endline raw;
    `Ok ()

let list common_opts prefix =
  let c = IO.connect common_opts.Common.port in
  let _ = Connection.rpc c (In.Login (Protocol_unix.whoami ())) in
  match Connection.rpc c (In.List prefix) with
  | Error e -> `Error(true, Printexc.to_string e)
  | Ok raw ->
    let all = Out.string_list_of_rpc (Jsonrpc.of_string raw) in
    List.iter print_endline all;
    `Ok ()

let dump common_opts =
  let c = IO.connect common_opts.Common.port in
  let from = ref 0L in
  let timeout = 5. in
  let start = ref None in
  while true do
    match Connection.rpc c (In.Trace (!from, timeout)) with
      | Error e -> raise e
      | Ok raw ->
        let trace = Out.trace_of_rpc (Jsonrpc.of_string raw) in
        let endpoint = function
          | None -> "-"
          | Some x -> x in
        let message = function
          | Event.Message (id, m) -> m.Message.payload
          | Event.Ack id -> Printf.sprintf "ack %Ld" id in
        List.iter (fun (id, event) ->
          let time = match !start with
            | None ->
              start := Some event.Event.time;
              0.
            | Some t ->
              event.Event.time -. t in
          Printf.fprintf stdout "%Ld: %.1f: %10s -> %10s -> %10s: %s\n%!" id time
            (endpoint event.Event.input) event.Event.queue
            (endpoint event.Event.output) (message event.Event.message)
        ) trace.Out.events;
        from :=
          begin match trace.Out.events with
            | [] -> !from
            | (id, _) :: ms -> Int64.add 1L (List.fold_left max id (List.map fst ms))
          end;
    done;
    `Ok ()

let diagnostics_cmd =
  let doc = "dump the current switch state" in
  let man = [
    `S "DESCRIPTION";
    `P "Dumps the current switch state for diagnostic purposes.";
  ] @ help in
  Term.(ret(pure diagnostics $ common_options_t)),
  Term.info "diagnostics" ~sdocs:_common_options ~doc ~man

let list_cmd =
  let doc = "list the currently-known queues" in
  let man = [
    `S "DESCRIPTION";
    `P "Print a list of all queues registered with the message switch";
  ] @ help in
  let prefix =
    let doc = Printf.sprintf "List queues with a specific prefix." in
    Arg.(value & opt string "" & info ["prefix"] ~docv:"PREFIX" ~doc) in
  Term.(ret(pure list $ common_options_t $ prefix)),
  Term.info "list" ~sdocs:_common_options ~doc ~man

let dump_cmd =
  let doc = "display a live stream of trace events" in
  let man = [
    `S "DESCRIPTION";
    `P "Display a live stream of trace events captured within the message switch";
  ] @ help in
  Term.(ret(pure dump $ common_options_t)),
  Term.info "dump" ~sdocs:_common_options ~doc ~man

let string_of_ic ?end_marker ic =
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      (match end_marker with None -> () | Some x -> if x = line then raise End_of_file);
      lines := line :: !lines
    done;
    ""
  with End_of_file -> String.concat "\n" (List.rev !lines)

let call common_options_t name body path timeout =
  match name with
  | None -> `Error(true, "a queue name is required")
  | Some name ->
    begin
    let txt = match body, path with
    | None, None ->
      Printf.printf "Enter body text:\n%!";
      string_of_ic stdin
    | Some _, Some _ ->
      failwith "please supply either a body or a file, not both"
    | Some x, _ -> x
    | None, Some x ->
      let ic = open_in x in
      let txt = string_of_ic ic in
      close_in ic;
      txt in

    let c = Client.connect common_options_t.Common.port name in
    let result = Client.rpc c ?timeout txt in
    print_endline result;
    `Ok ()
    end

let call_cmd =
  let doc = "perform a remote procedure call" in
  let man = [
    `S "DESCRIPTION";
    `P "Perform a remote procedure call against a named service.";
  ] @ help in
  let qname =
    let doc = "Name of remote service to invoke." in
    Arg.(value & pos 0 (some string) None & info [] ~doc) in
  let body =
    let doc = "Request text to send to the remote service." in
    Arg.(value & opt (some string) None & info ["body"] ~docv:"BODY" ~doc) in
  let path =
    let doc = "File containing request text to send to the remote service." in
    Arg.(value & opt (some file) None & info ["file"] ~docv:"FILE" ~doc) in
  let timeout =
    let doc = "Time to wait for a response before failing." in
    Arg.(value & opt (some int) None & info ["timeout"] ~docv:"TIMEOUT" ~doc) in

  Term.(ret(pure call $ common_options_t $ qname $ body $ path $ timeout)), 
  Term.info "call" ~sdocs:_common_options ~doc ~man

let serve common_options_t name program =
  match name with
  | None ->
    `Error(true, "a queue name is required")
  | Some name ->
    let c = Protocol_unix.IO.connect common_options_t.Common.port in
    Protocol_unix.Server.listen (fun req ->
      match program with
      | None ->
        print_endline "Received:";
        print_endline req;
        print_endline "Enter body text: (end with a \".\")";
        string_of_ic ~end_marker:"." stdin
      | Some program ->
        let stdout, stdin, stderr = Unix.open_process_full program [| program |] in
        output_string stdin req; close_out stdin;
        let res = string_of_ic stdout in
        let (_: Unix.process_status) = Unix.close_process_full (stdout, stdin, stderr) in
        res
    ) c name;
    `Ok ()

let serve_cmd =
  let doc = "respond to remote procedure calls" in
  let man = [
    `S "DESCRIPTION";
    `P "Listen for remote procedure calls and run the specified program with the body, returning the program's output as the response.";
  ] @ help in
  let qname =
    let doc = "Name of service to implement." in
    Arg.(value & pos 0 (some string) None & info [] ~doc) in
  let program =
    let doc = "Path of the program to invoke on every call." in
    Arg.(value & opt (some file) None & info ["program"] ~doc) in

  Term.(ret(pure serve $ common_options_t $ qname $ program)),
  Term.info "serve" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "interact with an XCP message switch" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "m-cli" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [list_cmd; dump_cmd; call_cmd; serve_cmd; diagnostics_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
