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

let list common_opts prefix =
  let c = IO.connect common_opts.Common.port in
  let _ = Connection.rpc c (In.Login (Protocol_unix.whoami ())) in
  match Connection.rpc c (In.List prefix) with
  | Error e -> `Error(true, Printexc.to_string e)
  | Ok raw ->
    let all = Out.string_list_of_rpc (Jsonrpc.of_string raw) in
    List.iter print_endline all;
    `Ok ()


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

let default_cmd = 
  let doc = "interact with an XCP message switch" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "m-cli" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [list_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
