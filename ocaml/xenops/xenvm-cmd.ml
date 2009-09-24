(* connect to the mini monitor for a specific vm and send command *)

open Printf

let _ =
	if Array.length Sys.argv < 3 then (
		eprintf "usage: %s <uuid> <cmd> [cmd args]\n" Sys.argv.(0);
		exit 2
	);
	let uuid = Sys.argv.(1) in

	let cmds = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in
	let s = (String.concat " " (Array.to_list cmds)) in
	match Xenvmlib.request uuid s with
	| Xenvmlib.Ok          -> ()
	| Xenvmlib.Error error -> eprintf "error: %s\n" error; exit 1
	| Xenvmlib.Msg msg     -> printf "%s\n" msg
	| Xenvmlib.Unknown s   -> eprintf "warning: unknown answer: \"%s\"\n" s

