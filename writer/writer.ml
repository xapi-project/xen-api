open Cmdliner

let help_secs = [
	`S "MORE HELP";
	`P "Use `$(mname) $(i,command) --help' for help on a single command.";
	`Noblank;
]

let default_cmd =
	let doc = "RRD protocol writer" in
	let man = help_secs in
	Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ pure ())),
	Term.info "writer" ~version:"0.1" ~doc ~man

let write_file_cmd =
	let path =
		let doc = "The path of the file to write" in
		Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
	in
	let protocol =
		let doc = "The protocol to use to write the rrd data" in
		Arg.(required & pos 1 (some string) None & info [] ~docv:"PROTOCOL" ~doc)
	in
	let doc = "write to a file" in
	let man = [
		`S "DESCRIPTION";
		`P "Write rrd data to a file, using the specified protocol"
	] @ help_secs in
	Term.(pure Writer_commands.write_file $ path $ protocol),
	Term.info "file" ~doc ~man

let cmds = [
	write_file_cmd
]

let () =
	match Term.eval_choice default_cmd cmds with
	| `Error _ -> exit 1
	| _ -> exit 0
