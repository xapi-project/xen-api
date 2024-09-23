module C = Cmdliner

let ( // ) = Filename.concat

let man =
  [
    `P "$(mname) is a helper util for multi-version XenServer drivers"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; `S C.Manpage.s_bugs
  ; `P "Check bug reports at https://github.com/xapi-project/xen-api"
  ]

let directory =
  C.Arg.(
    value
    & pos 0 dir "/"
    & info [] ~docv:"DIRECTORY"
        ~doc:"Directory for XenServer multi-driver hierarchy"
  )

let list =
  let doc = "list multi-version drivers ELF notes" in
  let man =
    [
      `S C.Manpage.s_description
    ; `P "List multi-version drivers' ELF\nnotes in JSON format"
    ; `Blocks man
    ]
  in
  let cmd directory =
    Sys.readdir directory
    |> Array.iter (fun driver_name ->
           Xapi_host_driver_helpers.dump_notes ("/sys/module" // driver_name)
       ) ;
    `Ok ()
  in
  let info = C.Cmd.info "list" ~doc ~man in
  C.(Cmd.v info Term.(ret (const cmd $ directory)))

let cmds = [list]

let main_cmd =
  let help = `Help (`Pager, None) in
  let doc = "Multi-version driver utils" in
  let info = C.Cmd.info "mvd_utils" ~doc ~man in
  let default = C.Term.(ret @@ const help) in
  C.Cmd.group info ~default cmds

let _ = C.Cmd.eval main_cmd |> exit
