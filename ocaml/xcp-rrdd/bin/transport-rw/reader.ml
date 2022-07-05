(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Cmdliner

let help_secs =
  [
    `S "MORE HELP"
  ; `P "Use `$(mname) $(i,command) --help' for help on a single command."
  ; `Noblank
  ]

let read_file_cmd =
  let path =
    let doc = "The path of the file to read" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
  in
  let protocol =
    let doc = "The protocol to use to read the rrd data" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"PROTOCOL" ~doc)
  in
  let once =
    let doc = "If set, only read data once rather than in a loop." in
    Arg.(value & flag & info ["once"] ~docv:"ONCE" ~doc)
  in
  let doc = "read from a file" in
  let man =
    [
      `S "DESCRIPTION"
    ; `P "Read rrd data from a file, using the specified protocol"
    ]
    @ help_secs
  in
  ( Term.(const Reader_commands.read_file $ once $ path $ protocol)
  , Cmd.info "file" ~doc ~man
  )

let read_page_cmd =
  let domid =
    let doc = "The remote domid which is writing to shared memory" in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"DOMID" ~doc)
  in
  let grantref =
    let doc =
      "The grant reference of the shared page which will be read from"
    in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"GRANTREF" ~doc)
  in
  let protocol =
    let doc = "The protocol to use to read the rrd data" in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"PROTOCOL" ~doc)
  in
  let doc = "read from a page of shared memory" in
  let man =
    [
      `S "DESCRIPTION"
    ; `P
        "Read rrd data from a page of shared memory, using the specified \
         protocol"
    ]
    @ help_secs
  in
  ( Term.(const Reader_commands.read_page $ domid $ grantref $ protocol)
  , Cmd.info "page" ~doc ~man
  )

let cmds = List.map (fun (t, i) -> Cmd.v i t) [read_file_cmd; read_page_cmd]

let () =
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let doc = "RRD protocol reader" in
  let info = Cmd.info "reader" ~version:"0.1" ~doc ~man:help_secs in
  let cmd = Cmd.group ~default info cmds in
  exit (Cmd.eval cmd)
