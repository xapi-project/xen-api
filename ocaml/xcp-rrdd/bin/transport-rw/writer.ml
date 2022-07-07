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
  let man =
    [
      `S "DESCRIPTION"
    ; `P "Write rrd data to a file, using the specified protocol"
    ]
    @ help_secs
  in
  Cmd.v
    (Cmd.info "file" ~doc ~man)
    Term.(const Writer_commands.write_file $ path $ protocol)

let write_page_cmd =
  let domid =
    let doc = "The remote domid which will read the shared memory" in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"DOMID" ~doc)
  in
  let protocol =
    let doc = "The protocol to use to write the rrd data" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"PROTOCOL" ~doc)
  in
  let doc = "write to a page" in
  let man =
    [
      `S "DESCRIPTION"
    ; `P "Write rrd data to page of memory shared with another domain"
    ]
    @ help_secs
  in
  Cmd.v
    (Cmd.info "page" ~doc ~man)
    Term.(const Writer_commands.write_page $ domid $ protocol)

let cmds = [write_file_cmd; write_page_cmd]

let () =
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let doc = "RRD protocol writer" in
  let info = Cmd.info "writer" ~version:"0.1" ~doc ~man:help_secs in
  let cmd = Cmd.group ~default info cmds in
  exit (Cmd.eval cmd)
