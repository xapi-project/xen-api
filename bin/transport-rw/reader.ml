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

let help_secs = [
  `S "MORE HELP";
  `P "Use `$(mname) $(i,command) --help' for help on a single command.";
  `Noblank;
]

let default_cmd =
  let doc = "RRD protocol reader" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ pure ())),
  Term.info "reader" ~version:"0.1" ~doc ~man

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
  let man = [
    `S "DESCRIPTION";
    `P "Read rrd data from a file, using the specified protocol"
  ] @ help_secs in
  Term.(pure Reader_commands.read_file $ once $ path $ protocol),
  Term.info "file" ~doc ~man

let read_page_cmd =
  let domid =
    let doc = "The remote domid which is writing to shared memory" in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"DOMID" ~doc)
  in
  let grantref =
    let doc = "The grant reference of the shared page which will be read from" in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"GRANTREF" ~doc)
  in
  let protocol =
    let doc = "The protocol to use to read the rrd data" in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"PROTOCOL" ~doc)
  in
  let doc = "read from a page of shared memory" in
  let man = [
    `S "DESCRIPTION";
    `P "Read rrd data from a page of shared memory, using the specified protocol"
  ] @ help_secs in
  Term.(pure Reader_commands.read_page $ domid $ grantref $ protocol),
  Term.info "page" ~doc ~man

let cmds = [
  read_file_cmd;
  read_page_cmd;
]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
