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

let project_url = "http://github.com/djs55/xenops-cli"

open Common
open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

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
  let socket = 
    let doc = Printf.sprintf "Specify path to the server Unix domain socket." in
    Arg.(value & opt file !Xenops_interface.default_path & info ["socket"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ socket)
    
(* Commands *)

let list_cmd =
  let doc = "list the VMs registered with xenopsd" in
  let man = [
    `S "DESCRIPTION";
    `P "Lists the VMs registered with the xenopsd service.

       VMs are registered with xenospd via the \"add\" command and
       will be monitored until the corresponding \"remove\" command.

       xenopsd will not touch any VMs (and domains) which have not
       been explicitly registered." ] @ help in
  Term.(pure Xn.list $ common_options_t),
  Term.info "list" ~sdocs:_common_options ~doc ~man

let start_cmd =
  let vm = 
    let doc = "The name or UUID of the VM to be started." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"VM" ~doc) in
  let paused =
    let doc = "Leave the VM in a Paused state." in
    Arg.(value & flag & info [ "paused" ] ~doc) in
  let doc = "start a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Start a VM.

       If no additional arguments are provided then this command
       will return when the VM is in the \"Running\" state.
       If the --paused argument is specified then the VM will
       be left in the \"Paused\" state.";
    `S "ERRORS";
    `P "Something about memory.";
    `P "Something about disks.";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.start $ common_options_t $ paused $ vm)),
  Term.info "start" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "interact with the XCP xenopsd VM management service" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "xenops-cli" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [list_cmd; start_cmd ]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
