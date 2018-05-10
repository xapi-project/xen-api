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
  let queue =
    let default = Some "org.xen.xapi.xenops.classic" in
    let doc = Printf.sprintf "Specify queue name in message switch." in
    Arg.(value & opt (some string) default & info ["queue"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ socket $ queue)
    
(* Commands *)

let events_cmd =
  let doc = "display a live stream of events" in
  let man = [
    `S "DESCRIPTION";
    `P "Displays a live stream of events received from xenopsd. These events include VM powercycle events and VM configuration changes."
  ] @ help in
  Term.(ret(pure Xn.events $ common_options_t)),
  Term.info "events" ~sdocs:_common_options ~doc ~man

let create_cmd =
  let doc = "register a VM and start it immediately" in
  let man = [
    `S "DESCRIPTION";
    `P "Registers a new VM with the xenopsd service and starts it immediately.";
  ] @ help in
  let filename =
    let doc = Printf.sprintf "Path to the VM metadata." in
    Arg.(value & pos 0 (some file) None & info [] ~doc) in
  let console =
    let doc = "Connect to the VM's console." in
    Arg.(value & flag & info [ "console" ] ~doc) in
  Term.(ret(pure Xn.create $ common_options_t $ filename $ console)),
  Term.info "create" ~sdocs:_common_options ~doc ~man

let add_cmd =
  let doc = "register a new VM with xenopsd" in
  let man = [
    `S "DESCRIPTION";
    `P "Registers a new VM with the xenopsd service.";
  ] @ help in
  let filename = 
    let doc = Printf.sprintf "Path to the VM metadata to be registered." in
    Arg.(value & pos 0 (some file) None & info [] ~doc) in
  Term.(ret(pure Xn.add $ common_options_t $ filename)),
  Term.info "add" ~sdocs:_common_options ~doc ~man

let list_cmd =
  let doc = "list the VMs registered with xenopsd" in
  let man = [
    `S "DESCRIPTION";
    `P "Lists the VMs registered with the xenopsd service.";
    `P "VMs are registered with xenospd via the \"add\" command and
       will be monitored until the corresponding \"remove\" command.";
    `P "xenopsd will not touch any VMs (and domains) which have not
       been explicitly registered." ] @ help in
  Term.(pure Xn.list $ common_options_t),
  Term.info "list" ~sdocs:_common_options ~doc ~man

let vm_arg verb =
  let doc = Printf.sprintf "The name or UUID of the VM to be %s." verb in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"VM" ~doc)

let remove_cmd =
  let vm = vm_arg "unregistered" in
  let doc = "unregister a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Unregister a VM.";
    `P "The xenopsd service will only manipulate VMs if they are
       explicitly registered with it. You should unregister a VM if either:";
    `P "1. the VM is not needed any more; or";
    `P "2. you intend to manage the VM on another host or using another toolstack.";
    `P "Note: before attempting to use multiple toolstacks simultaneously on a single host,
        check all the relevant documentation to see whether this is a sensible thing to do.";
    `P "Only Halted VMs may be unregistered.";
    `S "ERRORS";
    `P "Something about power state exceptions";
   ] in
  Term.(ret (pure Xn.remove $ common_options_t $ vm)),
  Term.info "remove" ~sdocs:_common_options ~doc ~man

let start_cmd =
  let vm = vm_arg "started" in
  let paused =
    let doc = "Leave the VM in a Paused state." in
    Arg.(value & flag & info [ "paused" ] ~doc) in
  let console =
    let doc = "Connect to the VM's console." in
    Arg.(value & flag & info [ "console" ] ~doc) in
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
  Term.(ret (pure Xn.start $ common_options_t $ paused $ console $ vm)),
  Term.info "start" ~sdocs:_common_options ~doc ~man

let console_cmd =
  let vm =
    let doc = "The name or UUID of the VM whose console to be accessed." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"VM" ~doc) in
  let doc = "attach to the console of a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Attach to the console of a VM.";
    `S "ERRORS";
    `P "Something about memory.";
    `P "Something about disks.";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.console_connect $ common_options_t $ vm)),
  Term.info "console" ~sdocs:_common_options ~doc ~man

let shutdown_cmd =
  let vm = vm_arg "shutdown and powered off" in
  let timeout =
    let doc = "Amount of time to wait for the VM to cleanly shut itself down, before we power it off." in
    Arg.(value & opt (some float) None & info [ "timeout" ] ~doc) in
  let doc = "shutdown a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Shutdown a VM.";
    `P "If the specified VM is running, it will be asked to shutdown.
       If a <timeout> is specified then we will wait. If no <timeout>
       is specified or if the timeout expires, the VM will be powered off.";
    `S "ERRORS";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.shutdown $ common_options_t $ timeout $ vm)),
  Term.info "shutdown" ~sdocs:_common_options ~doc ~man

let reboot_cmd =
  let vm = vm_arg "rebooted" in
  let timeout =
    let doc = "Amount of time to wait for the VM to cleanly shut itself down, before we power it off and then on." in
    Arg.(value & opt (some float) None & info [ "timeout" ] ~doc) in
  let doc = "reboot a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Reboot a VM.";
    `P "If the specified VM is running, it will be asked to reboot.
       If a <timeout> is specified then we will wait. If no <timeout>
       is specified or if the timeout expires, the VM will be powered off.
       We will then power the VM back on again.";
    `S "ERRORS";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.reboot $ common_options_t $ timeout $ vm)),
  Term.info "reboot" ~sdocs:_common_options ~doc ~man

let suspend_cmd =
  let vm = vm_arg "suspended" in
  let device =
    let doc = "Block device to write the suspend image to" in
    Arg.(value & opt (some file) None & info [ "block-device" ] ~doc) in
  let doc = "suspend a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Suspend a VM.";
    `P "If the specified VM is running, it will be asked to suspend.
       The memory image will be saved to the specified block device.";
    `S "ERRORS";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.suspend $ common_options_t $ device $ vm)),
  Term.info "suspend" ~sdocs:_common_options ~doc ~man

let resume_cmd =
  let vm = vm_arg "resumed" in
  let device =
    let doc = "Block device to read the suspend image from" in
    Arg.(value & opt (some file) None & info [ "block-device" ] ~doc) in
  let doc = "resume a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Resume a VM.";
    `P "The VM memory image will be reloaded from the specified block device
       and the VM will be left in a Running state.";
    `S "ERRORS";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.resume $ common_options_t $ device $ vm)),
  Term.info "resume" ~sdocs:_common_options ~doc ~man

let pause_cmd =
  let vm = vm_arg "paused" in
  let doc = "pause a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Pause a VM.";
    `P "The running VM will be marked as Paused: although it will
       still consume memory on the host, all of its virtual CPUs will
       be taken offline so the VM will stop executing.";
    `S "ERRORS";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.pause $ common_options_t $ vm)),
  Term.info "pause" ~sdocs:_common_options ~doc ~man

let unpause_cmd =
  let vm = vm_arg "unpaused" in
  let doc = "unpause a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Unpause a VM.";
    `P "A paused VM will be marked as Running: all of its virtual CPUs
       will be brought back online so the VM will start executing.";
    `S "ERRORS";
    `P "Something about the current power state." ] @ help in
  Term.(ret (pure Xn.unpause $ common_options_t $ vm)),
  Term.info "unpause" ~sdocs:_common_options ~doc ~man

let import_cmd =
  let filename =
    let doc = "Path of a previously-exported VM" in
    Arg.(value & opt (some file) None & info [ "filename" ] ~doc) in
  let metadata =
    let doc = "Import the VM metadata only." in
    Arg.(value & flag & info [ "metadata-only" ] ~doc) in
  let doc = "import a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Import a VM from a filesystem";
    `P "A previously exported VM is reloaded from the filesystem.";
    `P "The VM should have been exported in native (not xm/xl) format
       by \"export\". If you have an xm/xl format metadata file, it
       can be imported using \"add\".";
    `S "ERRORS";
    `P "Something about duplicate uuids." ] @ help in
  Term.(ret (pure Xn.import $common_options_t $ metadata $ filename)),
  Term.info "import" ~sdocs:_common_options ~doc ~man

let export_cmd =
  let vm = vm_arg "exported" in
  let filename =
    let doc = "Path to create in the filesystem" in
    Arg.(value & opt (some string) None & info [ "filename" ] ~doc) in
  let metadata =
    let doc = "Export the VM metadata only" in
    Arg.(value & flag & info [ "metadata-only" ] ~doc) in
  let xm =
    let doc = "Export in xm/xl format, instead of native" in
    Arg.(value & flag & info [ "xm" ] ~doc) in
  let doc = "export a VM" in
  let man = [
    `S "DESCRIPTION";
    `P "Export a VM to the filesystem";
    `P "A currently registered VM is exported to the filesystem.";
    `S "ERRORS";
    `P "Something about power states." ] @ help in
  Term.(ret (pure Xn.export $common_options_t $ metadata $ xm $ filename $ vm)),
  Term.info "export" ~sdocs:_common_options ~doc ~man

let diagnostics_cmd =
  let doc = "retrieve diagnostic information" in
  let man = [
    `S "DESCRIPTION";
    `P "Retrieve diagnostic information from the xenopsd service.";
  ] @ help in
  Term.(ret (pure Xn.diagnostics $ common_options_t)),
  Term.info "diagnostics" ~sdocs:_common_options ~doc ~man

let tasks_cmd =
  let doc = "List in-progress tasks" in
  let man = [
    `S "DESCRIPTION";
    `P "Describe the set of in-progress tasks.";
  ] @ help in
  Term.(ret (pure Xn.task_list $ common_options_t)),
  Term.info "tasks" ~sdocs:_common_options ~doc ~man

let task_cancel_cmd =
  let doc = "Cancel an in-progress task" in
  let man = [
    `S "DESCRIPTION";
    `P "Attempt to cancel an in-progress task. The task should either complete, fail within a short amount of time.";
  ] @ help in
  let task =
    let doc = "Task id to cancel" in
    Arg.(value & pos 0 (some string) None & info [] ~doc) in
  Term.(ret (pure Xn.task_cancel $ common_options_t $ task)),
  Term.info "task-cancel" ~sdocs:_common_options ~doc ~man

let cd_eject_cmd =
  let doc = "Eject a CDROM" in
  let man = [
    `S "DESCRIPTION";
    `P "Eject a CDROM from a CDROM drive";
  ] @ help in
  let vbd =
    let doc = "VBD id" in
    Arg.(value & pos 0 (some string) None & info [] ~doc) in
  Term.(ret (pure Xn.cd_eject $ common_options_t $ vbd)),
  Term.info "cd-eject" ~sdocs:_common_options ~doc ~man  

let stat_vm_cmd =
  let doc = "Query the runtime status of a running VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Query the runtime status of a running VM.";
  ] @ help in
  let vm =
    let doc = "The uuid of the VM to stat." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"uuid") in
  Term.(ret (pure Xn.stat_vm $ common_options_t $ vm)),
  Term.info "vm-stat" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "interact with the XCP xenopsd VM management service" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "xenops-cli" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [list_cmd; create_cmd; add_cmd; remove_cmd; start_cmd; shutdown_cmd; reboot_cmd;
            suspend_cmd; resume_cmd; pause_cmd; unpause_cmd;
            import_cmd; export_cmd; console_cmd; diagnostics_cmd; events_cmd;
            tasks_cmd; task_cancel_cmd; cd_eject_cmd; stat_vm_cmd ]

let _ =
  Xcp_client.use_switch := false;
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
