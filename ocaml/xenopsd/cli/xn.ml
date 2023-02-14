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

open Xenops_interface
open Xenops_client

let diagnose_error f =
  try f ()
  with e -> (
    Printf.fprintf stderr "Caught exception: %s\n" (Printexc.to_string e) ;
    match e with
    | Unix.Unix_error (Unix.EACCES, _, _) ->
        Printf.fprintf stderr "Access was denied (EACCES).\n" ;
        let uid = Unix.geteuid () in
        if uid <> 0 then (
          Printf.fprintf stderr "My effective uid is %d.\n" uid ;
          Printf.fprintf stderr "\nPlease switch to root (uid 0) and retry.\n" ;
          exit 1
        ) else (
          Printf.fprintf stderr
            "I observe that my effective uid is 0 (i.e. I'm running as root).\n" ;
          Printf.fprintf stderr
            "\n\
             Investigate the settings of any active security software \
             (selinux) and retry.\n" ;
          exit 1
        )
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
        Printf.fprintf stderr
          "Connection to the server was refused (ECONNREFUSED).\n" ;
        Printf.fprintf stderr
          "\nPlease start (or restart) the xenopsd service and retry.\n" ;
        exit 1
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Printf.fprintf stderr "The server socket does not exist (ENOENT).\n" ;
        Printf.fprintf stderr "\nPossible fixes include:\n" ;
        Printf.fprintf stderr
          "1. Start the xenopsd service; it will create the socket when it is \
           started;\n" ;
        Printf.fprintf stderr
          "2. Override the default path using the --socket=<path> option;\n" ;
        exit 1
    | Unix.Unix_error (Unix.EISDIR, _, _) ->
        Printf.fprintf stderr
          "The path refered to a directory. I require a file.\n" ;
        Printf.fprintf stderr "\nPlease check the path and retry.\n" ;
        exit 1
    | _ ->
        Printf.fprintf stderr
          "I don't have any relevant diagnostic advice. Please re-read the \
           documentation\n\
           and if you can't resolve the problem, send an email to \
           <xen-api@lists.xen.org>.\n" ;
        exit 1
  )

let usage () =
  Printf.fprintf stderr
    "%s <command> [args] - send commands to the xenops daemon\n" Sys.argv.(0) ;
  Printf.fprintf stderr "%s add <config> - add a VM from <config>\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s list [verbose] - query the states of known VMs\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s remove <name or id> - forget about a VM\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s start <name or id> [paused] - start a VM\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s pause <name or id> - pause a VM\n" Sys.argv.(0) ;
  Printf.fprintf stderr "%s unpause <name or id> - unpause a VM\n" Sys.argv.(0) ;
  Printf.fprintf stderr "%s shutdown <name or id> - shutdown a VM\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s reboot <name or id> - reboot a VM\n" Sys.argv.(0) ;
  Printf.fprintf stderr "%s suspend <name or id> <disk> - suspend a VM\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s resume <name or id> <disk> - resume a VM\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s migrate <name or id> <url> - migrate a VM to <url>\n" Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s vbd-list <name or id> - query the states of a VM's block devices\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s console-list <name or id> - query the states of a VM's consoles\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s pci-add <name or id> <number> <bdf> - associate the PCI device <bdf> \
     with <name or id>\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s pci-remove <name or id> <number> - disassociate the PCI device <bdf> \
     with <name or id>\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s pci-list <name or id> - query the states of a VM's PCI devices\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s cd-insert <id> <disk> - insert a CD into a VBD\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s cd-eject <id> - eject a CD from a VBD\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s export-metadata <id> - export metadata associated with <id>\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s export-metadata-xm <id> - export metadata associated with <id> in xm \
     format\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s delay <id> <time> - add an explicit delay of length <time> to this \
     VM's queue\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s events-watch - display all events generated by the server\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr
    "%s set-worker-pool-size <threads> - set the size of the worker pool\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s diagnostics - display diagnostic information\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s task-list - display the state of all known tasks\n"
    Sys.argv.(0) ;
  Printf.fprintf stderr "%s shutdown - shutdown the xenops service\n"
    Sys.argv.(0) ;
  ()

let dbg = "xn"

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

(* Grabs the result from a task and destroys it *)
let success_task f id =
  finally
    (fun () ->
      let t = Client.TASK.stat dbg id in
      match t.Task.state with
      | Task.Completed _ ->
          f t
      | Task.Failed x -> (
          let exn =
            match Rpcmarshal.unmarshal Errors.error.Rpc.Types.ty x with
            | Ok x ->
                Xenopsd_error x
            | Error (`Msg x) ->
                Xenopsd_error
                  (Internal_error
                     (Printf.sprintf "Error unmarshalling failure: %s" x)
                  )
          in
          match exn with
          | Xenopsd_error (Failed_to_contact_remote_service x) ->
              Printf.printf "Failed to contact remote service on: %s\n" x ;
              Printf.printf "Check the address and credentials.\n" ;
              exit 1
          | _ ->
              raise exn
        )
      | Task.Pending _ ->
          failwith "task pending"
    )
    (fun () -> Client.TASK.destroy dbg id)

let parse_source x =
  match
    List.filter (fun x -> x <> "") (Astring.String.cuts ~sep:":" ~empty:false x)
  with
  | ["phy"; path] ->
      Some (Local path)
  | ["sm"; path] ->
      Some (VDI path)
  | ["file"; _path] ->
      Printf.fprintf stderr
        "I don't understand 'file' disk paths. Please use 'phy'.\n" ;
      exit 2
  | [] ->
      None (* empty *)
  | _ ->
      Printf.fprintf stderr
        "I don't understand '%s'. Please use 'phy:path,...\n" x ;
      exit 2

let print_source = function
  | None ->
      ""
  | Some (Local path) ->
      Printf.sprintf "phy:%s" path
  | Some (VDI path) ->
      Printf.sprintf "sm:%s" path

let print_pci x =
  let open Pci in
  let open Xn_cfg_types in
  let msi =
    match x.msitranslate with
    | None ->
        ""
    | Some true ->
        Printf.sprintf ",%s=1" _msitranslate
    | Some false ->
        Printf.sprintf ",%s=0" _msitranslate
  in
  let power =
    match x.power_mgmt with
    | None ->
        ""
    | Some true ->
        Printf.sprintf ",%s=1" _power_mgmt
    | Some false ->
        Printf.sprintf ",%s=0" _power_mgmt
  in
  Printf.sprintf "%04x:%02x:%02x.%01x%s%s" x.address.domain x.address.bus
    x.address.dev x.address.fn msi power

let parse_pci vm_id (x, idx) =
  match Re.Str.split_delim (Re.Str.regexp "[,]") x with
  | bdf :: options ->
      let hex x = int_of_string ("0x" ^ x) in
      let parse_dev_fn x =
        match Re.Str.split_delim (Re.Str.regexp "[.]") x with
        | [dev; fn] ->
            (hex dev, hex fn)
        | _ ->
            Printf.fprintf stderr
              "Failed to parse BDF: %s. It should be '[DDDD:]BB:VV.F'\n" bdf ;
            exit 2
      in
      let domain, bus, dev, fn =
        match Re.Str.split_delim (Re.Str.regexp "[:]") bdf with
        | [domain; bus; dev_dot_fn] ->
            let dev, fn = parse_dev_fn dev_dot_fn in
            (hex domain, hex bus, dev, fn)
        | [bus; dev_dot_fn] ->
            let dev, fn = parse_dev_fn dev_dot_fn in
            (0, hex bus, dev, fn)
        | _ ->
            Printf.fprintf stderr
              "Failed to parse BDF: %s. It should be '[DDDD:]BB:VV.F'\n" bdf ;
            exit 2
      in
      let options =
        List.map
          (fun x ->
            match Re.Str.bounded_split_delim (Re.Str.regexp "[=]") x 2 with
            | [k; v] ->
                (k, v)
            | _ ->
                Printf.fprintf stderr
                  "Failed to parse PCI option: %s. It should be key=value.\n" x ;
                exit 2
          )
          options
      in
      let bool_opt k opts =
        if List.mem_assoc k opts then Some (List.assoc k opts = "1") else None
      in
      let open Xn_cfg_types in
      let msitranslate = bool_opt _msitranslate options in
      let power_mgmt = bool_opt _power_mgmt options in
      let address = {Pci.domain; bus; dev; fn} in
      {
        Pci.id= (vm_id, string_of_int idx)
      ; position= idx
      ; address
      ; msitranslate
      ; power_mgmt
      }
  | _ ->
      Printf.fprintf stderr
        "Failed to parse PCI '%s'. It should be \
         '[DDDD:]BB:VV.F[,option1[,option2]]'."
        x ;
      exit 2

type disk_info = {
    id: string
  ; ty: Vbd.ty
  ; position: Device_number.t
  ; mode: Vbd.mode
  ; disk: disk option
}

let parse_disk_info x =
  match Re.Str.split_delim (Re.Str.regexp "[,]") x with
  | [source; device_number; rw] ->
      let ty, device_number, device_number' =
        match Re.Str.split_delim (Re.Str.regexp "[:]") device_number with
        | [x] ->
            (Vbd.Disk, x, Device_number.of_string false x)
        | [x; "floppy"] ->
            (Vbd.Floppy, x, Device_number.of_string false x)
        | [x; "cdrom"] ->
            (Vbd.CDROM, x, Device_number.of_string false x)
        | _ ->
            Printf.fprintf stderr
              "Failed to understand disk name '%s'. It should be 'xvda' or \
               'hda:cdrom'\n"
              device_number ;
            exit 2
      in
      let mode =
        match String.lowercase_ascii rw with
        | "r" ->
            Vbd.ReadOnly
        | "w" ->
            Vbd.ReadWrite
        | x ->
            Printf.fprintf stderr
              "Failed to understand disk mode '%s'. It should be 'r' or 'w'\n" x ;
            exit 2
      in
      let backend = parse_source source in
      {id= device_number; ty; position= device_number'; mode; disk= backend}
  | _ ->
      Printf.fprintf stderr
        "I don't understand '%s'. Please use 'phy:path,xvda,w'\n" x ;
      exit 2

let vbd_of_disk_info vm_id info =
  {
    Vbd.id= (vm_id, info.id)
  ; position= Some info.position
  ; mode= info.mode
  ; backend= info.disk
  ; ty= info.ty
  ; unpluggable= true
  ; extra_backend_keys= []
  ; extra_private_keys= []
  ; qos= None
  ; persistent= true
  }

let print_disk vbd =
  let device_number = snd vbd.Vbd.id in
  let mode =
    match vbd.Vbd.mode with Vbd.ReadOnly -> "r" | Vbd.ReadWrite -> "w"
  in
  let ty =
    match vbd.Vbd.ty with
    | Vbd.CDROM ->
        ":cdrom"
    | Vbd.Floppy ->
        ":floppy"
    | Vbd.Disk ->
        ""
  in
  let source = print_source vbd.Vbd.backend in
  Printf.sprintf "%s,%s%s,%s" source device_number ty mode

let print_vif vif =
  let mac =
    if vif.Vif.mac = "" then "" else Printf.sprintf "mac=%s" vif.Vif.mac
  in
  let bridge =
    match vif.Vif.backend with
    | Network.Local x ->
        Printf.sprintf "bridge=%s" x
    | Network.Sriov _ ->
        failwith "unimplemented"
    | Network.Remote (_, _) ->
        Printf.fprintf stderr "Cannot handle backend = Netback(_, _)\n%!" ;
        exit 2
  in
  String.concat "," [mac; bridge]

let parse_vif vm_id (x, idx) =
  let open Xn_cfg_types in
  let xs =
    List.filter
      (fun x -> x <> "")
      (Re.Str.split_delim (Re.Str.regexp "[ \t]*,[ \t]*") x)
  in
  let kvpairs =
    List.map
      (fun x ->
        match Re.Str.bounded_split_delim (Re.Str.regexp "[=]") x 2 with
        | [k; v] ->
            (k, v)
        | _ ->
            Printf.fprintf stderr
              "I don't understand '%s'. Please use \
               'mac=xx:xx:xx:xx:xx:xx,bridge=xenbrX'.\n"
              x ;
            exit 2
      )
      xs
  in
  {
    Vif.id= (vm_id, string_of_int idx)
  ; position= idx
  ; mac= (if List.mem_assoc _mac kvpairs then List.assoc _mac kvpairs else "")
  ; carrier= true
  ; mtu= 1500
  ; rate= None
  ; backend=
      ( if List.mem_assoc _bridge kvpairs then
          Network.Local (List.assoc _bridge kvpairs)
      else
        Network.Local "xenbr0"
      )
  ; other_config= []
  ; locking_mode= Vif.default_locking_mode
  ; extra_private_keys= []
  ; ipv4_configuration= Unspecified4
  ; ipv6_configuration= Unspecified6
  ; pvs_proxy= None
  ; vlan= None
  }

let print_vm id =
  let open Xn_cfg_types in
  let vm_t, _ = Client.VM.stat dbg id in
  let quote x = Printf.sprintf "'%s'" x in
  let boot =
    match vm_t.ty with
    | PV {boot; _} -> (
      match boot with
      | Direct {kernel= k; cmdline= c; ramdisk= i} -> (
          [(_builder, quote "generic"); (_kernel, quote k); (_root, quote c)]
          @ match i with None -> [] | Some x -> [(_ramdisk, x)]
        )
      | Indirect {bootloader= b; _} ->
          [(_builder, quote "generic"); (_bootloader, quote b)]
    )
    | HVM {boot_order= b; _} ->
        [(_builder, quote "hvm"); (_boot, quote b)]
    | PVinPVH _ | PVH _ ->
        failwith "unimplemented"
  in
  let name = [(_name, quote vm_t.name)] in
  let vcpus = [(_vcpus, string_of_int vm_t.vcpus)] in
  let bytes_to_mib x = Int64.div x (Int64.mul 1024L 1024L) in
  let memory =
    [(_memory, vm_t.memory_static_max |> bytes_to_mib |> Int64.to_string)]
  in
  let vbds = Client.VBD.list dbg id |> List.map fst in
  let vbds =
    [
      ( _disk
      , Printf.sprintf "[ %s ]"
          (String.concat ", "
             (List.map (fun x -> Printf.sprintf "'%s'" (print_disk x)) vbds)
          )
      )
    ]
  in
  let vifs = Client.VIF.list dbg id |> List.map fst in
  let vifs =
    [
      ( _vif
      , Printf.sprintf "[ %s ]"
          (String.concat ", "
             (List.map (fun x -> Printf.sprintf "'%s'" (print_vif x)) vifs)
          )
      )
    ]
  in
  let pcis = Client.PCI.list dbg id |> List.map fst in
  (* Sort into order based on position *)
  let pcis =
    List.sort (fun a b -> compare a.Pci.position b.Pci.position) pcis
  in
  let pcis =
    [
      ( _pci
      , Printf.sprintf "[ %s ]"
          (String.concat ", "
             (List.map (fun x -> Printf.sprintf "'%s'" (print_pci x)) pcis)
          )
      )
    ]
  in
  let global_pci_opts =
    [
      (_vm_pci_msitranslate, if vm_t.pci_msitranslate then "1" else "0")
    ; (_vm_pci_power_mgmt, if vm_t.pci_power_mgmt then "1" else "0")
    ]
  in
  String.concat "\n"
    (List.map
       (fun (k, v) -> Printf.sprintf "%s=%s" k v)
       (name @ boot @ vcpus @ memory @ vbds @ vifs @ pcis @ global_pci_opts)
    )

let canonicalise_filename x =
  try
    Unix.access x [Unix.R_OK] ;
    Filename.(if is_relative x then concat (Unix.getcwd ()) x else x)
  with _ ->
    Printf.fprintf stderr "Cannot find file: %s\n%!" x ;
    exit 1

let add' _copts x () =
  match x with
  | None ->
      `Error (false, "You must supply a path to a VM metadata file.")
  | Some filename ->
      let ic = open_in filename in
      finally
        (fun () ->
          let lexbuf = Lexing.from_channel ic in
          let config = Xn_cfg_parser.file Xn_cfg_lexer.token lexbuf in
          let open Xn_cfg_types in
          let mem x = List.mem_assoc x config in
          let find x = List.assoc x config in
          let find_opt x = List.assoc_opt x config in
          let any xs = List.exists mem xs in
          let pv =
            false
            || mem _builder
               && List.mem (find _builder |> string) ["linux"; "generic"]
            || ((not (mem _builder)) && any [_bootloader; _kernel])
          in
          (* We need to have the disk information ready so we can set the PV
             indirect boot info in the VM record *)
          let disks = if mem _disk then find _disk |> list string else [] in
          let disks = List.map parse_disk_info disks in
          let devices =
            List.rev
              (List.fold_left
                 (fun acc x ->
                   match x.disk with None -> acc | Some x -> x :: acc
                 )
                 [] disks
              )
          in
          let open Vm in
          let builder_info =
            match pv with
            | true ->
                PV
                  {
                    framebuffer= false
                  ; framebuffer_ip= Some "0.0.0.0"
                  ; vncterm= true
                  ; vncterm_ip= Some "0.0.0.0"
                  ; pci_passthrough= false
                  ; boot=
                      ( if mem _bootloader then
                          Indirect
                            {
                              bootloader= find _bootloader |> string
                            ; extra_args= ""
                            ; legacy_args= ""
                            ; bootloader_args= ""
                            ; devices
                            }
                      else if mem _kernel then
                        Direct
                          {
                            kernel=
                              find _kernel |> string |> canonicalise_filename
                          ; cmdline=
                              (if mem _root then find _root |> string else "")
                          ; ramdisk=
                              ( if mem _ramdisk then
                                  Some
                                    (find _ramdisk
                                    |> string
                                    |> canonicalise_filename
                                    )
                              else
                                None
                              )
                          }
                      else (
                        List.iter
                          (Printf.fprintf stderr "%s\n")
                          [
                            "I couldn't determine how to start this VM."
                          ; Printf.sprintf
                              "A PV guest needs either %s or %s and %s"
                              _bootloader _kernel _ramdisk
                          ] ;
                        exit 1
                      )
                      )
                  }
            | false ->
                HVM
                  {
                    hap= true
                  ; shadow_multiplier= 1.
                  ; timeoffset= ""
                  ; video_mib= 4
                  ; video= Cirrus
                  ; acpi= true
                  ; serial= None
                  ; keymap= None
                  ; vnc_ip= Some "0.0.0.0"
                  ; pci_emulations= []
                  ; pci_passthrough= false
                  ; boot_order=
                      (if mem _boot then find _boot |> string else "cd")
                  ; qemu_disk_cmdline= false
                  ; qemu_stubdom= false
                  ; firmware= Xenops_types.Vm.default_firmware
                  ; tpm=
                      ( match find_opt _vtpm with
                      | Some id ->
                          Some
                            (Vtpm (string id |> Uuidm.of_string |> Option.get))
                      | _ ->
                          None
                      )
                  }
          in
          let uuid =
            if mem _uuid then
              find _uuid |> string
            else
              Uuidx.(to_string (make ()))
          in
          let name = if mem _name then find _name |> string else uuid in
          let mib =
            if mem _memory then find _memory |> int |> Int64.of_int else 64L
          in
          let bytes = Int64.mul 1024L (Int64.mul 1024L mib) in
          let vcpus = if mem _vcpus then find _vcpus |> int else 1 in
          let pci_msitranslate =
            if mem _vm_pci_msitranslate then
              find _vm_pci_msitranslate |> bool
            else
              true
          in
          let pci_power_mgmt =
            if mem _vm_pci_power_mgmt then
              find _vm_pci_power_mgmt |> bool
            else
              false
          in
          let has_vendor_device =
            if mem _vm_has_vendor_device then
              find _vm_has_vendor_device |> bool
            else
              false
          in
          let vm =
            {
              id= uuid
            ; name
            ; ssidref= 0l
            ; xsdata= []
            ; platformdata=
                [
                  (* HVM defaults *)
                  ("nx", "false")
                ; ("acpi", "true")
                ; ("apic", "true")
                ; ("pae", "true")
                ; ("viridian", "true")
                ]
            ; bios_strings= []
            ; ty= builder_info
            ; suppress_spurious_page_faults= false
            ; machine_address_size= None
            ; memory_static_max= bytes
            ; memory_dynamic_max= bytes
            ; memory_dynamic_min= bytes
            ; vcpu_max= vcpus
            ; vcpus
            ; scheduler_params= {priority= None; affinity= []}
            ; on_crash= [Vm.Shutdown]
            ; on_shutdown= [Vm.Shutdown]
            ; on_reboot= [Vm.Start]
            ; on_softreboot= [Vm.Softreboot]
            ; pci_msitranslate
            ; pci_power_mgmt
            ; has_vendor_device
            ; generation_id= None
            }
          in
          let (id : Vm.id) = Client.VM.add dbg vm in
          let one x = x |> vbd_of_disk_info id |> Client.VBD.add dbg in
          let (_ : Vbd.id list) = List.map one disks in
          let vifs = if mem _vif then find _vif |> list string else [] in
          let rec ints first last =
            if first > last then [] else first :: ints (first + 1) last
          in
          let vifs = List.combine vifs (ints 0 (List.length vifs - 1)) in
          let one x = x |> parse_vif id |> Client.VIF.add dbg in
          let (_ : Vif.id list) = List.map one vifs in
          let pcis = if mem _pci then find _pci |> list string else [] in
          let pcis = List.combine pcis (ints 0 (List.length pcis - 1)) in
          let one x = x |> parse_pci id |> Client.PCI.add dbg in
          let (_ : Pci.id list) = List.map one pcis in
          Printf.fprintf stdout "%s\n" id ;
          `Ok id
        )
        (fun () -> close_in ic)

let add copts x () =
  match add' copts x () with
  | `Ok id ->
      Printf.fprintf stdout "%s\n" id ;
      `Ok ()
  | `Error (a, b) ->
      `Error (a, b)

let add copts x = diagnose_error (add copts x)

let rpc_of t v = Rpcmarshal.marshal t.Rpc.Types.ty v

let string_of_power_state = function
  | Running ->
      "Running"
  | Suspended ->
      "Suspend"
  | Halted ->
      "Halted "
  | Paused ->
      "Paused "

let list_verbose () =
  let vms = Client.VM.list dbg () in
  List.iter
    (fun (vm, state) ->
      Printf.printf "%-45s%-5s\n" vm.Vm.name
        (state.Vm.power_state |> string_of_power_state) ;
      Printf.printf "  %s\n" (vm |> rpc_of Vm.t |> Jsonrpc.to_string) ;
      Printf.printf "  %s\n" (state |> rpc_of Vm.state |> Jsonrpc.to_string)
    )
    vms

let list_compact () =
  let open Vm in
  let line name domid mem vcpus state time =
    Printf.sprintf "%-45s%-5s%-6s%-5s     %-8s%s" name domid mem vcpus state
      time
  in
  let header = line "Name" "ID" "Mem" "VCPUs" "State" "Time(s)" in
  let string_of_vm (vm, state) =
    let domid =
      match state.Vm.power_state with
      | Running ->
          String.concat "," (List.map string_of_int state.Vm.domids)
      | _ ->
          "-"
    in
    let mem =
      Int64.to_string (Int64.div (Int64.div vm.memory_static_max 1024L) 1024L)
    in
    let vcpus = string_of_int state.vcpu_target in
    let state = state.Vm.power_state |> string_of_power_state in
    line vm.name domid mem vcpus state ""
  in
  let vms = Client.VM.list dbg () in
  let lines = header :: List.map string_of_vm vms in
  List.iter (Printf.printf "%s\n") lines

let list copts =
  diagnose_error (if copts.Common.verbose then list_verbose else list_compact)

type t = Line of string | Block of t list

let pp x =
  let open Rpc in
  let rec to_string_list = function
    | Line x ->
        [x]
    | Block xs ->
        let xs' = List.map to_string_list xs |> List.concat in
        List.map (fun x -> "    " ^ x) xs'
  in
  let flatten xs =
    let rec aux line = function
      | Line x :: xs ->
          aux (if line <> "" then line ^ " " ^ x else x) xs
      | Block x :: xs ->
          (if line <> "" then [Line line] else [])
          @ [Block (aux "" x)]
          @ aux "" xs
      | [] ->
          if line <> "" then [Line line] else []
    in
    aux "" xs
  in
  let rec to_t = function
    | Int32 x ->
        [Line (Printf.sprintf "%d" (Int32.to_int x))]
    | Int x ->
        [Line (Printf.sprintf "%Ld" x)]
    | Bool x ->
        [Line (Printf.sprintf "%b" x)]
    | Float x ->
        [Line (Printf.sprintf "%g" x)]
    | String x ->
        [Line x]
    | DateTime x ->
        [Line x]
    | Enum [] ->
        [Line "[]"]
    | Enum xs ->
        [Line "["; Block (List.concat (List.map to_t xs)); Line "]"]
    | Dict [] ->
        [Line "{}"]
    | Dict xs ->
        [
          Line "{"
        ; Block
            (List.concat (List.map (fun (s, t) -> Line (s ^ ": ") :: to_t t) xs))
        ; Line "}"
        ]
    | Base64 x ->
        [Line x]
    | Null ->
        []
  in
  x
  |> to_t
  |> flatten
  |> List.map to_string_list
  |> List.concat
  |> List.iter (Printf.printf "%s\n")

let diagnostics' () =
  Client.get_diagnostics dbg () |> Jsonrpc.of_string |> pp ;
  `Ok ()

let stat_vm _ id =
  let _vm_t, vm_stat = Client.VM.stat dbg id in
  let kvs =
    match rpc_of Vm.state vm_stat with
    | Dict kvs ->
        List.map (fun (k, v) -> (k, Jsonrpc.to_string v)) kvs
    | _ ->
        []
  in
  List.iter (fun (k, v) -> Printf.fprintf stdout "%30s: %s\n" k v) kvs ;
  `Ok ()

let diagnostics _copts = diagnose_error diagnostics'

let find_by_name x =
  let open Vm in
  let all = Client.VM.list dbg () in
  let this_one (y, _) = y.id = x || y.name = x in
  try List.find this_one all
  with Not_found ->
    Printf.fprintf stderr "Failed to find VM: %s\n" x ;
    exit 1

let remove _copts x =
  let open Vm in
  let vm, _ = find_by_name x in
  let vbds = Client.VBD.list dbg vm.id in
  List.iter (fun (vbd, _) -> Client.VBD.remove dbg vbd.Vbd.id) vbds ;
  let vifs = Client.VIF.list dbg vm.id in
  List.iter (fun (vif, _) -> Client.VIF.remove dbg vif.Vif.id) vifs ;
  Client.VM.remove dbg vm.id

let need_vm f x () =
  match x with
  | None ->
      `Error (false, "You must supply a VM name or UUID")
  | Some x ->
      let () = f x in
      `Ok ()

let remove copts x = diagnose_error (need_vm (remove copts) x)

let export_metadata _copts filename x =
  let open Vm in
  let vm, _ = find_by_name x in
  let txt = Client.VM.export_metadata dbg vm.id in
  let oc = open_out filename in
  finally (fun () -> output_string oc txt) (fun () -> close_out oc)

let export_metadata_xm _copts filename x : unit =
  let open Vm in
  let vm, _ = find_by_name x in
  let txt = print_vm vm.id in
  let oc = open_out filename in
  finally (fun () -> output_string oc txt) (fun () -> close_out oc)

let export copts metadata xm filename (x : Vm.id option) () =
  if not metadata then
    `Error (false, "Unfortunately I only support metadata import")
  else
    match x with
    | None ->
        `Error (false, "Please supply a VM uuid or name")
    | Some x -> (
      match filename with
      | None ->
          `Error (false, "Please provide a filename")
      | Some f ->
          ( if xm then
              export_metadata_xm
          else
            export_metadata
          )
            copts f x ;
          `Ok ()
    )

let export copts metadata xm filename x =
  diagnose_error (export copts metadata xm filename x)

let delay x t =
  let vm, _ = find_by_name x in
  Client.VM.delay dbg vm.Vm.id t
  |> wait_for_task dbg
  |> success_task ignore_task

let import_metadata _copts filename =
  let ic = open_in filename in
  let buf = Buffer.create 128 in
  let line = Bytes.make 128 '\000' in
  finally
    (fun () ->
      try
        while true do
          match input ic line 0 (Bytes.length line) with
          | 0 ->
              raise End_of_file
          | n ->
              Buffer.add_bytes buf (Bytes.sub line 0 n)
        done
      with End_of_file -> ()
    )
    (fun () -> close_in ic) ;
  let txt = Buffer.contents buf in
  let id = Client.VM.import_metadata dbg txt in
  Printf.printf "%s\n" id

let import copts metadata filename () =
  if not metadata then
    `Error (false, "Unfortunately I only support metadata import")
  else
    match filename with
    | None ->
        `Error (false, "Please provide a filename")
    | Some f ->
        import_metadata copts f ; `Ok ()

let import copts metadata filename =
  diagnose_error (import copts metadata filename)

let shutdown _copts timeout x =
  let open Vm in
  let vm, _ = find_by_name x in
  Client.VM.shutdown dbg vm.id timeout
  |> wait_for_task dbg
  |> success_task ignore_task

let shutdown copts timeout x =
  diagnose_error (need_vm (shutdown copts timeout) x)

let pause _copts x =
  let open Vm in
  let vm, _ = find_by_name x in
  Client.VM.pause dbg vm.id |> wait_for_task dbg |> success_task ignore_task

let pause copts x = diagnose_error (need_vm (pause copts) x)

let unpause _copts x =
  let open Vm in
  let vm, _ = find_by_name x in
  Client.VM.unpause dbg vm.id |> wait_for_task dbg |> success_task ignore_task

let unpause copts x = diagnose_error (need_vm (unpause copts) x)

let reboot _copts timeout x =
  let open Vm in
  let vm, _ = find_by_name x in
  Client.VM.reboot dbg vm.id timeout
  |> wait_for_task dbg
  |> success_task ignore_task

let reboot copts timeout x = diagnose_error (need_vm (reboot copts timeout) x)

let suspend _copts disk x =
  (* We don't currently know how to create a fresh disk *)
  let disk =
    match disk with
    | None ->
        Printf.fprintf stderr "Please specify a block device.\n" ;
        exit 1
    | Some disk ->
        disk
  in
  let open Vm in
  let vm, _ = find_by_name x in
  Client.VM.suspend dbg vm.id (Local disk)
  |> wait_for_task dbg
  |> success_task ignore_task

let suspend copts disk x = diagnose_error (need_vm (suspend copts disk) x)

let resume _copts disk x =
  (* We don't currently store where the suspend image is *)
  let disk =
    match disk with
    | None ->
        Printf.fprintf stderr "Please specify a block device.\n" ;
        exit 1
    | Some disk ->
        disk
  in
  let open Vm in
  let vm, _ = find_by_name x in
  Client.VM.resume dbg vm.id (Local disk)
  |> wait_for_task dbg
  |> success_task ignore_task

let resume copts disk x = diagnose_error (need_vm (resume copts disk) x)

let migrate ~id ~url ~compress ~verify_dest =
  let vm, _ = find_by_name id in
  let bool b =
    match String.lowercase_ascii b with
    | "t" | "true" | "on" | "1" ->
        true
    | _ ->
        false
  in
  Client.VM.migrate dbg vm.Vm.id [] [] [] url (bool compress) (bool verify_dest)
  |> wait_for_task dbg

let trim limit str =
  let l = String.length str in
  if l < limit then
    str
  else
    "..." ^ String.sub str (l - limit + 3) (limit - 3)

let vbd_list x =
  let vm, _ = find_by_name x in
  let vbds = Client.VBD.list dbg vm.Vm.id in
  let line id position mode ty plugged disk disk2 =
    Printf.sprintf "%-10s %-8s %-4s %-5s %-7s %-35s %-35s " id position mode ty
      plugged disk disk2
  in
  let header =
    line "id" "position" "mode" "type" "plugged" "disk" "xenstore_disk"
  in
  let lines =
    List.map
      (fun (vbd, state) ->
        let id = snd vbd.Vbd.id in
        let position =
          match vbd.Vbd.position with
          | None ->
              "None"
          | Some x ->
              Device_number.to_linux_device x
        in
        let mode = if vbd.Vbd.mode = Vbd.ReadOnly then "RO" else "RW" in
        let ty =
          match vbd.Vbd.ty with
          | Vbd.CDROM ->
              "CDROM"
          | Vbd.Floppy ->
              "Floppy"
          | Vbd.Disk ->
              "HDD"
        in
        let plugged = if state.Vbd.plugged then "X" else " " in
        let disk =
          match vbd.Vbd.backend with
          | None ->
              ""
          | Some (Local x) ->
              x |> trim 32
          | Some (VDI path) ->
              path |> trim 32
        in
        let info = Client.VBD.stat dbg vbd.Vbd.id in
        let disk2 =
          match (snd info).Vbd.backend_present with
          | None ->
              ""
          | Some (Local x) ->
              x |> trim 32
          | Some (VDI path) ->
              path |> trim 32
        in
        line id position mode ty plugged disk disk2
      )
      vbds
  in
  List.iter print_endline (header :: lines)

let console_list _copts x =
  let _, s = find_by_name x in
  Printf.fprintf stderr "json=[%s]\n%!" (Jsonrpc.to_string (rpc_of Vm.state s)) ;
  let line protocol port = Printf.sprintf "%-10s %-6s" protocol port in
  let header = line "protocol" "port" in
  let lines =
    List.map
      (fun c ->
        let protocol =
          match c.Vm.protocol with Vm.Rfb -> "RFB" | Vm.Vt100 -> "VT100"
        in
        line protocol (string_of_int c.Vm.port)
      )
      s.Vm.consoles
  in
  List.iter print_endline (header :: lines)

let raw_console_proxy sockaddr =
  let long_connection_retry_timeout = 5. in
  let with_raw_mode f =
    (* Remember the current terminal state so we can restore it *)
    let tc = Unix.tcgetattr Unix.stdin in
    (* Switch into a raw mode, passing through stuff like Control + C *)
    let tc' =
      {
        tc with
        Unix.c_ignbrk= false
      ; Unix.c_brkint= false
      ; Unix.c_parmrk= false
      ; Unix.c_istrip= false
      ; Unix.c_inlcr= false
      ; Unix.c_igncr= false
      ; Unix.c_icrnl= false
      ; Unix.c_ixon= false
      ; Unix.c_opost= false
      ; Unix.c_echo= false
      ; Unix.c_echonl= false
      ; Unix.c_icanon= false
      ; Unix.c_isig= false
      ; (* IEXTEN? *)
        Unix.c_csize= 8
      ; Unix.c_parenb= false
      ; Unix.c_vmin= 0
      ; Unix.c_vtime= 0
      }
    in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW tc' ;
    finally f (fun () -> Unix.tcsetattr Unix.stdin Unix.TCSANOW tc)
  in
  let proxy fd =
    (* The releatively complex design here helps to buffer input/output when the
       underlying connection temporarily breaks, hence provides seemingly
       continous connection. *)
    let block = 65536 in
    let buf_local = Bytes.make block '\000' in
    let buf_local_end = ref 0 in
    let buf_local_start = ref 0 in
    let buf_remote = Bytes.make block '\000' in
    let buf_remote_end = ref 0 in
    let buf_remote_start = ref 0 in
    let final = ref false in
    let finished = ref false in
    while not !finished do
      if !buf_local_start <> !buf_local_end then (
        let b =
          Unix.write Unix.stdout buf_local !buf_local_start
            (!buf_local_end - !buf_local_start)
        in
        buf_local_start := !buf_local_start + b ;
        if !buf_local_start = !buf_local_end then (
          buf_local_start := 0 ;
          buf_local_end := 0
        )
      ) else if !buf_remote_start <> !buf_remote_end then (
        let b =
          Unix.write fd buf_remote !buf_remote_start
            (!buf_remote_end - !buf_remote_start)
        in
        buf_remote_start := !buf_remote_start + b ;
        if !buf_remote_start = !buf_remote_end then (
          buf_remote_start := 0 ;
          buf_remote_end := 0
        )
      ) else if !final then
        finished := true
      else
        let r, _, _ = Unix.select [Unix.stdin; fd] [] [] (-1.) in
        if List.mem Unix.stdin r then (
          let b =
            Unix.read Unix.stdin buf_remote !buf_remote_end
              (block - !buf_remote_end)
          in
          let i = ref !buf_remote_end in
          while
            !i < !buf_remote_end + b
            && Char.code (Bytes.get buf_remote !i) <> 0x1d
          do
            incr i
          done ;
          if !i < !buf_remote_end + b then final := true ;
          buf_remote_end := !i
        ) ;
        if List.mem fd r then
          let b =
            Unix.read fd buf_local !buf_local_end (block - !buf_local_end)
          in
          buf_local_end := !buf_local_end + b
    done
  in
  let delay = ref 0.1 in
  let rec keep_connection () =
    try
      let s =
        Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
      in
      finally
        (fun () ->
          Unix.connect s sockaddr ;
          delay := 0.1 ;
          proxy s
        )
        (fun () -> Unix.close s)
    with
    | Unix.Unix_error (_, _, _) when !delay <= long_connection_retry_timeout ->
        ignore (Unix.select [] [] [] !delay) ;
        delay := !delay *. 2. ;
        keep_connection ()
    | e ->
        Printf.fprintf stderr "Console error: %s\n%!" (Printexc.to_string e)
  in
  with_raw_mode keep_connection

let xenconsoles =
  ["/usr/lib/xen-4.1/bin/xenconsole"; "/usr/lib/xen-4.2/bin/xenconsole"]

let vncviewer_binary =
  let n = "vncviewer" in
  let dirs =
    Re.Str.split_delim (Re.Str.regexp_string ":") (Unix.getenv "PATH")
  in
  List.fold_left
    (fun result dir ->
      match result with
      | Some x ->
          Some x
      | None ->
          let path = Filename.concat dir n in
          if
            try
              Unix.access path [Unix.X_OK] ;
              true
            with _ -> false
          then
            Some path
          else
            None
    )
    None dirs

let unix_proxy path =
  let unix = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  ( try Unix.connect unix (Unix.ADDR_UNIX path)
    with e -> Unix.close unix ; raise e
  ) ;
  let listen = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind listen (Unix.ADDR_INET (Unix.inet_addr_any, 0)) ;
  Unix.listen listen 5 ;
  let addr = Unix.getsockname listen in
  let port =
    match addr with Unix.ADDR_INET (_, port) -> port | _ -> assert false
  in
  match Unix.fork () with
  | 0 ->
      let buf = Bytes.make 16384 '\000' in
      let accept, _ = Unix.accept listen in
      let copy a b =
        while true do
          let n = Unix.read a buf 0 (Bytes.length buf) in
          if n = 0 then exit 0 ;
          let m = Unix.write b buf 0 n in
          if m = 0 then exit 0
        done
      in
      (match Unix.fork () with 0 -> copy unix accept | _ -> copy accept unix) ;
      0
  | _ ->
      port

let vncviewer port =
  match vncviewer_binary with
  | Some path ->
      Unix.execv path [|path; Printf.sprintf "localhost:%d" port|]
  | None ->
      ()

let console_connect' _copts x =
  let _, s = find_by_name x in
  let preference a b =
    match (a, b) with
    | {Vm.protocol= Vm.Rfb; _}, {Vm.protocol= Vm.Vt100; _} ->
        1
    | {Vm.protocol= Vm.Vt100; _}, {Vm.protocol= Vm.Rfb; _} ->
        -1
    | _, _ ->
        0
  in
  let consoles = List.sort preference s.Vm.consoles in
  let connect = function
    | {Vm.protocol= Vm.Vt100; path; _} when path <> "" ->
        raw_console_proxy (Unix.ADDR_UNIX path)
    | {Vm.protocol= Vm.Vt100; port; _} when port <> 0 ->
        raw_console_proxy
          (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port))
    | {Vm.protocol= Vm.Rfb; path; _} when path <> "" ->
        let port = unix_proxy path in
        vncviewer port
    | {Vm.protocol= Vm.Rfb; port; _} when port <> 0 ->
        vncviewer port
    | _ ->
        failwith "unimplemented"
  in
  List.iter connect consoles ;
  List.iter
    (fun exe ->
      if Sys.file_exists exe then
        Unix.execv exe [|exe; string_of_int (List.hd s.Vm.domids)|]
    )
    xenconsoles ;
  Printf.fprintf stderr "Failed to find a text console.\n%!" ;
  exit 1

let console_connect copts x = diagnose_error (need_vm (console_connect' copts) x)

let start' copts paused console x =
  let vm, _ = find_by_name x in
  Client.VM.start dbg vm.id false
  |> wait_for_task dbg
  |> success_task ignore_task ;
  if not paused then
    Client.VM.unpause dbg vm.id |> wait_for_task dbg |> success_task ignore_task ;
  if console then
    console_connect' copts x

let start copts paused console x =
  diagnose_error (need_vm (start' copts paused console) x)

let create copts x console () =
  match add' copts x () with
  | `Ok id ->
      start' copts false console id ;
      `Ok ()
  | `Error (a, b) ->
      `Error (a, b)

let create copts console x = diagnose_error (create copts console x)

let pci_add x idx bdf =
  let vm, _ = find_by_name x in
  let open Pci in
  let domain, bus, dev, fn =
    Scanf.sscanf bdf "%04x:%02x:%02x.%1x" (fun a b c d -> (a, b, c, d))
  in
  let address = {domain; bus; dev; fn} in
  let id =
    Client.PCI.add dbg
      {
        id= (vm.Vm.id, idx)
      ; position= int_of_string idx
      ; address
      ; msitranslate= None
      ; power_mgmt= None
      }
  in
  Printf.printf "%s.%s\n" (fst id) (snd id)

let pci_remove x idx =
  let vm, _ = find_by_name x in
  Client.PCI.remove dbg (vm.Vm.id, idx)

let pci_list x =
  let vm, _ = find_by_name x in
  let pcis = Client.PCI.list dbg vm.Vm.id in
  let line id bdf = Printf.sprintf "%-10s %-3s %-12s" id bdf in
  let header = line "id" "pos" "bdf" in
  let lines =
    List.map
      (fun (pci, _state) ->
        let open Pci in
        let id = snd pci.id in
        let bdf =
          Printf.sprintf "%04x:%02x:%02x.%01x" pci.address.domain
            pci.address.bus pci.address.dev pci.address.fn
        in
        line id (string_of_int pci.position) bdf
      )
      pcis
  in
  List.iter print_endline (header :: lines)

let find_vbd id =
  let vbd_id : Vbd.id =
    match Re.Str.bounded_split_delim (Re.Str.regexp "[.]") id 2 with
    | [a; b] ->
        (a, b)
    | _ ->
        Printf.fprintf stderr
          "Failed to parse VBD id: %s (expected VM.device)\n" id ;
        exit 1
  in
  let vm_id = fst vbd_id in
  let vm, _ = find_by_name vm_id in
  let vbds = Client.VBD.list dbg vm.Vm.id in
  let this_one (y, _) = snd y.Vbd.id = snd vbd_id in
  try List.find this_one vbds
  with Not_found ->
    Printf.fprintf stderr "Failed to find VBD: %s\n" id ;
    exit 1

let cd_eject _ = function
  | None ->
      `Error (true, "Please supply a VBD id")
  | Some id ->
      let vbd, _ = find_vbd id in
      Client.VBD.eject dbg vbd.Vbd.id |> wait_for_task dbg |> ignore ;
      `Ok ()

let cd_insert id disk =
  let vbd, _ = find_vbd id in
  match parse_source disk with
  | None ->
      Printf.fprintf stderr "Cannot insert a disk which doesn't exist\n" ;
      exit 1
  | Some backend ->
      Client.VBD.insert dbg vbd.Vbd.id backend |> wait_for_task dbg

let rec events_watch from =
  let _, events, next = Client.UPDATES.get dbg from None in
  let open Dynamic in
  let lines =
    List.map
      (function
        | Vm id ->
            Printf.sprintf "VM %s" id
        | Vbd id ->
            Printf.sprintf "VBD %s.%s" (fst id) (snd id)
        | Vif id ->
            Printf.sprintf "VIF %s.%s" (fst id) (snd id)
        | Pci id ->
            Printf.sprintf "PCI %s.%s" (fst id) (snd id)
        | Task id ->
            Printf.sprintf "Task %s" id
        | Vgpu id ->
            Printf.sprintf "VGPU %s.%s" (fst id) (snd id)
        | Vusb id ->
            Printf.sprintf "VUSB %s.%s" (fst id) (snd id)
        )
      events
  in
  List.iter (fun x -> Printf.printf "%-8d %s\n" next x) lines ;
  flush stdout ;
  events_watch (Some next)

let events _copts = events_watch None

let set_worker_pool_size size = Client.HOST.set_worker_pool_size dbg size

let print_date float =
  let time = Unix.gmtime float in
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ" (time.Unix.tm_year + 1900)
    (time.Unix.tm_mon + 1) time.Unix.tm_mday time.Unix.tm_hour time.Unix.tm_min
    time.Unix.tm_sec

let task_list _ =
  let all = Client.TASK.list dbg in
  List.iter
    (fun t ->
      Printf.printf "%-8s %-12s %-30s %s\n" t.Task.id (print_date t.Task.ctime)
        t.Task.dbg
        (t.Task.state |> rpc_of Task.state |> Jsonrpc.to_string) ;
      List.iter
        (fun (name, state) ->
          Printf.printf "  |_ %-30s %s\n" name
            (state |> rpc_of Task.state |> Jsonrpc.to_string)
        )
        t.Task.subtasks
    )
    all ;
  `Ok ()

let task_cancel _ = function
  | None ->
      `Error (true, "Please supply a task id")
  | Some id ->
      Client.TASK.cancel dbg id ; `Ok ()

let debug_shutdown () = Client.DEBUG.shutdown dbg ()

let verbose_task t =
  let string_of_state = function
    | Task.Completed t ->
        Printf.sprintf "%.2f" t.Task.duration
    | Task.Failed x ->
        Printf.sprintf "Error: %s" (x |> Jsonrpc.to_string)
    | Task.Pending _ ->
        Printf.sprintf "Error: still pending"
  in
  let rows =
    List.map
      (fun (name, state) -> [name; string_of_state state])
      t.Task.subtasks
  in
  let rows = rows @ List.map (fun (k, v) -> [k; v]) t.Task.debug_info in
  Table.print rows ;
  Printf.printf "\n" ;
  Printf.printf "Overall: %s\n" (string_of_state t.Task.state)

let old_main () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let verbose = List.mem "-v" args in
  let args = List.filter (fun x -> x <> "-v") args in
  (* Extract any -path X argument *)
  let extract args key =
    let result = ref None in
    let args =
      List.fold_left
        (fun (acc, foundit) x ->
          if foundit then (
            result := Some x ;
            (acc, false)
          ) else if x = key then
            (acc, true)
          else
            (x :: acc, false)
        )
        ([], false) args
      |> fst
      |> List.rev
    in
    (!result, args)
  in
  let path, args = extract args "-path" in
  ( match path with
  | Some path ->
      Xenops_interface.set_sockets_dir path
  | None ->
      ()
  ) ;
  let task = success_task (if verbose then verbose_task else ignore_task) in
  match args with
  | ["help"] | [] ->
      usage () ; exit 0
  | ["migrate"; id; url] ->
      migrate ~id ~url ~compress:"false" ~verify_dest:"false" |> task
  | ["migrate"; id; url; compress] ->
      migrate ~id ~url ~compress ~verify_dest:"false" |> task
  | ["migrate"; id; url; compress; verify_dest] ->
      migrate ~id ~url ~compress ~verify_dest |> task
  | ["vbd-list"; id] ->
      vbd_list id
  | ["pci-add"; id; idx; bdf] ->
      pci_add id idx bdf
  | ["pci-remove"; id; idx] ->
      pci_remove id idx
  | ["pci-list"; id] ->
      pci_list id
  | ["cd-insert"; id; disk] ->
      cd_insert id disk |> task
  | ["delay"; id; t] ->
      delay id (float_of_string t)
  | ["events-watch"] ->
      events_watch None
  | ["set-worker-pool-size"; size] ->
      set_worker_pool_size (int_of_string size)
  | ["shutdown"] ->
      debug_shutdown ()
  | cmd :: _ ->
      Printf.fprintf stderr "Unrecognised command: %s\n" cmd ;
      usage () ;
      exit 1
