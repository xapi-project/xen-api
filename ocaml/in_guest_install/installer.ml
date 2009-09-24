(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
let install_mountpoint = "/tmp/install"
let mkfs_ext2 = "mkfs.ext2"
let mkfs_ext3 = "mkfs.ext3"
let mount = "mount"

let debug_enabled = ref true

let error fmt = Printf.fprintf stderr fmt
let debug fmt = Printf.kprintf (fun s -> if !debug_enabled then output_string stderr s) fmt

open Client

type fs = Ext2 | Ext3 | Swap
type disk = { device: string; mountpoint: string; fs: fs; size: int64; sr: API.ref_SR }

let fs_of_string = function
  | "ext2" -> Ext2
  | "ext3" -> Ext3
  | "swap" -> Swap
  | x -> failwith (Printf.sprintf "Unsupported filesystem type: %s" x)

let runcmd x = match Unix.system x with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> failwith (Printf.sprintf "Error %d running %s" n x)

(** Return a list of disks extracted from the VM.other_config *)
let requested_disks rpc session_id vm = 
  let other = Client.VM.get_other_config rpc session_id vm in
  let make_disk i = 
    let sr = List.assoc (Printf.sprintf "disk-%d-sr" i) other in
    let sr = List.hd (Client.SR.get_by_name_label rpc session_id sr) in
    { device = List.assoc (Printf.sprintf "disk-%d-device" i) other;
      mountpoint = List.assoc (Printf.sprintf "disk-%d-mountpoint" i) other;
      fs = fs_of_string (List.assoc (Printf.sprintf "disk-%d-fs" i) other);
      size = Int64.of_string (List.assoc (Printf.sprintf "disk-%d-size" i) other);
      sr = sr;
    } in
  let rec loop i = 
    try 
      let this = make_disk i in
      this :: (loop (i + 1))
    with _ -> [] in
  loop 0

(* Map of disk -> VBD  *)
let vbd_table = Hashtbl.create 10

(** Create a new VDI for a disk and attach it to the VM via a VBD. *)
let make_disk rpc session_id vm disk = 
  let vdi = Client.VDI.create ~rpc ~session_id
    ~name_label:disk.device ~name_description:"Created by guest installer"
    ~sR:disk.sr ~virtual_size:disk.size
    ~_type:`system ~sharable:false ~read_only:false ~other_config:[] in
  let vbd = Client.VBD.create ~rpc ~session_id
    ~vM:vm ~vDI:vdi ~userdevice:disk.device ~bootable:false ~mode:`RW ~_type:`Disk
    ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
  Hashtbl.add vbd_table disk vbd

(** Initialise the filesystem on a disk *)
let mkfs disk = 
  let dev = " /dev/" ^ disk.device in match disk.fs with
  | Ext2 -> runcmd (mkfs_ext2 ^ dev)
  | Ext3 -> runcmd (mkfs_ext3 ^ dev)
  | Swap -> ()
  
(** Mount a disk on a mountpoint under 'install_mountpoint' *)
let mount disk = match disk.fs with
  | Ext2 | Ext3 ->
      let mnt = install_mountpoint ^ "/" ^ disk.mountpoint in
      Unixext.mkdir_rec mnt 0o755;
      runcmd (mount ^ " /dev/" ^ disk.device ^ " " ^ mnt)
  | Swap -> ()

(** Hotplug all VBDs, initialise filesystems on them, mount them under install_mountpoint.
    Next step is to mount the install media and run the install script. *)
let make_install_tree rpc session_id vm disks = 
  List.iter (fun disk ->
	       let vbd = Hashtbl.find vbd_table disk in
	       Client.VBD.plug rpc session_id vbd;
	       mkfs disk;
	       mount disk) disks

let _ =
  debug "In-guest installer\n";
  let xs = Xs.domain_open () in
  let domid = int_of_string (xs.Xs.read "domid") in
  debug "Domain id: %d\n" domid;

  let host, port, session_id, vm = Domain.guest_get_api_access ~xs in
  let vm = Ref.of_string vm and session_id = Ref.of_string session_id in

  let rpc xml = Xmlrpcclient.do_xml_rpc ~version:"1.0" ~host ~port ~path:"/" xml in

  List.iter (fun vm ->
	       debug "Found VM: %s\n" (Client.VM.get_name_label rpc session_id vm)
	    ) (Client.VM.get_all rpc session_id);

  let disks = requested_disks rpc session_id vm in
  List.iter (make_disk rpc session_id vm) disks;
  make_install_tree rpc session_id vm disks;
  debug "Time to find the install media"


