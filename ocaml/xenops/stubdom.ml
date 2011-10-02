(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

module D = Debug.Debugger(struct let name = "xenops" end)
open D

let fs_backend_path = "/usr/sbin/fs-backend"

let create ~xc ~xs (info: Device.Dm.info) domid =
    let stubdom_name = Printf.sprintf "stubdom:%d" domid in
	let stubdom_uuid = Uuid.make_uuid() in
    debug "jjd27: creating stubdom with name '%s' and uuid '%s'" stubdom_name (Uuid.to_string stubdom_uuid);
	let info = {
 		Domain.ssidref = 0l;
 		Domain.hvm = false;
 		Domain.hap = false;
 		Domain.name = stubdom_name;
 		Domain.platformdata = [];
 		Domain.xsdata = [];
		Domain.bios_strings = [];
	} in
    let stubdom_domid = Domain.make ~xc ~xs info stubdom_uuid in
    debug "jjd27: created stubdom with domid %d" stubdom_domid;

    Domain.set_machine_address_size ~xc stubdom_domid (Some 32);

    (* Now build it as a PV domain *)
    let mib32 = Int64.mul 32L 1024L in
    let (_: Domain.domarch) = Domain.build ~xc ~xs {
        Domain.memory_max=mib32;
        Domain.memory_target=mib32;
        Domain.kernel="/usr/lib/xen/boot/ioemu-stubdom.gz";
        Domain.vcpus=1;
        Domain.priv=Domain.BuildPV {Domain.cmdline=""; Domain.ramdisk=None};
    } stubdom_domid in

    (* Point the stub domain at the guest *)
    debug "jjd27: pointing stubdom %d to guest %d" stubdom_domid domid;
	(* XXX: this binding is missing
    Xc.domain_set_target xc stubdom_domid domid; *)

    (* Tell XenStore that the stubdom should have implicit privileges over the target domain *)
    debug "jjd27: telling XenStore that stubdom %d has target %d" stubdom_domid domid;
	(* XXX: this command is missing
    xs.Xs.set_target stubdom_domid domid; *)

    (* Write the guest's domid into XenStore *)
    let path = xs.Xs.getdomainpath stubdom_domid in
    xs.Xs.write (Printf.sprintf "%s/target" path) (string_of_int domid);
    debug "jjd27: writing guest domid %d into xenstore at %s/target" domid path;

    let vm = xs.Xs.read (Printf.sprintf "/local/domain/%d/vm" domid) in
    debug "jjd27: target vm's path is %s\n" vm;

    (* Write the qemu-dm command-line arguments into XenStore *)
	let guest_uuid_str = Uuid.to_string (Domain.get_uuid xc domid) in
    let path = Printf.sprintf "/vm/%s/image/dmargs" guest_uuid_str in
	(* XXX *)
	let args = [] (* Device.Dm.cmdline info *) in
    let args = List.tl (List.tl args) in
    let args = List.filter (fun x -> x <> "-serial" && x <> "pty") args in
    xs.Xs.write path (String.concat " " args);
    debug "jjd27: written qemu-dm args into xenstore at %s: [%s]" path (String.concat " " args);

    (* Make that XenStore path readable by the stub domain *)
    xs.Xs.setperms path (0, Xsraw.PERM_NONE, [ (domid, Xsraw.PERM_READ); (stubdom_domid, Xsraw.PERM_READ) ]);
    debug "jjd27: set the permissions on %s" path;

	let syslog_stdout = Forkhelpers.Syslog_WithKey (Printf.sprintf "fs-backend-%d" domid) in
	let pid = Forkhelpers.safe_close_and_exec None None None [] ~syslog_stdout fs_backend_path [] in
	Forkhelpers.dontwaitpid pid;

    (* Set the FS backend in XenStore for the stubdom to have access to the domain filesystem *)
    Device.Vfs.add ~xc ~xs stubdom_domid;

    (* Add a vfb device to the stubdom and domain *)
    List.iter (fun domid -> ignore (Device.Vfb.add ~xc ~xs domid)) [stubdom_domid; domid];

    (* Add a vkbd device to the stubdom and the guest *)
    List.iter (fun domid -> ignore (Device.Vkbd.add ~xc ~xs domid)) [stubdom_domid; domid];

	(* XXX: 
    (* Add a place for qemu to record the dm state in XenStore, with appropriate permissions *)
    List.iter (fun domid -> Device.Dm.init ~xs ~domid) [stubdom_domid; domid];
	*)
    stubdom_domid
