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
(* Make VDIs with ext2 filesystems on them *)
open Vmopshelpers
open Pervasiveext
open Client
open Printf

module D=Debug.Debugger(struct let name="xapi" end)
open D

let make_tmp_dir() =
	let tmp_file = Filename.temp_file "xapi_mount_" "" in
	Unix.unlink tmp_file;
	Unix.mkdir tmp_file 0o640;
	tmp_file

(** Block-attach a VDI to dom0 and run 'f' with the device name *)
let with_block_attached_device __context rpc session_id vdi mode f = 
  let dom0 = Helpers.get_domain_zero ~__context in
  Attach_helpers.with_vbds rpc session_id __context dom0 [ vdi ] mode
    (fun vbds ->
       let vbd = List.hd vbds in
       f ("/dev/" ^ (Db.VBD.get_device ~__context ~self:vbd)))

(** Block-attach a VDI to dom0, open the device and pass the file descriptor to [f] *)
let with_open_block_attached_device __context rpc session_id vdi mode f = 
	with_block_attached_device __context rpc session_id vdi mode
	(fun path ->
		let mode' = match mode with
		| `RO -> [ Unix.O_RDONLY ]
		| `RW -> [ Unix.O_RDWR ] in
		let fd = Unix.openfile path mode' 0 in
		finally
		(fun () -> f fd)
		(fun () -> Unix.close fd)
	)

(** Execute a function with a list of device paths after attaching a bunch of VDIs to dom0 *)
let with_block_attached_devices (__context: Context.t) rpc (session_id: API.ref_session) (vdis: API.ref_VDI list) mode f = 
  let rec loop acc = function
    | [] -> f (List.rev acc)
    | vdi :: vdis -> with_block_attached_device __context rpc session_id vdi mode (fun path -> loop (path :: acc) vdis) in
  loop [] vdis

(** Return a URL suitable for passing to the sparse_dd process *)
let import_vdi_url ~__context ?(prefer_slaves=false) rpc session_id vdi = 
        (* Find a suitable host for the SR containing the VDI *)
        let sr = Db.VDI.get_SR ~__context ~self:vdi in
        let host = Importexport.find_host_for_sr ~__context ~prefer_slaves sr in
        let address = Db.Host.get_address ~__context ~self:host in
        Printf.sprintf "https://%s%s?vdi=%s&session_id=%s" address Constants.import_raw_vdi_uri (Ref.string_of vdi) (Ref.string_of session_id)

(** Catch those smint exceptions and convert into meaningful internal errors *)
let with_api_errors f x = 
  try f x
  with
  | Smint.Command_failed(ret, status, stdout_log, stderr_log)
  | Smint.Command_killed(ret, status, stdout_log, stderr_log) ->
      let msg = Printf.sprintf "Smint.Command_{failed,killed} ret = %d; status = %s; stdout = %s; stderr = %s"
	ret status stdout_log stderr_log in
      raise (Api_errors.Server_error (Api_errors.internal_error, [msg]))

(** Mount a filesystem somewhere, with optional type *)
let mount ?ty:(ty = None) src dest =
  let ty = match ty with None -> [] | Some ty -> [ "-t"; ty ] in
  ignore(Forkhelpers.execute_command_get_output "/bin/mount" (ty @ [ src; dest ]))
    
let timeout = 300. (* 5 minutes: something is seriously wrong if we hit this timeout *)
exception Umount_timeout
  
(** Unmount a mountpoint. Retries every 5 secs for a total of 5mins before returning failure *)
let umount ?(retry=true) dest =
  let finished = ref false in
  let start = Unix.gettimeofday () in
  
  while not(!finished) && (Unix.gettimeofday () -. start < timeout) do
    try
      ignore(Forkhelpers.execute_command_get_output "/bin/umount" [dest] ); 
      finished := true
    with e ->
      if not(retry) then raise e;
      debug "Caught exception (%s) while unmounting %s: pausing before retrying"
	(ExnHelper.string_of_exn e) dest;
      Thread.delay 5.
  done;
  if not(!finished) then raise Umount_timeout

let with_mounted_dir device mount_point rmdir f =
  finally 
    (fun () -> 
      debug "About to create mount point (perhaps)";
      let output, _ = Forkhelpers.execute_command_get_output "/bin/mkdir" ["-p"; mount_point] in
      debug "Mountpoint created (output=%s)" output;
      with_api_errors (mount ~ty:(Some "ext2") device) mount_point;
      debug "Mounted";
      f mount_point)
    (fun () ->
      Helpers.log_exn_continue ("with_fs_vdi: unmounting " ^ mount_point) 
	(fun () -> umount mount_point) ();
      Helpers.log_exn_continue ("with_fs_vdi: rmdir " ^ mount_point)
	(fun () -> if rmdir then Unix.rmdir mount_point) ())  
    
(** Block-attach a VDI to dom0, mount an ext2 filesystem and run 'f' with the mountpoint *)
let with_fs_vdi __context vdi f = 
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
     with_block_attached_device __context rpc session_id vdi `RW
       (fun device ->
	  let mount_point = make_tmp_dir () in
	  with_mounted_dir device mount_point true f
	 )
    )

(** Stick ext2 filesystem on VDI and turn off maximal mount count + checking interval *)
let mke2fs device =
  ignore(Forkhelpers.execute_command_get_output "/sbin/mkfs" ["-t"; "ext2"; device]);
  ignore(Forkhelpers.execute_command_get_output "/sbin/tune2fs"  ["-i"; "0"; "-c"; "0"; device])

(** Create a new VDI, block attach it to dom0, create an ext2 filesystem,
    run 'f' with the vdi_ref and the mountpoint. Leave the VDI around, unless there is
    an exception in which case we delete it. *)
let with_new_fs_vdi __context ~name_label ~name_description ~sR ~required_space ~_type ~sm_config f =
  	let add_fs_overhead req =
		let fs_overhead_factor = 1.05 (* allow 5% overhead for ext2 *) in
		(Int64.of_float ((Int64.to_float req)*.fs_overhead_factor))
		in
	Helpers.call_api_functions ~__context
	  (fun rpc session_id ->
	    let vdi_ref = Client.VDI.create ~rpc ~session_id
	      ~name_label ~name_description ~sR ~virtual_size:(add_fs_overhead required_space)
	      ~sharable:false ~read_only:false ~_type ~other_config:[] ~xenstore_data:[] ~sm_config ~tags:[] in	      
	    try
	      with_block_attached_device __context rpc session_id vdi_ref `RW
		(fun device ->
		  with_api_errors
		      (fun () ->
			mke2fs device;
			(* Mount it *)
			let mount_point = make_tmp_dir() in
			with_mounted_dir device mount_point true (f vdi_ref)
		      ) ()
		)
	    with e ->
	      debug "Caught error (%s) during with_new_fs_vdi: deleting created VDI" (ExnHelper.string_of_exn e);
	      Client.VDI.destroy ~rpc ~session_id ~self:vdi_ref;
	      raise e
	  )
	  
(* SCTX-286: thin provisioning is thrown away over VDI.copy, VM.import(VM.export).
   Return true if the newly created vdi must have zeroes written into it; default to false
   under the assumption that "proper" storage devices (ie not our legacy LVM stuff) always 
   create disks full of virtual zeroes, if for no other reason other than it being a
   privacy violation to return a VDI containing someone else's old data.

   This knowledge clearly ought to be in the SM backend rather than here. *)
let must_write_zeroes_into_new_vdi ~__context vdi =
  let vdi_r = Db.VDI.get_record ~__context ~self:vdi in
  let sr_r = Db.SR.get_record ~__context ~self:vdi_r.API.vDI_SR in
  let potentially_using_lvhd sr_r = List.mem (String.lowercase sr_r.API.sR_type) [ "lvm"; "lvmoiscsi"; "lvmohba" ] in
  let requested_raw_vdi vdi_r = List.mem (List.hd Xha_statefile.statefile_sm_config) vdi_r.API.vDI_sm_config in
  let upgraded_to_lvhd sr_r = List.mem ("use_vhd", "true") sr_r.API.sR_sm_config in

  (* Equallogic arrays in 'thick' mode don't zero disks *)
  let using_eql sr_r = String.lowercase sr_r.API.sR_type =  "equal" in
  let using_eql_thick sr_r = List.mem ("allocation", "thick") (List.map (fun (x, y) -> String.lowercase x, String.lowercase y) sr_r.API.sR_sm_config) in

  (* We presume that storagelink arrays don't zero disks either *)
  let using_csl sr_r = String.lowercase sr_r.API.sR_type = "cslg" in

  (* Julian agreed with the following logic by email + chat: *)
  false
  || (potentially_using_lvhd sr_r
	  && ((requested_raw_vdi vdi_r) || (not (upgraded_to_lvhd sr_r)))
	 )
  (* After speaking to Julian again: *)
  || (using_eql sr_r && (using_eql_thick sr_r))
  || (using_csl sr_r)


let copy_vdi ~__context vdi_src vdi_dst = 
  TaskHelper.set_cancellable ~__context;
  Helpers.call_api_functions ~__context (fun rpc session_id ->

  (* Use the sparse copy unless we must write zeroes into the new VDI *)
  let sparse = not (must_write_zeroes_into_new_vdi ~__context vdi_dst) in

  (* Copy locally unless this host can't see the destination SR *)
  let can_local_copy = Importexport.check_sr_availability ~__context (Db.VDI.get_SR ~__context ~self:vdi_dst) in

  let size = Db.VDI.get_virtual_size ~__context ~self:vdi_src in

  let local_copy = can_local_copy && not (Xapi_fist.force_remote_vdi_copy ()) in

  debug "Sm_fs_ops.copy_vdi: %s-copying %Ld%s preserving sparseness" (if local_copy then "locally" else "remotely") size (if sparse then "" else " NOT");

try
	with_block_attached_device __context rpc session_id vdi_src `RO
	(fun device_src ->
       		if local_copy
		then with_block_attached_device __context rpc session_id vdi_dst `RW
		(fun device_dst ->
			Sparse_dd_wrapper.dd ~__context sparse device_src device_dst size
		)
		else begin
			let remote_uri = import_vdi_url ~__context ~prefer_slaves:true rpc session_id vdi_dst in
			debug "remote_uri = %s" remote_uri;
			Sparse_dd_wrapper.dd ~__context sparse device_src remote_uri size
		end
	)
with
| Unix.Unix_error(Unix.EIO, _, _) ->
	raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O error"]))
| e ->
	debug "Caught exception %s" (ExnHelper.string_of_exn e);
	log_backtrace ();
	raise e
)
