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

open Client
open Printf

module D=Debug.Make(struct let name="sm_fs_ops" end)
open D

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
       Stdext.Pervasiveext.finally
         (fun () -> f fd)
         (fun () -> Unix.close fd)
    )

(** Return a URL suitable for passing to the sparse_dd process *)
let import_vdi_url ~__context ?(prefer_slaves=false) rpc session_id task_id vdi =
  (* Find a suitable host for the SR containing the VDI *)
  let sr = Db.VDI.get_SR ~__context ~self:vdi in
  let host = Importexport.find_host_for_sr ~__context ~prefer_slaves sr in
  let address = Db.Host.get_address ~__context ~self:host in
  Printf.sprintf "https://%s%s?vdi=%s&session_id=%s&task_id=%s"
    address Constants.import_raw_vdi_uri (Ref.string_of vdi)
    (Ref.string_of session_id) (Ref.string_of task_id)

(* SCTX-286: thin provisioning is thrown away over VDI.copy, VM.import(VM.export).
   Return true if the newly created vdi must have zeroes written into it; default to false
   under the assumption that "proper" storage devices (ie not our legacy LVM stuff) always
   create disks full of virtual zeroes, if for no other reason other than it being a
   privacy violation to return a VDI containing someone else's old data.

   This knowledge clearly ought to be in the SM backend rather than here. *)
let must_write_zeroes_into_new_vdi ~__context vdi =
  let vdi_r = Db.VDI.get_record ~__context ~self:vdi in
  let sr_r = Db.SR.get_record ~__context ~self:vdi_r.API.vDI_SR in
  let potentially_using_lvhd sr_r = List.mem (String.lowercase_ascii sr_r.API.sR_type) [ "lvm"; "lvmoiscsi"; "lvmohba" ] in
  let requested_raw_vdi vdi_r = List.mem (List.hd Xha_statefile.statefile_sm_config) vdi_r.API.vDI_sm_config in
  let upgraded_to_lvhd sr_r = List.mem ("use_vhd", "true") sr_r.API.sR_sm_config in

  (* Equallogic arrays in 'thick' mode don't zero disks *)
  let using_eql sr_r = String.lowercase_ascii sr_r.API.sR_type =  "equal" in
  let using_eql_thick sr_r = List.mem ("allocation", "thick") (List.map (fun (x, y) -> String.lowercase_ascii x, String.lowercase_ascii y) sr_r.API.sR_sm_config) in

  (* We presume that storagelink arrays don't zero disks either *)
  let using_csl sr_r = String.lowercase_ascii sr_r.API.sR_type = "cslg" in

  (* Julian agreed with the following logic by email + chat: *)
  false
  || (potentially_using_lvhd sr_r
      && ((requested_raw_vdi vdi_r) || (not (upgraded_to_lvhd sr_r)))
     )
  (* After speaking to Julian again: *)
  || (using_eql sr_r && (using_eql_thick sr_r))
  || (using_csl sr_r)


let copy_vdi ~__context ?base vdi_src vdi_dst =
  TaskHelper.set_cancellable ~__context;
  Helpers.call_api_functions ~__context (fun rpc session_id ->

      (* Use the sparse copy unless we must write zeroes into the new VDI *)
      let sparse = not (must_write_zeroes_into_new_vdi ~__context vdi_dst) in
      if not sparse && (base <> None) then begin
        (* This doesn't make sense because we will be forced to write zeroes
           			   into the destination VDI, and then the user will not be able to tell
           			   the difference between zeroes that exist in the user's virtual disk,
           			   and zeroes that we had to write to clear the disk. *)
        error "VDI.copy: destination VDI does not support sparse copy BUT a delta-only copy requested. The output will be useless so I refuse to make it.";
        raise (Api_errors.Server_error(Api_errors.vdi_not_sparse, [ Ref.string_of vdi_dst ]))
      end;

      (* Copy locally unless this host can't see the destination SR *)
      let can_local_copy = Importexport.check_sr_availability ~__context (Db.VDI.get_SR ~__context ~self:vdi_dst) in

      let size = Db.VDI.get_virtual_size ~__context ~self:vdi_src in

      let local_copy = can_local_copy && not (Xapi_fist.force_remote_vdi_copy ()) in

      debug "Sm_fs_ops.copy_vdi: %s-copying %Ld%s preserving sparseness %s"
        (if local_copy then "locally" else "remotely")
        size
        (if sparse then "" else " NOT")
        (match base with None -> "" | Some x -> Printf.sprintf "copying only differences from %s" (Ref.string_of x));
      let progress_cb progress =
        TaskHelper.exn_if_cancelling ~__context;
        TaskHelper.operate_on_db_task ~__context
          (fun self -> Db.Task.set_progress ~__context ~self ~value:progress) in
      let copy base =
        try
          with_block_attached_device __context rpc session_id vdi_src `RO
            (fun device_src ->
               if local_copy
               then with_block_attached_device __context rpc session_id vdi_dst `RW
                   (fun device_dst ->
                      Sparse_dd_wrapper.dd ~progress_cb ?base sparse device_src device_dst size
                   )
               else
                 (* Create a new subtask for the inter-host sparse_dd. Without
                    							 * this there was a race in VM.copy, as both VDI.copy and VM.copy
                    							 * would be waiting on the same VDI.copy task.
                    							 *
                    							 * Now, VDI.copy waits for the sparse_dd task, and VM.copy in turn
                    							 * waits for the VDI.copy task.
                    							 *
                    							 * Note that progress updates are still applied directly to the
                    							 * VDI.copy task. *)
                 Server_helpers.exec_with_subtask ~__context ~task_in_database:true "sparse_dd"
                   (fun ~__context ->
                      let import_task_id = Context.get_task_id __context in
                      let remote_uri =
                        import_vdi_url ~__context ~prefer_slaves:true
                          rpc session_id import_task_id vdi_dst
                      in
                      debug "remote_uri = %s" remote_uri;
                      try
                        Sparse_dd_wrapper.dd ~progress_cb ?base sparse device_src remote_uri size;
                        Tasks.wait_for_all ~rpc ~session_id ~tasks:[import_task_id];
                        match Db.Task.get_status ~__context ~self:import_task_id with
                        | `success -> ()
                        | _ ->
                          begin match Db.Task.get_error_info ~__context ~self:import_task_id with
                            | [] -> (* This should never happen *)
                              failwith("Copy of VDI to remote failed with unspecified error!")
                            | code :: params ->
                              debug "Copy of VDI to remote failed: %s [ %s ]" code (String.concat "; " params);
                              raise (Api_errors.Server_error (code, params))
                          end
                      with e ->
                        Tasks.wait_for_all ~rpc ~session_id ~tasks:[import_task_id];
                        raise e
                   )
            )
        with
        | Unix.Unix_error(Unix.EIO, _, _) as e ->
          let e' = Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O error"]) in
          Backtrace.reraise e e'
        | e ->
          Backtrace.is_important e;
          raise e in
      match base with
      | None -> copy None
      | Some base_vdi ->
        with_block_attached_device __context rpc session_id base_vdi `RO
          (fun device_base -> copy (Some device_base))
    )
