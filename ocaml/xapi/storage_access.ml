(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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

open Stdext
open Listext
open Threadext
open Xstringext
module XenAPI = Client.Client
open Fun
open Storage_interface

module D=Debug.Make(struct let name="storage_access" end)
open D

let transform_storage_exn f =
  try
    f ()
  with
  | Backend_error(code, params) as e ->
    Backtrace.reraise e (Api_errors.Server_error(code, params))
  | Backend_error_with_backtrace(code, backtrace :: params) as e ->
    let backtrace = Backtrace.Interop.of_json "SM" backtrace in
    Backtrace.add e backtrace;
    Backtrace.reraise e (Api_errors.Server_error(code, params))
  | Api_errors.Server_error(code, params) as e -> raise e
  | No_storage_plugin_for_sr sr as e ->
    Server_helpers.exec_with_new_task "transform_storage_exn"
      (fun __context ->
         let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
         Backtrace.reraise e (Api_errors.Server_error(Api_errors.sr_not_attached, [ Ref.string_of sr ]))
      )
  | e ->
    Backtrace.reraise e (Api_errors.Server_error(Api_errors.internal_error, [ Printexc.to_string e ]))

exception No_VDI

(* Find a VDI given a storage-layer SR and VDI *)
let find_vdi ~__context sr vdi =
  let open Db_filter_types in
  let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
  match Db.VDI.get_records_where ~__context ~expr:(And((Eq (Field "location", Literal vdi)),Eq (Field "SR", Literal (Ref.string_of sr)))) with
  | x :: _ -> x
  | _ -> raise No_VDI

(* Find a VDI reference given a name *)
let find_content ~__context ?sr name =
  (* PR-1255: the backend should do this for us *)
  let open Db_filter_types in
  let expr = Opt.default True (Opt.map (fun sr -> Eq(Field "SR", Literal (Ref.string_of (Db.SR.get_by_uuid ~__context ~uuid:sr)))) sr) in
  let all = Db.VDI.get_records_where ~__context ~expr in
  List.find
    (fun (_, vdi_rec) ->
       false
       || (vdi_rec.API.vDI_location = name) (* PR-1255 *)
    ) all

let redirect sr =
  raise (Redirect (Some (Pool_role.get_master_address ())))

module SMAPIv1 = struct
  (** xapi's builtin ability to call local SM plugins using the existing
      	    protocol. The code here should only call the SM functions and encapsulate
      	    the return or error properly. It should not perform side-effects on
      	    the xapi database: these should be handled in the layer above so they
      	    can be shared with other SM implementation types.

      	    Where this layer has to perform interface adjustments (see VDI.activate
      	    and the read/write debacle), this highlights desirable improvements to
      	    the backend interface.
      	*)

  type context = Smint.request

  let vdi_info_of_vdi_rec __context vdi_rec =
    let content_id =
      if List.mem_assoc "content_id" vdi_rec.API.vDI_other_config
      then List.assoc "content_id" vdi_rec.API.vDI_other_config
      else vdi_rec.API.vDI_location (* PR-1255 *)
    in {
      vdi = vdi_rec.API.vDI_location;
      uuid = Some vdi_rec.API.vDI_uuid;
      content_id = content_id; (* PR-1255 *)
      name_label = vdi_rec.API.vDI_name_label;
      name_description = vdi_rec.API.vDI_name_description;
      ty = Storage_utils.string_of_vdi_type vdi_rec.API.vDI_type;
      metadata_of_pool = Ref.string_of vdi_rec.API.vDI_metadata_of_pool;
      is_a_snapshot = vdi_rec.API.vDI_is_a_snapshot;
      snapshot_time = Date.to_string vdi_rec.API.vDI_snapshot_time;
      snapshot_of =
        if Db.is_valid_ref __context vdi_rec.API.vDI_snapshot_of
        then Db.VDI.get_uuid ~__context ~self:vdi_rec.API.vDI_snapshot_of
        else "";
      read_only = vdi_rec.API.vDI_read_only;
      cbt_enabled = vdi_rec.API.vDI_cbt_enabled;
      virtual_size = vdi_rec.API.vDI_virtual_size;
      physical_utilisation = vdi_rec.API.vDI_physical_utilisation;
      persistent = vdi_rec.API.vDI_on_boot = `persist;
      sharable = vdi_rec.API.vDI_sharable;
      sm_config = vdi_rec.API.vDI_sm_config;
    }

  let vdi_info_from_db ~__context self =
    let vdi_rec = Db.VDI.get_record ~__context ~self in
    vdi_info_of_vdi_rec __context vdi_rec

  (* For SMAPIv1, is_a_snapshot, snapshot_time and snapshot_of are stored in
     	 * xapi's database. For SMAPIv2 they should be implemented by the storage
     	 * backend. *)
  let set_is_a_snapshot context ~dbg ~sr ~vdi ~is_a_snapshot =
    Server_helpers.exec_with_new_task "VDI.set_is_a_snapshot"
      ~subtask_of:(Ref.of_string dbg)
      (fun __context ->
         let vdi, _ = find_vdi ~__context sr vdi in
         Db.VDI.set_is_a_snapshot ~__context ~self:vdi ~value:is_a_snapshot
      )

  let set_snapshot_time context ~dbg ~sr ~vdi ~snapshot_time =
    Server_helpers.exec_with_new_task "VDI.set_snapshot_time"
      ~subtask_of:(Ref.of_string dbg)
      (fun __context ->
         let vdi, _ = find_vdi ~__context sr vdi in
         let snapshot_time = Date.of_string snapshot_time in
         Db.VDI.set_snapshot_time ~__context ~self:vdi ~value:snapshot_time
      )

  let set_snapshot_of context ~dbg ~sr ~vdi ~snapshot_of =
    Server_helpers.exec_with_new_task "VDI.set_snapshot_of"
      ~subtask_of:(Ref.of_string dbg)
      (fun __context ->
         let vdi, _ = find_vdi ~__context sr vdi in
         let snapshot_of, _ = find_vdi ~__context sr snapshot_of in
         Db.VDI.set_snapshot_of ~__context ~self:vdi ~value:snapshot_of
      )

  module Query = struct
    let query context ~dbg = {
      driver = "storage_access";
      name = "SMAPIv1 adapter";
      description = "Allows legacy SMAPIv1 adapters to expose an SMAPIv2 interface";
      vendor = "XCP";
      copyright = "see the source code";
      version = "2.0";
      required_api_version = "2.0";
      features = [];
      configuration = [];
      required_cluster_stack = [];
    }

    let diagnostics context ~dbg =
      "No diagnostics are available for SMAPIv1 plugins"
  end

  module DP = struct
    let create context ~dbg ~id = assert false
    let destroy context ~dbg ~dp = assert false
    let diagnostics context () = assert false
    let attach_info context ~dbg ~sr ~vdi ~dp = assert false
    let stat_vdi context ~dbg ~sr ~vdi = assert false
  end

  module SR = struct
    include Storage_skeleton.SR

    let probe context ~dbg ~queue ~device_config ~sm_config =
      let _type =
        (* SMAPIv1 plugins have no namespaces, so strip off everything up to
           				   the final dot *)
        try
          let i = String.rindex queue '.' in
          String.sub queue (i + 1) (String.length queue -i - 1)
        with Not_found ->
          queue in
      Server_helpers.exec_with_new_task "SR.probe" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let task = Context.get_task_id __context in
           Storage_interface.Raw (Sm.sr_probe (Some task,(Sm.sm_master true :: device_config)) _type sm_config)
        )

    let create context ~dbg ~sr ~name_label ~name_description ~device_config ~physical_size =
      Server_helpers.exec_with_new_task "SR.create" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let subtask_of = Some (Context.get_task_id __context) in
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
           Db.SR.set_name_label ~__context ~self:sr ~value:name_label;
           Db.SR.set_name_description ~__context ~self:sr ~value:name_description;
           let device_config = (Sm.sm_master true) :: device_config in
           Sm.call_sm_functions ~__context ~sR:sr
             (fun _ _type ->
                try
                  Sm.sr_create (subtask_of, device_config) _type sr physical_size
                with
                | Smint.Not_implemented_in_backend ->
                  error "SR.create failed SR:%s Not_implemented_in_backend" (Ref.string_of sr);
                  raise (Storage_interface.Backend_error(Api_errors.sr_operation_not_supported, [ Ref.string_of sr ]))
                | Api_errors.Server_error(code, params) ->
                  raise (Backend_error(code, params))
                | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.create failed SR:%s error:%s" (Ref.string_of sr) e';
                  raise e
             );
            List.filter (fun (x,_) -> x <> "SRmaster") device_config
        )

    let set_name_label context ~dbg ~sr ~new_name_label =
      Server_helpers.exec_with_new_task "SR.set_name_label" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
           Db.SR.set_name_label ~__context ~self:sr ~value:new_name_label
        )

    let set_name_description context ~dbg ~sr ~new_name_description =
      Server_helpers.exec_with_new_task "SR.set_name_description" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
           Db.SR.set_name_description ~__context ~self:sr ~value:new_name_description
        )

    let attach context ~dbg ~sr ~device_config =
      Server_helpers.exec_with_new_task "SR.attach" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in

           (* Existing backends expect an SRMaster flag to be added
              					   through the device-config. *)
           let srmaster = Helpers.i_am_srmaster ~__context ~sr in
           let device_config = (Sm.sm_master srmaster) :: device_config in
           Sm.call_sm_functions ~__context ~sR:sr
             (fun _ _type ->
                try
                  Sm.sr_attach (Some (Context.get_task_id __context), device_config) _type sr
                with
                | Api_errors.Server_error(code, params) ->
                  raise (Backend_error(code, params))
                | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.attach failed SR:%s error:%s" (Ref.string_of sr) e';
                  raise e
             )
        )
    let detach context ~dbg ~sr =
      Server_helpers.exec_with_new_task "SR.detach" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in

           Sm.call_sm_functions ~__context ~sR:sr
             (fun device_config _type ->
                try
                  Sm.sr_detach device_config _type sr
                with
                | Api_errors.Server_error(code, params) ->
                  raise (Backend_error(code, params))
                | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.detach failed SR:%s error:%s" (Ref.string_of sr) e';
                  raise e
             )
        )

    let reset context ~dbg ~sr = assert false

    let destroy context ~dbg ~sr =
      Server_helpers.exec_with_new_task "SR.destroy" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in

           Sm.call_sm_functions ~__context ~sR:sr
             (fun device_config _type ->
                try
                  Sm.sr_delete device_config _type sr
                with
                | Smint.Not_implemented_in_backend ->
                  raise (Storage_interface.Backend_error(Api_errors.sr_operation_not_supported, [ Ref.string_of sr ]))
                | Api_errors.Server_error(code, params) ->
                  raise (Backend_error(code, params))
                | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.detach failed SR:%s error:%s" (Ref.string_of sr) e';
                  raise e
             )
        )

    let stat context ~dbg ~sr:sr' =
      Server_helpers.exec_with_new_task "SR.stat" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr' in
           Sm.call_sm_functions ~__context ~sR:sr
             (fun device_config _type ->
                try
                  Sm.sr_update device_config _type sr;
                  let r = Db.SR.get_record ~__context ~self:sr in
                  let name_label = r.API.sR_name_label in
                  let name_description = r.API.sR_name_description in
                  let total_space = r.API.sR_physical_size in
                  let free_space = Int64.sub r.API.sR_physical_size r.API.sR_physical_utilisation in
                  let clustered = false in
                  let health = Storage_interface.Healthy in
                  { name_label; name_description; total_space; free_space; clustered; health }
                with
                | Smint.Not_implemented_in_backend ->
                  raise (Storage_interface.Backend_error(Api_errors.sr_operation_not_supported, [ Ref.string_of sr ]))
                | Api_errors.Server_error(code, params) ->
                  error "SR.scan failed SR:%s code=%s params=[%s]" (Ref.string_of sr) code (String.concat "; " params);
                  raise (Backend_error(code, params))
                | Sm.MasterOnly -> redirect sr
                | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.scan failed SR:%s error:%s" (Ref.string_of sr) e';
                  raise e
             )
        )

    let scan context ~dbg ~sr:sr' =
      Server_helpers.exec_with_new_task "SR.scan" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let sr = Db.SR.get_by_uuid ~__context ~uuid:sr' in
           Sm.call_sm_functions ~__context ~sR:sr
             (fun device_config _type ->
                try
                  Sm.sr_scan device_config _type sr;
                  let open Db_filter_types in
                  let vdis = Db.VDI.get_records_where ~__context ~expr:(Eq(Field "SR", Literal (Ref.string_of sr))) |> List.map snd in
                  List.map (vdi_info_of_vdi_rec __context) vdis
                with
                | Smint.Not_implemented_in_backend ->
                  raise (Storage_interface.Backend_error(Api_errors.sr_operation_not_supported, [ Ref.string_of sr ]))
                | Api_errors.Server_error(code, params) ->
                  error "SR.scan failed SR:%s code=%s params=[%s]" (Ref.string_of sr) code (String.concat "; " params);
                  raise (Backend_error(code, params))
                | Sm.MasterOnly -> redirect sr
                | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.scan failed SR:%s error:%s" (Ref.string_of sr) e';
                  raise e
             )
        )


    let list context ~dbg = assert false

    let update_snapshot_info_src context ~dbg ~sr ~vdi
        ~url ~dest ~dest_vdi ~snapshot_pairs =
      assert false

    let update_snapshot_info_dest context ~dbg ~sr ~vdi ~src_vdi ~snapshot_pairs =
      Server_helpers.exec_with_new_task "SR.update_snapshot_info_dest"
        ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let local_vdis = scan __context ~dbg ~sr in
           let find_sm_vdi ~vdi ~vdi_info_list =
             try List.find (fun x -> x.vdi = vdi) vdi_info_list
             with Not_found -> raise (Vdi_does_not_exist vdi)
           in
           let assert_content_ids_match ~vdi_info1 ~vdi_info2 =
             if vdi_info1.content_id <> vdi_info2.content_id
             then raise (Content_ids_do_not_match (vdi_info1.vdi, vdi_info2.vdi))
           in
           (* For each (local snapshot vdi, source snapshot vdi) pair:
              					 * - Check that the content_ids are the same
              					 * - Copy snapshot_time from the source VDI to the local VDI
              					 * - Set the local VDI's snapshot_of to vdi
              					 * - Set is_a_snapshot = true for the local snapshot *)
           List.iter
             (fun (local_snapshot, src_snapshot_info) ->
                let local_snapshot_info =
                  find_sm_vdi ~vdi:local_snapshot ~vdi_info_list:local_vdis in
                assert_content_ids_match local_snapshot_info src_snapshot_info;
                set_snapshot_time __context ~dbg ~sr
                  ~vdi:local_snapshot
                  ~snapshot_time:src_snapshot_info.snapshot_time;
                set_snapshot_of __context ~dbg ~sr
                  ~vdi:local_snapshot ~snapshot_of:vdi;
                set_is_a_snapshot __context ~dbg ~sr
                  ~vdi:local_snapshot ~is_a_snapshot:true;)
             snapshot_pairs)
  end

  module VDI = struct
    let for_vdi ~dbg ~sr ~vdi op_name f =
      Server_helpers.exec_with_new_task op_name ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let open Db_filter_types in
           let self = find_vdi ~__context sr vdi |> fst in
           Sm.call_sm_vdi_functions ~__context ~vdi:self
             (fun device_config _type sr ->
                f device_config _type sr self
             )
        )
    (* Allow us to remember whether a VDI is attached read/only or read/write.
       		   If this is meaningful to the backend then this should be recorded there! *)
    let vdi_read_write = Hashtbl.create 10
    let vdi_read_write_m = Mutex.create ()
    let vdi_read_caching_m = Mutex.create ()
    let per_host_key ~__context ~prefix =
      let host_uuid = Db.Host.get_uuid ~__context ~self:(Helpers.get_localhost ~__context) in
      Printf.sprintf "%s-%s" prefix host_uuid
    let read_caching_key ~__context =
      per_host_key ~__context ~prefix:"read-caching-enabled-on"
    let read_caching_reason_key ~__context =
      per_host_key ~__context ~prefix:"read-caching-reason"


    let epoch_begin context ~dbg ~sr ~vdi ~persistent =
      try
        for_vdi ~dbg ~sr ~vdi "VDI.epoch_begin"
          (fun device_config _type sr self ->
             Sm.vdi_epoch_begin device_config _type sr self)
      with
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))

    let attach context ~dbg ~dp ~sr ~vdi ~read_write =
      try
        let attach_info =
          for_vdi ~dbg ~sr ~vdi "VDI.attach"
            (fun device_config _type sr self ->
               let attach_info_v1 = Sm.vdi_attach device_config _type sr self read_write in
               (* Record whether the VDI is benefiting from read caching *)
               Server_helpers.exec_with_new_task "VDI.attach" ~subtask_of:(Ref.of_string dbg) (fun __context ->
                   let read_caching = not attach_info_v1.Smint.o_direct in
                   let on_key = read_caching_key ~__context in
                   let reason_key = read_caching_reason_key ~__context in
                   Mutex.execute vdi_read_caching_m (fun () ->
                       Db.VDI.remove_from_sm_config ~__context ~self ~key:on_key;
                       Db.VDI.remove_from_sm_config ~__context ~self ~key:reason_key;
                       Db.VDI.add_to_sm_config ~__context ~self ~key:on_key
                         ~value:(string_of_bool read_caching);
                       if not read_caching then
                         Db.VDI.add_to_sm_config ~__context ~self ~key:reason_key
                           ~value:(attach_info_v1.Smint.o_direct_reason)
                     )
                 );
               { params = attach_info_v1.Smint.params;
                 o_direct = attach_info_v1.Smint.o_direct;
                 o_direct_reason = attach_info_v1.Smint.o_direct_reason;
                 xenstore_data = attach_info_v1.Smint.xenstore_data; }
            ) in
        Mutex.execute vdi_read_write_m
          (fun () -> Hashtbl.replace vdi_read_write (sr, vdi) read_write);
        attach_info
      with Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))

    let activate context ~dbg ~dp ~sr ~vdi =
      try
        let read_write = Mutex.execute vdi_read_write_m
            (fun () ->
               if not (Hashtbl.mem vdi_read_write (sr, vdi)) then error "VDI.activate: doesn't know if sr:%s vdi:%s is RO or RW" sr vdi;
               Hashtbl.find vdi_read_write (sr, vdi)) in
        for_vdi ~dbg ~sr ~vdi "VDI.activate"
          (fun device_config _type sr self ->
             Server_helpers.exec_with_new_task "VDI.activate" ~subtask_of:(Ref.of_string dbg)
               (fun __context ->
                  (if read_write
                   then Db.VDI.remove_from_other_config ~__context ~self ~key:"content_id"));
             (* If the backend doesn't advertise the capability then do nothing *)
             if List.mem_assoc Smint.Vdi_activate (Sm.features_of_driver _type)
             then Sm.vdi_activate device_config _type sr self read_write
             else info "%s sr:%s does not support vdi_activate: doing nothing" dp (Ref.string_of sr)
          )
      with Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))

    let deactivate context ~dbg ~dp ~sr ~vdi =
      try
        for_vdi ~dbg ~sr ~vdi "VDI.deactivate"
          (fun device_config _type sr self ->
             Server_helpers.exec_with_new_task "VDI.deactivate" ~subtask_of:(Ref.of_string dbg)
               (fun __context ->
                  let other_config = Db.VDI.get_other_config ~__context ~self in
                  if not (List.mem_assoc "content_id" other_config)
                  then Db.VDI.add_to_other_config ~__context ~self ~key:"content_id" ~value:(Uuid.string_of_uuid (Uuid.make_uuid ())));
             (* If the backend doesn't advertise the capability then do nothing *)
             if List.mem_assoc Smint.Vdi_deactivate (Sm.features_of_driver _type)
             then Sm.vdi_deactivate device_config _type sr self
             else info "%s sr:%s does not support vdi_deactivate: doing nothing" dp (Ref.string_of sr)
          )
      with Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))

    let detach context ~dbg ~dp ~sr ~vdi =
      try
        for_vdi ~dbg ~sr ~vdi "VDI.detach"
          (fun device_config _type sr self ->
             Sm.vdi_detach device_config _type sr self;
             Server_helpers.exec_with_new_task "VDI.detach" ~subtask_of:(Ref.of_string dbg) (fun __context ->
                 let on_key = read_caching_key ~__context in
                 let reason_key = read_caching_reason_key ~__context in
                 Mutex.execute vdi_read_caching_m (fun () ->
                     Db.VDI.remove_from_sm_config ~__context ~self ~key:on_key;
                     Db.VDI.remove_from_sm_config ~__context ~self ~key:reason_key
                   )
               )
          );
        Mutex.execute vdi_read_write_m
          (fun () -> Hashtbl.remove vdi_read_write (sr, vdi))
      with Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))

    let epoch_end context ~dbg ~sr ~vdi =
      try
        for_vdi ~dbg ~sr ~vdi "VDI.epoch_end"
          (fun device_config _type sr self ->
             Sm.vdi_epoch_end device_config _type sr self)
      with
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))

    let require_uuid vdi_info =
      match vdi_info.Smint.vdi_info_uuid with
      | Some uuid -> uuid
      | None -> failwith "SM backend failed to return <uuid> field"

    let newvdi ~__context vi =
      (* The current backends stash data directly in the db *)
      let uuid = require_uuid vi in
      vdi_info_from_db ~__context (Db.VDI.get_by_uuid ~__context ~uuid)

    let create context ~dbg ~sr ~vdi_info =
      try
        Server_helpers.exec_with_new_task "VDI.create" ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
             let vi =
               Sm.call_sm_functions ~__context ~sR:sr
                 (fun device_config _type ->
                    Sm.vdi_create device_config _type sr vdi_info.sm_config vdi_info.ty
                      vdi_info.virtual_size vdi_info.name_label vdi_info.name_description
                      vdi_info.metadata_of_pool vdi_info.is_a_snapshot
                      vdi_info.snapshot_time vdi_info.snapshot_of vdi_info.read_only
                 ) in
             newvdi ~__context vi
          )
      with
      | Api_errors.Server_error(code, params) -> raise (Backend_error(code, params))
      | Sm.MasterOnly -> redirect sr

    (* A list of keys in sm-config that will be preserved on clone/snapshot *)
    let sm_config_keys_to_preserve_on_clone = [
      "base_mirror"
    ]

    let snapshot_and_clone call_name call_f is_a_snapshot context ~dbg ~sr ~vdi_info =
      try
        Server_helpers.exec_with_new_task call_name ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             let vi = for_vdi ~dbg ~sr ~vdi:vdi_info.vdi call_name
                 (fun device_config _type sr self ->
                    call_f device_config _type vdi_info.sm_config sr self
                 ) in
             (* PR-1255: modify clone, snapshot to take the same parameters as create? *)
             let self, _ = find_vdi ~__context sr vi.Smint.vdi_info_location in
             let clonee, _ = find_vdi ~__context sr vdi_info.vdi in
             let content_id =
               try
                 List.assoc "content_id"
                   (Db.VDI.get_other_config ~__context ~self:clonee)
               with _ ->
                 Uuid.string_of_uuid (Uuid.make_uuid ())
             in
             Db.VDI.set_name_label ~__context ~self ~value:vdi_info.name_label;
             Db.VDI.set_name_description ~__context ~self ~value:vdi_info.name_description;
             Db.VDI.set_snapshot_time ~__context ~self ~value:(Date.of_string vdi_info.snapshot_time);
             Db.VDI.set_is_a_snapshot ~__context ~self ~value:is_a_snapshot;
             Db.VDI.remove_from_other_config ~__context ~self ~key:"content_id";
             Db.VDI.add_to_other_config ~__context ~self ~key:"content_id" ~value:content_id;
             debug "copying sm-config";
             List.iter (fun (key, value) ->
                 let preserve = List.mem key sm_config_keys_to_preserve_on_clone in
                 if preserve then (
                   Db.VDI.remove_from_sm_config ~__context ~self ~key;
                   Db.VDI.add_to_sm_config ~__context ~self ~key ~value;
                 )
               ) vdi_info.sm_config;
             for_vdi ~dbg ~sr ~vdi:vi.Smint.vdi_info_location "VDI.update"
               (fun device_config _type sr self ->
                  Sm.vdi_update device_config _type sr self
               );
             let vdi = vdi_info_from_db ~__context self in
             debug "vdi = %s" (string_of_vdi_info vdi);
             vdi
          )
      with
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))
      | Smint.Not_implemented_in_backend ->
        raise (Unimplemented call_name)
      | Sm.MasterOnly -> redirect sr


    let snapshot = snapshot_and_clone "VDI.snapshot" Sm.vdi_snapshot true
    let clone = snapshot_and_clone "VDI.clone" Sm.vdi_clone false

    let set_name_label context ~dbg ~sr ~vdi ~new_name_label =
      Server_helpers.exec_with_new_task "VDI.set_name_label" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let self, _ = find_vdi ~__context sr vdi in
           Db.VDI.set_name_label ~__context ~self ~value:new_name_label
        )

    let set_name_description context ~dbg ~sr ~vdi ~new_name_description =
      Server_helpers.exec_with_new_task "VDI.set_name_description" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let self, _ = find_vdi ~__context sr vdi in
           Db.VDI.set_name_description ~__context ~self ~value:new_name_description
        )

    let resize context ~dbg ~sr ~vdi ~new_size =
      try
        let vi = for_vdi ~dbg ~sr ~vdi "VDI.resize"
            (fun device_config _type sr self ->
               Sm.vdi_resize device_config _type sr self new_size
            ) in
        Server_helpers.exec_with_new_task "VDI.resize" ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             let self, _ = find_vdi ~__context sr vi.Smint.vdi_info_location in
             Db.VDI.get_virtual_size ~__context ~self
          )
      with
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))
      | Smint.Not_implemented_in_backend ->
        raise (Unimplemented "VDI.resize")
      | Sm.MasterOnly -> redirect sr

    let destroy context ~dbg ~sr ~vdi =
      try
        for_vdi ~dbg ~sr ~vdi "VDI.destroy"
          (fun device_config _type sr self ->
             Sm.vdi_delete device_config _type sr self
          );
        Mutex.execute vdi_read_write_m
          (fun () -> Hashtbl.remove vdi_read_write (sr, vdi))
      with
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))
      | No_VDI ->
        raise (Vdi_does_not_exist vdi)
      | Sm.MasterOnly -> redirect sr

    let stat context ~dbg ~sr ~vdi =
      try
        Server_helpers.exec_with_new_task "VDI.stat" ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             for_vdi ~dbg ~sr ~vdi "VDI.stat"
               (fun device_config _type sr self ->
                  Sm.vdi_update device_config _type sr self;
                  vdi_info_of_vdi_rec __context (Db.VDI.get_record ~__context ~self)
               )
          )
      with e ->
        error "VDI.stat caught: %s" (Printexc.to_string e);
        raise (Vdi_does_not_exist vdi)

    let introduce context ~dbg ~sr ~uuid ~sm_config ~location =
      try
        Server_helpers.exec_with_new_task "VDI.introduce" ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
             let vi =
               Sm.call_sm_functions ~__context ~sR:sr
                 (fun device_config sr_type ->
                    Sm.vdi_introduce device_config sr_type sr uuid sm_config location) in
             newvdi ~__context vi
          )
      with e ->
        error "VDI.introduce caught: %s" (Printexc.to_string e);
        raise (Vdi_does_not_exist location)

    let set_persistent context ~dbg ~sr ~vdi ~persistent =
      try
        Server_helpers.exec_with_new_task "VDI.set_persistent" ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             if not persistent then begin
               info "VDI.set_persistent: calling VDI.clone and VDI.destroy to make an empty vhd-leaf";
               let location = for_vdi ~dbg ~sr ~vdi "VDI.clone"
                   (fun device_config _type sr self ->
                      let vi = Sm.vdi_clone device_config _type [] sr self in
                      vi.Smint.vdi_info_location
                   ) in
               for_vdi ~dbg ~sr ~vdi:location "VDI.destroy"
                 (fun device_config _type sr self ->
                    Sm.vdi_delete device_config _type sr self
                 )
             end
          )
      with
      | Api_errors.Server_error(code, params) -> raise (Backend_error(code, params))
      | Sm.MasterOnly -> redirect sr

    let get_by_name context ~dbg ~sr ~name =
      info "VDI.get_by_name dbg:%s sr:%s name:%s" dbg sr name;
      (* PR-1255: the backend should do this for us *)
      Server_helpers.exec_with_new_task "VDI.get_by_name" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           (* PR-1255: the backend should do this for us *)
           try
             let _, vdi = find_content ~__context ~sr name in
             let vi = vdi_info_of_vdi_rec __context vdi in
             debug "VDI.get_by_name returning successfully";
             vi
           with e ->
             error "VDI.get_by_name caught: %s" (Printexc.to_string e);
             raise (Vdi_does_not_exist name)
        )

    let set_content_id context ~dbg ~sr ~vdi ~content_id =
      info "VDI.get_by_content dbg:%s sr:%s vdi:%s content_id:%s" dbg sr vdi content_id;
      (* PR-1255: the backend should do this for us *)
      Server_helpers.exec_with_new_task "VDI.set_content_id" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let vdi, _ = find_vdi ~__context sr vdi in
           Db.VDI.remove_from_other_config ~__context ~self:vdi ~key:"content_id";
           Db.VDI.add_to_other_config ~__context ~self:vdi ~key:"content_id" ~value:content_id
        )

    let similar_content context ~dbg ~sr ~vdi =
      info "VDI.similar_content dbg:%s sr:%s vdi:%s" dbg sr vdi;
      Server_helpers.exec_with_new_task "VDI.similar_content" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           (* PR-1255: the backend should do this for us. *)
           let sr_ref = Db.SR.get_by_uuid ~__context ~uuid:sr in
           (* Return a nearest-first list of similar VDIs. "near" should mean
              					   "has similar blocks" but we approximate this with distance in the tree *)
           let module StringMap = Map.Make(struct type t = string let compare = compare end) in
           let _vhdparent = "vhd-parent" in
           let open Db_filter_types in
           let all = Db.VDI.get_records_where ~__context ~expr:(Eq (Field "SR", Literal (Ref.string_of sr_ref))) in
           let locations = List.fold_left
               (fun acc (_, vdi_rec) -> StringMap.add vdi_rec.API.vDI_location vdi_rec acc)
               StringMap.empty all in
           (* Compute a map of parent location -> children locations *)
           let children, parents = List.fold_left
               (fun (children, parents) (vdi_r, vdi_rec) ->
                  if List.mem_assoc _vhdparent vdi_rec.API.vDI_sm_config then begin
                    let me = vdi_rec.API.vDI_location in
                    let parent = List.assoc _vhdparent vdi_rec.API.vDI_sm_config in
                    let other_children = if StringMap.mem parent children then StringMap.find parent children else [] in
                    (StringMap.add parent (me :: other_children) children),
                    (StringMap.add me parent parents)
                  end else (children, parents)) (StringMap.empty, StringMap.empty) all in

           let rec explore current_distance acc vdi =
             (* add me *)
             let acc = StringMap.add vdi current_distance acc in
             (* add the parent *)
             let parent = if StringMap.mem vdi parents then [ StringMap.find vdi parents ] else [] in
             let children = if StringMap.mem vdi children then StringMap.find vdi children else [] in
             List.fold_left
               (fun acc vdi ->
                  if not(StringMap.mem vdi acc)
                  then explore (current_distance + 1) acc vdi
                  else acc) acc (parent @ children) in
           let module IntMap = Map.Make(struct type t = int let compare = compare end) in
           let invert map =
             StringMap.fold
               (fun vdi n acc ->
                  let current = if IntMap.mem n acc then IntMap.find n acc else [] in
                  IntMap.add n (vdi :: current) acc
               ) map IntMap.empty in
           let _, vdi_rec = find_vdi ~__context sr vdi in
           let vdis = explore 0 StringMap.empty vdi_rec.API.vDI_location |> invert |> IntMap.bindings |> List.map snd |> List.concat in
           let vdi_recs = List.map (fun l -> StringMap.find l locations) vdis in
           (** We drop cbt_metadata VDIs that do not have any actual data *)
           let vdi_recs = List.filter (fun r -> r.API.vDI_type <> `cbt_metadata) vdi_recs in
           List.map (fun x -> vdi_info_of_vdi_rec __context x) vdi_recs
        )

    let compose context ~dbg ~sr ~vdi1 ~vdi2 =
      info "VDI.compose dbg:%s sr:%s vdi1:%s vdi2:%s" dbg sr vdi1 vdi2;
      try
        Server_helpers.exec_with_new_task "VDI.compose" ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             (* This call 'operates' on vdi2 *)
             let vdi1 = find_vdi ~__context sr vdi1 |> fst in
             for_vdi ~dbg ~sr ~vdi:vdi2 "VDI.compose"
               (fun device_config _type sr self ->
                  Sm.vdi_compose device_config _type sr vdi1 self
               )
          )
      with
      | Smint.Not_implemented_in_backend ->
        raise (Unimplemented "VDI.compose")
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))
      | No_VDI ->
        raise (Vdi_does_not_exist vdi1)
      | Sm.MasterOnly -> redirect sr

    let add_to_sm_config context ~dbg ~sr ~vdi ~key ~value =
      info "VDI.add_to_sm_config dbg:%s sr:%s vdi:%s key:%s value:%s" dbg sr vdi key value;
      Server_helpers.exec_with_new_task "VDI.add_to_sm_config" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let self = find_vdi ~__context sr vdi |> fst in
           Db.VDI.add_to_sm_config ~__context ~self ~key ~value)

    let remove_from_sm_config context ~dbg ~sr ~vdi ~key =
      info "VDI.remove_from_sm_config dbg:%s sr:%s vdi:%s key:%s" dbg sr vdi key;
      Server_helpers.exec_with_new_task "VDI.remove_from_sm_config" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let self = find_vdi ~__context sr vdi |> fst in
           Db.VDI.remove_from_sm_config ~__context ~self ~key)

    let get_url context ~dbg ~sr ~vdi =
      info "VDI.get_url dbg:%s sr:%s vdi:%s" dbg sr vdi;
      (* XXX: PR-1255: tapdisk shouldn't hardcode xapi urls *)
      (* peer_ip/session_ref/vdi_ref *)
      Server_helpers.exec_with_new_task "VDI.get_url" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
           let ip = Helpers.get_management_ip_addr ~__context |> Opt.unbox in
           let rpc = Helpers.make_rpc ~__context in
           let localhost = Helpers.get_localhost ~__context in
           (* XXX: leaked *)
           let session_ref = XenAPI.Session.slave_login rpc localhost !Xapi_globs.pool_secret in
           let vdi, _ = find_vdi ~__context sr vdi in
           Printf.sprintf "%s/%s/%s" ip (Ref.string_of session_ref) (Ref.string_of vdi))

    let call_cbt_function context ~f ~f_name ~dbg ~sr ~vdi =
      try
        for_vdi ~dbg ~sr ~vdi f_name
          (fun device_config _type sr self ->
             f device_config _type sr self
          );
      with
      | Smint.Not_implemented_in_backend ->
        raise (Unimplemented f_name)
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))
      | No_VDI ->
        raise (Vdi_does_not_exist vdi)
      | Sm.MasterOnly -> redirect sr

    let enable_cbt context =
      call_cbt_function context ~f:Sm.vdi_enable_cbt ~f_name:"VDI.enable_cbt"

    let disable_cbt context =
      call_cbt_function context ~f:Sm.vdi_disable_cbt ~f_name:"VDI.disable_cbt"

    let data_destroy context ~dbg ~sr ~vdi =
      call_cbt_function context ~f:Sm.vdi_data_destroy ~f_name:"VDI.data_destroy" ~dbg ~sr ~vdi;
      set_content_id context ~dbg ~sr ~vdi ~content_id:"/No content: this is a cbt_metadata VDI/"

    let list_changed_blocks context ~dbg ~sr ~vdi_from ~vdi_to =
      try
        Server_helpers.exec_with_new_task "VDI.list_changed_blocks" ~subtask_of:(Ref.of_string dbg)
          (fun __context ->
             let vdi_from = find_vdi ~__context sr vdi_from |> fst in
             for_vdi ~dbg ~sr ~vdi:vdi_to "VDI.list_changed_blocks"
               (fun device_config _type sr vdi_to ->
                  Sm.vdi_list_changed_blocks device_config _type sr ~vdi_from ~vdi_to
               ))
      with
      | Smint.Not_implemented_in_backend ->
        raise (Unimplemented "VDI.list_changed_blocks")
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))
      | Sm.MasterOnly -> redirect sr

  end

  let get_by_name context ~dbg ~name = assert false

  module DATA = struct
    let copy_into context ~dbg ~sr ~vdi ~url ~dest = assert false
    let copy context ~dbg ~sr ~vdi ~dp ~url ~dest = assert false
    module MIRROR = struct
      let start context ~dbg ~sr ~vdi ~dp ~url ~dest = assert false
      let stop context ~dbg ~id = assert false
      let list context ~dbg = assert false
      let stat context ~dbg ~id = assert false
      let receive_start context ~dbg ~sr ~vdi_info ~id ~similar = assert false
      let receive_finalize context ~dbg ~id = assert false
      let receive_cancel context ~dbg ~id = assert false
    end
  end

  module Policy = struct
    let get_backend_vm context ~dbg ~vm ~sr ~vdi = assert false
  end

  module TASK = struct
    let stat context ~dbg ~task = assert false
    let destroy context ~dbg ~task = assert false
    let cancel context ~dbg ~task = assert false
    let list context ~dbg = assert false
  end

  module UPDATES = struct
    let get context ~dbg ~from ~timeout = assert false
  end
end

module type SERVER = sig
  val process : Smint.request -> Rpc.call -> Rpc.response
end

(* Start a set of servers for all SMAPIv1 plugins *)
let start_smapiv1_servers () =
  let drivers = Sm.supported_drivers () in
  List.iter (fun ty ->
      let path = !Storage_interface.default_path ^ ".d/" ^ ty in
      let queue_name = !Storage_interface.queue_name ^ "." ^ ty in
      let module S = Storage_interface.Server(SMAPIv1) in
      let s = Xcp_service.make ~path ~queue_name ~rpc_fn:(S.process None) () in
      let (_: Thread.t) = Thread.create (fun () -> Xcp_service.serve_forever s) () in
      ()
    ) drivers

let make_service uuid ty =
  {
    System_domains.uuid = uuid;
    ty = Constants._SM;
    instance = ty;
    url = Constants.path [ Constants._services; Constants._driver; uuid; Constants._SM; ty ];
  }

let check_queue_exists queue_name =
  let t = Xcp_client.(get_ok (Message_switch_unix.Protocol_unix.Client.connect ~switch:!switch_path ())) in
  let results =
    match Message_switch_unix.Protocol_unix.Client.list ~t ~prefix:!Storage_interface.queue_name ~filter:`Alive () with
    | `Ok list -> list
    | _ -> failwith "Failed to contact switch" (* Shouldn't ever happen *) in
  if not (List.mem queue_name results)
  then
    let prefix_len = String.length !Storage_interface.queue_name + 1 in
    let driver = String.sub queue_name prefix_len (String.length queue_name - prefix_len) in
    raise Api_errors.(Server_error(sr_unknown_driver,[driver]))

let external_rpc queue_name uri =
  let open Xcp_client in
  if !use_switch then check_queue_exists queue_name;
  fun call ->
    if !use_switch
    then json_switch_rpc queue_name call
    else xml_http_rpc
        ~srcstr:(get_user_agent ())
        ~dststr:queue_name
        uri
        call

(* Internal exception, never escapes the module *)
exception Message_switch_failure

(** Synchronise the SM table with the SMAPIv1 plugins on the disk and the SMAPIv2
    plugins mentioned in the configuration file whitelist. *)
let on_xapi_start ~__context =
  let existing = List.map (fun (rf, rc) -> rc.API.sM_type, (rf, rc)) (Db.SM.get_all_records ~__context) in
  let explicitly_configured_drivers = List.filter_map (function `Sm x -> Some x | `All -> None) !Xapi_globs.sm_plugins in
  let smapiv1_drivers = Sm.supported_drivers () in
  let configured_drivers = explicitly_configured_drivers @ smapiv1_drivers in
  let in_use_drivers = List.map (fun (rf, rc) -> rc.API.sR_type) (Db.SR.get_all_records ~__context) in
  let to_keep = configured_drivers @ in_use_drivers in
  (* Delete all records which aren't configured or in-use *)
  List.iter
    (fun ty ->
       info "Unregistering SM plugin %s since not in the whitelist and not in-use" ty;
       let self, _ = List.assoc ty existing in
       try
         Db.SM.destroy ~__context ~self
       with _ -> ()
    ) (List.set_difference (List.map fst existing) to_keep);
  (* Create all missing SMAPIv1 plugins *)
  List.iter
    (fun ty ->
       let query_result = Sm.info_of_driver ty |> Smint.query_result_of_sr_driver_info in
       Xapi_sm.create_from_query_result ~__context query_result
    ) (List.set_difference smapiv1_drivers (List.map fst existing));
  (* Update all existing SMAPIv1 plugins *)
  List.iter
    (fun ty ->
       let query_result = Sm.info_of_driver ty |> Smint.query_result_of_sr_driver_info in
       Xapi_sm.update_from_query_result ~__context (List.assoc ty existing) query_result
    ) (List.intersect smapiv1_drivers (List.map fst existing));
  let smapiv2_drivers = List.set_difference to_keep smapiv1_drivers in
  (* Query the message switch to detect running SMAPIv2 plugins. *)
  let running_smapiv2_drivers =
    if !Xcp_client.use_switch then begin
      try
        let open Message_switch_unix.Protocol_unix in
        let (>>|) result f =
          match Client.error_to_msg result with
          | `Error (`Msg x) ->
            error "Error %s while querying message switch queues" x;
            raise Message_switch_failure
          | `Ok x -> f x
        in
        Client.connect ~switch:!Xcp_client.switch_path ()
        >>| fun t ->
        Client.list ~t ~prefix:!Storage_interface.queue_name ~filter:`Alive ()
        >>| fun running_smapiv2_driver_queues ->
        List.filter
          (fun driver ->
             List.exists
               (Xstringext.String.endswith driver)
               running_smapiv2_driver_queues)
          smapiv2_drivers
      with
      | Message_switch_failure -> [] (* no more logging *)
      | e ->
        error "Unexpected error querying the message switch: %s" (Printexc.to_string e);
        Debug.log_backtrace e (Backtrace.get e);
        []
    end
    else smapiv2_drivers
  in
  (* Create all missing SMAPIv2 plugins *)
  let query ty =
    let queue_name = !Storage_interface.queue_name ^ "." ^ ty in
    let uri () = Storage_interface.uri () ^ ".d/" ^ ty in
    let rpc = external_rpc queue_name uri in
    let module C = Storage_interface.Client(struct let rpc = rpc end) in
    let dbg = Context.string_of_task __context in
    C.Query.query ~dbg in
  List.iter
    (fun ty ->
       Xapi_sm.create_from_query_result ~__context (query ty)
    ) (List.set_difference running_smapiv2_drivers (List.map fst existing));
  (* Update all existing SMAPIv2 plugins *)
  List.iter
    (fun ty ->
       Xapi_sm.update_from_query_result ~__context (List.assoc ty existing) (query ty)
    ) (List.intersect running_smapiv2_drivers (List.map fst existing))

let bind ~__context ~pbd =
  (* Start the VM if necessary, record its uuid *)
  let driver = System_domains.storage_driver_domain_of_pbd ~__context ~pbd in
  if Db.VM.get_power_state ~__context ~self:driver = `Halted then begin
    info "PBD %s driver domain %s is offline: starting" (Ref.string_of pbd) (Ref.string_of driver);
    try
      Helpers.call_api_functions ~__context
        (fun rpc session_id -> XenAPI.VM.start rpc session_id driver false false)
    with (Api_errors.Server_error(code, params)) when code = Api_errors.vm_bad_power_state ->
      error "Caught VM_BAD_POWER_STATE [ %s ]" (String.concat "; " params);
      (* ignore for now *)
  end;
  let uuid = Db.VM.get_uuid ~__context ~self:driver in

  let sr = Db.PBD.get_SR ~__context ~self:pbd in
  let ty = Db.SR.get_type ~__context ~self:sr in
  let sr = Db.SR.get_uuid ~__context ~self:sr in
  let queue_name = !Storage_interface.queue_name ^ "." ^ ty in
  let uri () = Storage_interface.uri () ^ ".d/" ^ ty in
  let rpc = external_rpc queue_name uri in
  let service = make_service uuid ty in
  System_domains.register_service service queue_name;
  let module Client = Storage_interface.Client(struct let rpc = rpc end) in
  let dbg = Context.string_of_task __context in
  let info = Client.Query.query ~dbg in
  Storage_mux.register sr rpc uuid info;
  info

let unbind ~__context ~pbd =
  let driver = System_domains.storage_driver_domain_of_pbd ~__context ~pbd in
  let uuid = Db.VM.get_uuid ~__context ~self:driver in

  let sr = Db.PBD.get_SR ~__context ~self:pbd in
  let ty = Db.SR.get_type ~__context ~self:sr in

  let sr = Db.SR.get_uuid ~__context ~self:sr in
  info "SR %s will nolonger be implemented by VM %s" sr (Ref.string_of driver);
  Storage_mux.unregister sr;

  let service = make_service uuid ty in
  System_domains.unregister_service service

let rpc call = Storage_mux.Server.process None call

module Client = Client(struct let rpc = rpc end)

let print_delta d =
  debug "Received update: %s" (Jsonrpc.to_string (Storage_interface.Dynamic.rpc_of_id d))

let event_wait dbg p =
  let finished = ref false in
  let event_id = ref "" in
  while not !finished do
    debug "Calling UPDATES.get %s %s 30" dbg !event_id;
    let deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    List.iter (fun d -> print_delta d) deltas;
    event_id := next_id;
    List.iter (fun d -> if p d then finished := true) deltas;
  done

let task_ended dbg id =
  match (Client.TASK.stat dbg id).Task.state with
  | Task.Completed _
  | Task.Failed _ -> true
  | Task.Pending _ -> false

let success_task dbg id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id;
  match t.Task.state with
  | Task.Completed _ -> t
  | Task.Failed x -> raise (exn_of_exnty (Exception.exnty_of_rpc x))
  | Task.Pending _ -> failwith "task pending"

let wait_for_task dbg id =
  debug "Waiting for task id=%s to finish" id;
  let finished = function
    | Dynamic.Task id' ->
      id = id' && (task_ended dbg id)
    | _ ->
      false in
  event_wait dbg finished;
  id

let vdi_of_task dbg t =
  match t.Task.state with
  | Task.Completed { Task.result = Some Vdi_info v } -> v
  | Task.Completed _ -> failwith "Runtime type error in vdi_of_task"
  | _ -> failwith "Task not completed"

let mirror_of_task dbg t =
  match t.Task.state with
  | Task.Completed { Task.result = Some Mirror_id i } -> i
  | Task.Completed _ -> failwith "Runtime type error in mirror_of_task"
  | _ -> failwith "Task not complete"

let progress_map_tbl = Hashtbl.create 10
let mirror_task_tbl = Hashtbl.create 10
let progress_map_m = Mutex.create ()

let add_to_progress_map f id = Mutex.execute progress_map_m (fun () -> Hashtbl.add progress_map_tbl id f); id
let remove_from_progress_map id = Mutex.execute progress_map_m (fun () -> Hashtbl.remove progress_map_tbl id); id
let get_progress_map id = Mutex.execute progress_map_m (fun () -> try Hashtbl.find progress_map_tbl id with _ -> (fun x -> x))

let register_mirror __context vdi =
  let task = Context.get_task_id __context in
  debug "Registering mirror of vdi %s with task %s" vdi (Ref.string_of task);
  Mutex.execute progress_map_m (fun () -> Hashtbl.add mirror_task_tbl vdi task); vdi
let unregister_mirror vdi = Mutex.execute progress_map_m (fun () -> Hashtbl.remove mirror_task_tbl vdi); vdi
let get_mirror_task vdi = Mutex.execute progress_map_m (fun () -> Hashtbl.find mirror_task_tbl vdi)

exception Not_an_sm_task
let wrap id = TaskHelper.Sm id
let unwrap x = match x with | TaskHelper.Sm id -> id | _ -> raise Not_an_sm_task
let register_task __context id = TaskHelper.register_task __context (wrap id); id
let unregister_task __context id = TaskHelper.unregister_task __context (wrap id); id

let update_task ~__context id =
  try
    let self = TaskHelper.id_to_task_exn (TaskHelper.Sm id) in (* throws Not_found *)
    let dbg = Context.string_of_task __context in
    let task_t = Client.TASK.stat dbg id in
    let map = get_progress_map id in
    match task_t.Task.state with
    | Task.Pending x ->
      Db.Task.set_progress ~__context ~self ~value:(map x)
    | _ -> ()
  with Not_found ->
    (* Since this is called on all tasks, possibly after the task has been
       		   destroyed, it's safe to ignore a Not_found exception here. *)
    ()
     | e ->
       error "storage event: Caught %s while updating task" (Printexc.to_string e)

let update_mirror ~__context id =
  try
    let dbg = Context.string_of_task __context in
    let m = Client.DATA.MIRROR.stat dbg id in
    if m.Mirror.failed
    then
      debug "Mirror %s has failed" id;
    let task = get_mirror_task m.Mirror.source_vdi in
    debug "Mirror associated with task: %s" (Ref.string_of task);
    (* Just to get a nice error message *)
    Db.Task.remove_from_other_config ~__context ~self:task ~key:"mirror_failed";
    Db.Task.add_to_other_config ~__context ~self:task ~key:"mirror_failed" ~value:m.Mirror.source_vdi;
    Helpers.call_api_functions ~__context
      (fun rpc session_id -> XenAPI.Task.cancel rpc session_id task)
  with
  | Not_found ->
    debug "Couldn't find mirror id: %s" id
  | Does_not_exist _ -> ()
  | e ->
    error "storage event: Caught %s while updating mirror" (Printexc.to_string e)

let rec events_watch ~__context from =
  let dbg = Context.string_of_task __context in
  let events, next = Client.UPDATES.get dbg from None in
  let open Dynamic in
  List.iter
    (function
      | Task id ->
        debug "sm event on Task %s" id;
        update_task ~__context id
      | Vdi vdi ->
        debug "sm event on VDI %s: ignoring" vdi
      | Dp dp ->
        debug "sm event on DP %s: ignoring" dp
      | Mirror id ->
        debug "sm event on mirror: %s" id;
        update_mirror ~__context id
    ) events;
  events_watch ~__context next

let events_from_sm () =
  ignore(Thread.create (fun () ->
      Server_helpers.exec_with_new_task "sm_events"
        (fun __context ->
           while true do
             try
               events_watch ~__context "";
             with e ->
               error "event thread caught: %s" (Printexc.to_string e);
               Thread.delay 10.
           done
        )) ())

let start () =
  let open Storage_impl.Local_domain_socket in
  let s = Xcp_service.make ~path:Xapi_globs.storage_unix_domain_socket ~queue_name:"org.xen.xapi.storage" ~rpc_fn:(Storage_mux.Server.process None) () in
  info "Started service on org.xen.xapi.storage";
  let (_: Thread.t) = Thread.create (fun () -> Xcp_service.serve_forever s) () in
  ()

(** [datapath_of_vbd domid userdevice] returns the name of the datapath which corresponds
    to device [userdevice] on domain [domid] *)
let datapath_of_vbd ~domid ~device =
  Printf.sprintf "vbd/%d/%s" domid device

let presentative_datapath_of_vbd ~__context ~vm ~vdi =
  try
    let vbds = Db.VDI.get_VBDs ~__context ~self:vdi in
    let vbd = List.find (fun self -> Db.VBD.get_VM ~__context ~self = vm) vbds in
    let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
    let device = Db.VBD.get_device ~__context ~self:vbd in
    if domid < 0 || device = "" then raise Not_found;
    datapath_of_vbd ~domid ~device
  with Not_found ->
    let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
    let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vdi in
    Printf.sprintf "vbd/%s/%s" vm_uuid vdi_uuid

let of_vbd ~__context ~vbd ~domid =
  let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
  let location = Db.VDI.get_location ~__context ~self:vdi in
  let sr = Db.VDI.get_SR ~__context ~self:vdi in
  let userdevice = Db.VBD.get_userdevice ~__context ~self:vbd in
  let hvm = Helpers.will_boot_hvm ~__context ~self:(Db.VBD.get_VM ~__context ~self:vbd) in
  let dbg = Context.get_task_id __context in
  let device_number = Device_number.of_string hvm userdevice in
  let device = Device_number.to_linux_device device_number in
  let dp = datapath_of_vbd ~domid ~device in
  rpc, (Ref.string_of dbg), dp, (Db.SR.get_uuid ~__context ~self:sr), location

(** [is_attached __context vbd] returns true if the [vbd] has an attached
    or activated datapath. *)
let is_attached ~__context ~vbd ~domid  =
  transform_storage_exn
    (fun () ->
       let rpc, dbg, dp, sr, vdi = of_vbd ~__context ~vbd ~domid in
       let open Vdi_automaton in
       let module C = Storage_interface.Client(struct let rpc = rpc end) in
       try
         let x = C.DP.stat_vdi ~dbg ~sr ~vdi () in
         x.superstate <> Detached
       with
       | e -> error "Unable to query state of VDI: %s, %s" vdi (Printexc.to_string e); false
    )

(** [on_vdi __context vbd domid f] calls [f rpc dp sr vdi] which is
    useful for executing Storage_interface.Client.VDI functions  *)
let on_vdi ~__context ~vbd ~domid f =
  let rpc, dbg, dp, sr, vdi = of_vbd ~__context ~vbd ~domid in
  let module C = Storage_interface.Client(struct let rpc = rpc end) in
  let dp = C.DP.create dbg dp in
  transform_storage_exn
    (fun () ->
       f rpc dbg dp sr vdi
    )

let reset ~__context ~vm =
  let dbg = Context.get_task_id __context in
  transform_storage_exn
    (fun () ->
       Opt.iter
         (fun pbd ->
            let sr = Db.SR.get_uuid ~__context ~self:(Db.PBD.get_SR ~__context ~self:pbd) in
            info "Resetting all state associated with SR: %s" sr;
            Client.SR.reset (Ref.string_of dbg) sr;
            Db.PBD.set_currently_attached ~__context ~self:pbd ~value:false;
         ) (System_domains.pbd_of_vm ~__context ~vm)
    )

(** [attach_and_activate __context vbd domid f] calls [f attach_info] where
    [attach_info] is the result of attaching a VDI which is also activated.
    This should be used everywhere except the migrate code, where we want fine-grained
    control of the ordering of attach/activate/deactivate/detach *)
let attach_and_activate ~__context ~vbd ~domid ~hvm f =
  transform_storage_exn
    (fun () ->
       let read_write = Db.VBD.get_mode ~__context ~self:vbd = `RW in
       on_vdi ~__context ~vbd ~domid
         (fun rpc dbg dp sr vdi ->
            let module C = Storage_interface.Client(struct let rpc = rpc end) in
            let attach_info = C.VDI.attach dbg dp sr vdi read_write in
            C.VDI.activate dbg dp sr vdi;
            f attach_info
         )
    )

(** [deactivate_and_detach __context vbd domid] idempotent function which ensures
    that any attached or activated VDI gets properly deactivated and detached. *)
let deactivate_and_detach ~__context ~vbd ~domid =
  transform_storage_exn
    (fun () ->
       (* It suffices to destroy the datapath: any attached or activated VDIs will be
          			   automatically detached and deactivated. *)
       on_vdi ~__context ~vbd ~domid
         (fun rpc dbg dp sr vdi ->
            let module C = Storage_interface.Client(struct let rpc = rpc end) in
            C.DP.destroy dbg dp false
         )
    )


let diagnostics ~__context =
  let dbg = Context.get_task_id __context |> Ref.string_of in
  String.concat "\n" [
    "DataPath information:";
    Client.DP.diagnostics ();
    "Backend information:";
    Client.Query.diagnostics dbg
  ]

let dp_destroy ~__context dp allow_leak =
  transform_storage_exn
    (fun () ->
       let dbg = Context.get_task_id __context in
       Client.DP.destroy (Ref.string_of dbg) dp allow_leak
    )

(* Set my PBD.currently_attached fields in the Pool database to match the local one *)
let resynchronise_pbds ~__context ~pbds =
  let dbg = Context.get_task_id __context in
  let srs = Client.SR.list (Ref.string_of dbg) in
  debug "Currently-attached SRs: [ %s ]" (String.concat "; " srs);
  List.iter
    (fun self ->
       let sr = Db.SR.get_uuid ~__context ~self:(Db.PBD.get_SR ~__context ~self) in
       let value = List.mem sr srs in
       debug "Setting PBD %s currently_attached <- %b" (Ref.string_of self) value;
       try
         if value then (let (_:query_result) = bind ~__context ~pbd:self in ());
         Db.PBD.set_currently_attached ~__context ~self ~value
       with e ->
         (* Unchecked this will block the dbsync code *)
         error "Service implementing SR %s has failed. Performing emergency reset of SR state" sr;
         Client.SR.reset (Ref.string_of dbg) sr;
         Db.PBD.set_currently_attached ~__context ~self ~value:false;
    ) pbds

(* -------------------------------------------------------------------------------- *)
(* The following functions are symptoms of a broken interface with the SM layer.
   They should be removed, by enhancing the SM layer. *)

(* This is a layering violation. The layers are:
     xapi: has a pool-wide view
     storage_impl: has a host-wide view of SRs and VDIs
     SM: has a SR-wide view
   Unfortunately the SM is storing some of its critical state (VDI-host locks) in the xapi
   metadata rather than on the backend storage. The xapi metadata is generally not authoritative
   and must be synchronised against the state of the world. Therefore we must synchronise the
   xapi view with the storage_impl view here. *)
let refresh_local_vdi_activations ~__context =
  let all_vdi_recs = Db.VDI.get_all_records ~__context in
  let localhost = Helpers.get_localhost ~__context in
  let all_hosts = Db.Host.get_all ~__context in

  let key host = Printf.sprintf "host_%s" (Ref.string_of host) in
  let hosts_of vdi_t =
    let prefix = "host_" in
    let ks = List.map fst vdi_t.API.vDI_sm_config in
    let ks = List.filter (String.startswith prefix) ks in
    let ks = List.map (fun k -> String.sub k (String.length prefix) (String.length k - (String.length prefix))) ks in
    List.map Ref.of_string ks in

  (* If this VDI is currently locked to this host, remove the lock.
     	   If this VDI is currently locked to a non-existent host (note host references
     	   change across pool join), remove the lock. *)
  let unlock_vdi (vdi_ref, vdi_rec) =
    (* VDI is already unlocked is the common case: avoid eggregious logspam *)
    let hosts = hosts_of vdi_rec in
    let i_locked_it = List.mem localhost hosts in
    let all = List.fold_left (&&) true in
    let someone_leaked_it = all (List.map (fun h -> not(List.mem h hosts)) all_hosts) in
    if i_locked_it || someone_leaked_it then begin
      info "Unlocking VDI %s (because %s)" (Ref.string_of vdi_ref)
        (if i_locked_it then "I locked it and then restarted" else "it was leaked (pool join?)");
      try
        List.iter (fun h -> Db.VDI.remove_from_sm_config ~__context ~self:vdi_ref ~key:(key h)) hosts
      with e ->
        error "Failed to unlock VDI %s: %s" (Ref.string_of vdi_ref) (ExnHelper.string_of_exn e)
    end in
  let open Vdi_automaton in
  (* Lock this VDI to this host *)
  let lock_vdi (vdi_ref, vdi_rec) ro_rw =
    info "Locking VDI %s" (Ref.string_of vdi_ref);
    if not(List.mem_assoc (key localhost) vdi_rec.API.vDI_sm_config) then begin
      try
        Db.VDI.add_to_sm_config ~__context ~self:vdi_ref ~key:(key localhost) ~value:(string_of_ro_rw ro_rw)
      with e ->
        error "Failed to lock VDI %s: %s" (Ref.string_of vdi_ref) (ExnHelper.string_of_exn e)
    end in
  let remember key ro_rw =
    (* The module above contains a hashtable of R/O vs R/W-ness *)
    Mutex.execute SMAPIv1.VDI.vdi_read_write_m
      (fun () -> Hashtbl.replace SMAPIv1.VDI.vdi_read_write key (ro_rw = RW)) in

  let dbg = Ref.string_of (Context.get_task_id __context) in
  let srs = Client.SR.list dbg in
  let sr_uuids = List.map (fun sr -> (sr, Db.SR.get_uuid ~__context ~self:sr)) (Db.SR.get_all ~__context) in
  List.iter
    (fun (vdi_ref, vdi_rec) ->
       let sr = List.assoc vdi_rec.API.vDI_SR sr_uuids in
       let vdi = vdi_rec.API.vDI_location in
       if List.mem sr srs
       then
         try
           let x = Client.DP.stat_vdi ~dbg ~sr ~vdi () in
           match x.superstate with
           | Activated RO ->
             lock_vdi (vdi_ref, vdi_rec) RO;
             remember (sr, vdi) RO
           | Activated RW ->
             lock_vdi (vdi_ref, vdi_rec) RW;
             remember (sr, vdi) RW
           | Attached RO ->
             unlock_vdi (vdi_ref, vdi_rec);
             remember (sr, vdi) RO
           | Attached RW ->
             unlock_vdi (vdi_ref, vdi_rec);
             remember (sr, vdi) RW
           | Detached ->
             unlock_vdi (vdi_ref, vdi_rec)
         with
         | e -> error "Unable to query state of VDI: %s, %s" vdi (Printexc.to_string e)
       else unlock_vdi (vdi_ref, vdi_rec)
    ) all_vdi_recs

(* This is a symptom of the ordering-sensitivity of the SM backend: it is not possible
   to upgrade RO -> RW or downgrade RW -> RO on the fly.
   One possible fix is to always attach RW and enforce read/only-ness at the VBD-level.
   However we would need to fix the LVHD "attach provisioning mode". *)
let vbd_attach_order ~__context vbds =
  (* return RW devices first since the storage layer can't upgrade a
     	   'RO attach' into a 'RW attach' *)
  let rw, ro = List.partition (fun self -> Db.VBD.get_mode ~__context ~self = `RW) vbds in
  rw @ ro

let vbd_detach_order ~__context vbds = List.rev (vbd_attach_order ~__context vbds)

let create_sr ~__context ~sr ~name_label ~name_description ~physical_size =
  transform_storage_exn
    (fun () ->
       let pbd, pbd_t = Sm.get_my_pbd_for_sr __context sr in
       let (_ : query_result) = bind ~__context ~pbd in
       let dbg = Ref.string_of (Context.get_task_id __context) in
       let result = Client.SR.create dbg (Db.SR.get_uuid ~__context ~self:sr) name_label name_description pbd_t.API.pBD_device_config physical_size in
       unbind ~__context ~pbd;
       result
    )

(* This is because the current backends want SR.attached <=> PBD.currently_attached=true.
   It would be better not to plug in the PBD, so that other API calls will be blocked. *)
let destroy_sr ~__context ~sr ~and_vdis =
  transform_storage_exn
    (fun () ->
       let pbd, pbd_t = Sm.get_my_pbd_for_sr __context sr in
       let (_ : query_result) = bind ~__context ~pbd in
       let dbg = Ref.string_of (Context.get_task_id __context) in
       let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
       Client.SR.attach dbg sr_uuid pbd_t.API.pBD_device_config;
       (* The current backends expect the PBD to be temporarily set to currently_attached = true *)
       Db.PBD.set_currently_attached ~__context ~self:pbd ~value:true;
       Pervasiveext.finally
         (fun () ->
            try
              List.iter (fun vdi ->
                  let location = Db.VDI.get_location ~__context ~self:vdi in
                  Client.VDI.destroy dbg sr_uuid location)
                and_vdis;
              Client.SR.destroy dbg sr_uuid
            with exn ->
              (* Clean up: SR is left attached if destroy fails *)
              Client.SR.detach dbg sr_uuid;
              raise exn
         )
         (fun () ->
            (* All PBDs are clearly currently_attached = false now *)
            Db.PBD.set_currently_attached ~__context ~self:pbd ~value:false);
       unbind ~__context ~pbd
    )

let task_cancel ~__context ~self =
  try
    let id = TaskHelper.task_to_id_exn self |> unwrap in
    let dbg = Context.string_of_task __context in
    info "storage_access: TASK.cancel %s" id;
    Client.TASK.cancel dbg id |> ignore;
    true
  with
  | Not_found -> false
  | Not_an_sm_task -> false
