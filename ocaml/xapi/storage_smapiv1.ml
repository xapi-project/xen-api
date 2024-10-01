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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
module Date = Xapi_stdext_date.Date
module XenAPI = Client.Client
open Storage_interface

exception No_VDI

let s_of_vdi = Vdi.string_of

let s_of_sr = Sr.string_of

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let with_dbg ~name ~dbg f =
  Debug_info.with_dbg ~module_name:"SMAPIv1" ~name ~dbg f

(* Find a VDI given a storage-layer SR and VDI *)
let find_vdi ~__context sr vdi =
  let sr = s_of_sr sr in
  let vdi = s_of_vdi vdi in
  let open Xapi_database.Db_filter_types in
  let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
  match
    Db.VDI.get_records_where ~__context
      ~expr:
        (And
           ( Eq (Field "location", Literal vdi)
           , Eq (Field "SR", Literal (Ref.string_of sr))
           )
        )
  with
  | x :: _ ->
      x
  | _ ->
      raise No_VDI

(* Find a VDI reference given a name *)
let find_content ~__context ?sr name =
  (* PR-1255: the backend should do this for us *)
  let open Xapi_database.Db_filter_types in
  let expr =
    Option.fold ~none:True
      ~some:(fun sr ->
        Eq
          ( Field "SR"
          , Literal
              (Ref.string_of (Db.SR.get_by_uuid ~__context ~uuid:(s_of_sr sr)))
          )
      )
      sr
  in
  let all = Db.VDI.get_records_where ~__context ~expr in
  List.find
    (fun (_, vdi_rec) -> false || vdi_rec.API.vDI_location = name (* PR-1255 *))
    all

let vdi_info_of_vdi_rec __context vdi_rec =
  let content_id =
    try List.assoc "content_id" vdi_rec.API.vDI_other_config
    with Not_found -> vdi_rec.API.vDI_location
    (* PR-1255 *)
  in
  {
    vdi= Storage_interface.Vdi.of_string vdi_rec.API.vDI_location
  ; uuid= Some vdi_rec.API.vDI_uuid
  ; content_id
  ; (* PR-1255 *)
    name_label= vdi_rec.API.vDI_name_label
  ; name_description= vdi_rec.API.vDI_name_description
  ; ty= Storage_utils.string_of_vdi_type vdi_rec.API.vDI_type
  ; metadata_of_pool= Ref.string_of vdi_rec.API.vDI_metadata_of_pool
  ; is_a_snapshot= vdi_rec.API.vDI_is_a_snapshot
  ; snapshot_time= Date.to_rfc3339 vdi_rec.API.vDI_snapshot_time
  ; snapshot_of=
      ( if Db.is_valid_ref __context vdi_rec.API.vDI_snapshot_of then
          Db.VDI.get_uuid ~__context ~self:vdi_rec.API.vDI_snapshot_of
        else
          ""
      )
      |> Storage_interface.Vdi.of_string
  ; read_only= vdi_rec.API.vDI_read_only
  ; cbt_enabled= vdi_rec.API.vDI_cbt_enabled
  ; virtual_size= vdi_rec.API.vDI_virtual_size
  ; physical_utilisation= vdi_rec.API.vDI_physical_utilisation
  ; persistent= vdi_rec.API.vDI_on_boot = `persist
  ; sharable= vdi_rec.API.vDI_sharable
  ; sm_config= vdi_rec.API.vDI_sm_config
  }

let redirect _sr =
  raise (Storage_error (Redirect (Some (Pool_role.get_master_address ()))))

(* Allow us to remember whether a VDI is attached read/only or read/write.
   		   If this is meaningful to the backend then this should be recorded there! *)
let vdi_read_write = Hashtbl.create 10

let vdi_read_write_m = Mutex.create ()

let vdi_read_caching_m = Mutex.create ()

module SMAPIv1 : Server_impl = struct
  (** xapi's builtin ability to call local SM plugins using the existing
      protocol. The code here should only call the SM functions and encapsulate
      the return or error properly. It should not perform side-effects on
      the xapi database: these should be handled in the layer above so they
      can be shared with other SM implementation types.

      Where this layer has to perform interface adjustments (see VDI.activate
      and the read/write debacle), this highlights desirable improvements to
      the backend interface.
    *)

  type context = unit

  let vdi_info_from_db ~__context self =
    let vdi_rec = Db.VDI.get_record ~__context ~self in
    vdi_info_of_vdi_rec __context vdi_rec

  (* For SMAPIv1, is_a_snapshot, snapshot_time and snapshot_of are stored in
     	 * xapi's database. For SMAPIv2 they should be implemented by the storage
     	 * backend. *)
  let set_is_a_snapshot _context ~dbg ~sr ~vdi ~is_a_snapshot =
    Server_helpers.exec_with_new_task "VDI.set_is_a_snapshot"
      ~subtask_of:(Ref.of_string dbg) (fun __context ->
        let vdi, _ = find_vdi ~__context sr vdi in
        Db.VDI.set_is_a_snapshot ~__context ~self:vdi ~value:is_a_snapshot
    )

  let set_snapshot_time _context ~dbg ~sr ~vdi ~snapshot_time =
    Server_helpers.exec_with_new_task "VDI.set_snapshot_time"
      ~subtask_of:(Ref.of_string dbg) (fun __context ->
        let vdi, _ = find_vdi ~__context sr vdi in
        let snapshot_time = Date.of_iso8601 snapshot_time in
        Db.VDI.set_snapshot_time ~__context ~self:vdi ~value:snapshot_time
    )

  let set_snapshot_of _context ~dbg ~sr ~vdi ~snapshot_of =
    Server_helpers.exec_with_new_task "VDI.set_snapshot_of"
      ~subtask_of:(Ref.of_string dbg) (fun __context ->
        let vdi, _ = find_vdi ~__context sr vdi in
        let snapshot_of, _ = find_vdi ~__context sr snapshot_of in
        Db.VDI.set_snapshot_of ~__context ~self:vdi ~value:snapshot_of
    )

  module Query = struct
    let query _context ~dbg:_ =
      {
        driver= "storage_access"
      ; name= "SMAPIv1 adapter"
      ; description=
          "Allows legacy SMAPIv1 adapters to expose an SMAPIv2 interface"
      ; vendor= "XCP"
      ; copyright= "see the source code"
      ; version= "2.0"
      ; required_api_version= "2.0"
      ; features= []
      ; configuration= []
      ; required_cluster_stack= []
      }

    let diagnostics _context ~dbg:_ =
      "No diagnostics are available for SMAPIv1 plugins"
  end

  module DP = Storage_skeleton.DP

  module SR = struct
    include Storage_skeleton.SR

    let probe _context ~dbg ~queue ~device_config ~sm_config =
      let _type =
        (* SMAPIv1 plugins have no namespaces, so strip off everything up to
           				   the final dot *)
        try
          let i = String.rindex queue '.' in
          String.sub queue (i + 1) (String.length queue - i - 1)
        with Not_found -> queue
      in
      Server_helpers.exec_with_new_task "SR.probe"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let task = Context.get_task_id __context in
          Storage_interface.Raw
            (Sm.sr_probe ~dbg
               (Some task, Sm.sm_master true :: device_config)
               _type sm_config
            )
      )

    let create _context ~dbg ~sr ~name_label ~name_description ~device_config
        ~physical_size =
      Server_helpers.exec_with_new_task "SR.create"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let subtask_of = Some (Context.get_task_id __context) in
          let sr =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr)
          in
          Db.SR.set_name_label ~__context ~self:sr ~value:name_label ;
          Db.SR.set_name_description ~__context ~self:sr ~value:name_description ;
          let device_config = Sm.sm_master true :: device_config in
          Sm.call_sm_functions ~__context ~sR:sr (fun _ _type ->
              try
                Sm.sr_create ~dbg
                  (subtask_of, device_config)
                  _type sr physical_size
              with
              | Smint.Not_implemented_in_backend ->
                  error "SR.create failed SR:%s Not_implemented_in_backend"
                    (Ref.string_of sr) ;
                  raise
                    (Storage_interface.Storage_error
                       (Backend_error
                          ( Api_errors.sr_operation_not_supported
                          , [Ref.string_of sr]
                          )
                       )
                    )
              | Api_errors.Server_error (code, params) ->
                  raise (Storage_error (Backend_error (code, params)))
              | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.create failed SR:%s error:%s" (Ref.string_of sr) e' ;
                  raise e
          ) ;
          List.filter (fun (x, _) -> x <> "SRmaster") device_config
      )

    let set_name_label _context ~dbg ~sr ~new_name_label =
      Server_helpers.exec_with_new_task "SR.set_name_label"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let sr =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr)
          in
          Db.SR.set_name_label ~__context ~self:sr ~value:new_name_label
      )

    let set_name_description _context ~dbg ~sr ~new_name_description =
      Server_helpers.exec_with_new_task "SR.set_name_description"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let sr =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr)
          in
          Db.SR.set_name_description ~__context ~self:sr
            ~value:new_name_description
      )

    let attach _context ~dbg ~sr ~device_config =
      Server_helpers.exec_with_new_task "SR.attach"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let sr =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr)
          in
          (* Existing backends expect an SRMaster flag to be added
             					   through the device-config. *)
          let srmaster = Helpers.i_am_srmaster ~__context ~sr in
          let device_config = Sm.sm_master srmaster :: device_config in
          Sm.call_sm_functions ~__context ~sR:sr (fun _ _type ->
              try
                Sm.sr_attach ~dbg
                  (Some (Context.get_task_id __context), device_config)
                  _type sr
              with
              | Api_errors.Server_error (code, params) ->
                  raise (Storage_error (Backend_error (code, params)))
              | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.attach failed SR:%s error:%s" (Ref.string_of sr) e' ;
                  raise e
          )
      )

    let detach _context ~dbg ~sr =
      Server_helpers.exec_with_new_task "SR.detach"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let sr =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr)
          in
          Sm.call_sm_functions ~__context ~sR:sr (fun device_config _type ->
              try Sm.sr_detach ~dbg device_config _type sr with
              | Api_errors.Server_error (code, params) ->
                  raise (Storage_error (Backend_error (code, params)))
              | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.detach failed SR:%s error:%s" (Ref.string_of sr) e' ;
                  raise e
          )
      )

    let reset _context ~dbg:_ ~sr:_ = assert false

    let destroy _context ~dbg ~sr =
      Server_helpers.exec_with_new_task "SR.destroy"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let sr =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr)
          in
          Sm.call_sm_functions ~__context ~sR:sr (fun device_config _type ->
              try Sm.sr_delete ~dbg device_config _type sr with
              | Smint.Not_implemented_in_backend ->
                  raise
                    (Storage_interface.Storage_error
                       (Backend_error
                          ( Api_errors.sr_operation_not_supported
                          , [Ref.string_of sr]
                          )
                       )
                    )
              | Api_errors.Server_error (code, params) ->
                  raise (Storage_error (Backend_error (code, params)))
              | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.detach failed SR:%s error:%s" (Ref.string_of sr) e' ;
                  raise e
          )
      )

    let stat _context ~dbg ~sr:sr' =
      Server_helpers.exec_with_new_task "SR.stat" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
          let sr =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr')
          in
          Sm.call_sm_functions ~__context ~sR:sr (fun device_config _type ->
              try
                Sm.sr_update ~dbg device_config _type sr ;
                let r = Db.SR.get_record ~__context ~self:sr in
                let sr_uuid = Some r.API.sR_uuid in
                let name_label = r.API.sR_name_label in
                let name_description = r.API.sR_name_description in
                let total_space = r.API.sR_physical_size in
                let free_space =
                  Int64.sub r.API.sR_physical_size r.API.sR_physical_utilisation
                in
                let clustered = false in
                let health = Storage_interface.Healthy in
                {
                  sr_uuid
                ; name_label
                ; name_description
                ; total_space
                ; free_space
                ; clustered
                ; health
                }
              with
              | Smint.Not_implemented_in_backend ->
                  raise
                    (Storage_interface.Storage_error
                       (Backend_error
                          ( Api_errors.sr_operation_not_supported
                          , [Ref.string_of sr]
                          )
                       )
                    )
              | Api_errors.Server_error (code, params) ->
                  error "SR.scan failed SR:%s code=%s params=[%s]"
                    (Ref.string_of sr) code
                    (String.concat "; " params) ;
                  raise (Storage_error (Backend_error (code, params)))
              | Sm.MasterOnly ->
                  redirect sr
              | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.scan failed SR:%s error:%s" (Ref.string_of sr) e' ;
                  raise e
          )
      )

    let scan _context ~dbg ~sr:sr' =
      Server_helpers.exec_with_new_task "SR.scan" ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
          let sr = Db.SR.get_by_uuid ~__context ~uuid:(s_of_sr sr') in
          Sm.call_sm_functions ~__context ~sR:sr (fun device_config _type ->
              try
                Sm.sr_scan ~dbg device_config _type sr ;
                let open Xapi_database.Db_filter_types in
                let vdis =
                  Db.VDI.get_records_where ~__context
                    ~expr:(Eq (Field "SR", Literal (Ref.string_of sr)))
                  |> List.map snd
                in
                List.map (vdi_info_of_vdi_rec __context) vdis
              with
              | Smint.Not_implemented_in_backend ->
                  raise
                    (Storage_interface.Storage_error
                       (Backend_error
                          ( Api_errors.sr_operation_not_supported
                          , [Ref.string_of sr]
                          )
                       )
                    )
              | Api_errors.Server_error (code, params) ->
                  error "SR.scan failed SR:%s code=%s params=[%s]"
                    (Ref.string_of sr) code
                    (String.concat "; " params) ;
                  raise (Storage_error (Backend_error (code, params)))
              | Sm.MasterOnly ->
                  redirect sr
              | e ->
                  let e' = ExnHelper.string_of_exn e in
                  error "SR.scan failed SR:%s error:%s" (Ref.string_of sr) e' ;
                  raise e
          )
      )

    let list _context ~dbg:_ = assert false

    let update_snapshot_info_src _context ~dbg:_ ~sr:_ ~vdi:_ ~url:_ ~dest:_
        ~dest_vdi:_ ~snapshot_pairs:_ =
      assert false

    let update_snapshot_info_dest _context ~dbg ~sr ~vdi ~src_vdi:_
        ~snapshot_pairs =
      Server_helpers.exec_with_new_task "SR.update_snapshot_info_dest"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let local_vdis = scan __context ~dbg ~sr in
          let find_sm_vdi ~vdi ~vdi_info_list =
            try List.find (fun x -> x.vdi = vdi) vdi_info_list
            with Not_found ->
              raise (Storage_error (Vdi_does_not_exist (s_of_vdi vdi)))
          in
          let assert_content_ids_match ~vdi_info1 ~vdi_info2 =
            if vdi_info1.content_id <> vdi_info2.content_id then
              raise
                (Storage_error
                   (Content_ids_do_not_match
                      (s_of_vdi vdi_info1.vdi, s_of_vdi vdi_info2.vdi)
                   )
                )
          in
          (* For each (local snapshot vdi, source snapshot vdi) pair:
             					 * - Check that the content_ids are the same
             					 * - Copy snapshot_time from the source VDI to the local VDI
             					 * - Set the local VDI's snapshot_of to vdi
             					 * - Set is_a_snapshot = true for the local snapshot *)
          List.iter
            (fun (local_snapshot, src_snapshot_info) ->
              let local_snapshot_info =
                find_sm_vdi ~vdi:local_snapshot ~vdi_info_list:local_vdis
              in
              assert_content_ids_match ~vdi_info1:local_snapshot_info
                ~vdi_info2:src_snapshot_info ;
              set_snapshot_time __context ~dbg ~sr ~vdi:local_snapshot
                ~snapshot_time:src_snapshot_info.snapshot_time ;
              set_snapshot_of __context ~dbg ~sr ~vdi:local_snapshot
                ~snapshot_of:vdi ;
              set_is_a_snapshot __context ~dbg ~sr ~vdi:local_snapshot
                ~is_a_snapshot:true
            )
            snapshot_pairs
      )
  end

  module VDI = struct
    let for_vdi ~dbg ~sr ~vdi op_name f =
      Server_helpers.exec_with_new_task op_name ~subtask_of:(Ref.of_string dbg)
        (fun __context ->
          let self = find_vdi ~__context sr vdi |> fst in
          Sm.call_sm_vdi_functions ~__context ~vdi:self
            (fun device_config _type sr -> f device_config _type sr self
          )
      )

    let per_host_key ~__context ~prefix =
      let host_uuid =
        Db.Host.get_uuid ~__context ~self:(Helpers.get_localhost ~__context)
      in
      Printf.sprintf "%s-%s" prefix host_uuid

    let read_caching_key ~__context =
      per_host_key ~__context ~prefix:"read-caching-enabled-on"

    let read_caching_reason_key ~__context =
      per_host_key ~__context ~prefix:"read-caching-reason"

    let epoch_begin _context ~dbg ~sr ~vdi ~vm:_ ~persistent:_ =
      with_dbg ~name:"VDI.epoch_begin" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        for_vdi ~dbg ~sr ~vdi "VDI.epoch_begin"
          (fun device_config _type sr self ->
            Sm.vdi_epoch_begin ~dbg device_config _type sr self
        )
      with Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))

    let attach2 _context ~dbg ~dp:_ ~sr ~vdi ~read_write =
      with_dbg ~name:"VDI.attach2" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        let backend =
          for_vdi ~dbg ~sr ~vdi "VDI.attach2"
            (fun device_config _type sr self ->
              let attach_info_v1 =
                Sm.vdi_attach ~dbg device_config _type sr self read_write
              in
              (* Record whether the VDI is benefiting from read caching *)
              Server_helpers.exec_with_new_task "VDI.attach2"
                ~subtask_of:(Ref.of_string dbg) (fun __context ->
                  let read_caching = not attach_info_v1.Smint.o_direct in
                  let on_key = read_caching_key ~__context in
                  let reason_key = read_caching_reason_key ~__context in
                  with_lock vdi_read_caching_m (fun () ->
                      Db.VDI.remove_from_sm_config ~__context ~self ~key:on_key ;
                      Db.VDI.remove_from_sm_config ~__context ~self
                        ~key:reason_key ;
                      Db.VDI.add_to_sm_config ~__context ~self ~key:on_key
                        ~value:(string_of_bool read_caching) ;
                      if not read_caching then
                        Db.VDI.add_to_sm_config ~__context ~self ~key:reason_key
                          ~value:attach_info_v1.Smint.o_direct_reason
                  )
              ) ;
              {
                implementations=
                  (* Use NBD if the config file prescribes it or if there is no other option *)
                  ( match
                      ( !Xapi_globs.prefer_nbd_attach
                      , attach_info_v1.Smint.params
                      )
                    with
                  | true, _ | false, None ->
                      [
                        XenDisk
                          {
                            params= attach_info_v1.Smint.params_nbd
                          ; extra= attach_info_v1.Smint.xenstore_data
                          ; backend_type= "vbd3"
                          }
                      ; Nbd {uri= attach_info_v1.Smint.params_nbd}
                      ]
                  | false, Some params ->
                      [
                        XenDisk
                          {
                            params
                          ; extra= attach_info_v1.Smint.xenstore_data
                          ; backend_type= "vbd3"
                          }
                      ; BlockDevice {path= params}
                      ]
                  )
              }
          )
        in
        with_lock vdi_read_write_m (fun () ->
            Hashtbl.replace vdi_read_write (sr, vdi) read_write
        ) ;
        backend
      with Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))

    let attach3 context ~dbg ~dp ~sr ~vdi ~vm:_ ~read_write =
      with_dbg ~name:"VDI.attach3" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      (*Throw away vm argument as does nothing in SMAPIv1*)
      attach2 context ~dbg ~dp ~sr ~vdi ~read_write

    let attach _ =
      failwith
        "We'll never get here: attach is implemented in \
         Storage_smapiv1_wrapper.Wrapper"

    let activate _context ~dbg ~dp ~sr ~vdi =
      with_dbg ~name:"VDI.activate" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        let read_write =
          with_lock vdi_read_write_m (fun () ->
              match Hashtbl.find_opt vdi_read_write (sr, vdi) with
              | Some x ->
                  x
              | None ->
                  error "VDI.activate: doesn't know if sr:%s vdi:%s is RO or RW"
                    (s_of_sr sr) (s_of_vdi vdi) ;
                  false
          )
        in
        for_vdi ~dbg ~sr ~vdi "VDI.activate" (fun device_config _type sr self ->
            Server_helpers.exec_with_new_task "VDI.activate"
              ~subtask_of:(Ref.of_string dbg) (fun __context ->
                if read_write then
                  Db.VDI.remove_from_other_config ~__context ~self
                    ~key:"content_id"
            ) ;
            (* If the backend doesn't advertise the capability then do nothing *)
            if List.mem_assoc Smint.Vdi_activate (Sm.features_of_driver _type)
            then
              Sm.vdi_activate ~dbg device_config _type sr self read_write
            else
              info "%s sr:%s does not support vdi_activate: doing nothing" dp
                (Ref.string_of sr)
        )
      with Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))

    let activate3 context ~dbg ~dp ~sr ~vdi ~vm:_ =
      with_dbg ~name:"VDI.activate3" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      activate context ~dbg ~dp ~sr ~vdi

    let activate_readonly = activate3

    let deactivate _context ~dbg ~dp ~sr ~vdi ~vm:_ =
      with_dbg ~name:"VDI.deactivate" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        for_vdi ~dbg ~sr ~vdi "VDI.deactivate"
          (fun device_config _type sr self ->
            Server_helpers.exec_with_new_task "VDI.deactivate"
              ~subtask_of:(Ref.of_string dbg) (fun __context ->
                let other_config = Db.VDI.get_other_config ~__context ~self in
                if not (List.mem_assoc "content_id" other_config) then
                  Db.VDI.add_to_other_config ~__context ~self ~key:"content_id"
                    ~value:Uuidx.(to_string (make ()))
            ) ;
            (* If the backend doesn't advertise the capability then do nothing *)
            if List.mem_assoc Smint.Vdi_deactivate (Sm.features_of_driver _type)
            then
              Sm.vdi_deactivate ~dbg device_config _type sr self
            else
              info "%s sr:%s does not support vdi_deactivate: doing nothing" dp
                (Ref.string_of sr)
        )
      with Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))

    let detach _context ~dbg ~dp:_ ~sr ~vdi ~vm:_ =
      with_dbg ~name:"VDI.detach" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        for_vdi ~dbg ~sr ~vdi "VDI.detach" (fun device_config _type sr self ->
            Sm.vdi_detach ~dbg device_config _type sr self ;
            Server_helpers.exec_with_new_task "VDI.detach"
              ~subtask_of:(Ref.of_string dbg) (fun __context ->
                let on_key = read_caching_key ~__context in
                let reason_key = read_caching_reason_key ~__context in
                with_lock vdi_read_caching_m (fun () ->
                    Db.VDI.remove_from_sm_config ~__context ~self ~key:on_key ;
                    Db.VDI.remove_from_sm_config ~__context ~self
                      ~key:reason_key
                )
            )
        ) ;
        with_lock vdi_read_write_m (fun () ->
            Hashtbl.remove vdi_read_write (sr, vdi)
        )
      with Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))

    let epoch_end _context ~dbg ~sr ~vdi ~vm:_ =
      with_dbg ~name:"VDI.epoch_end" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        for_vdi ~dbg ~sr ~vdi "VDI.epoch_end"
          (fun device_config _type sr self ->
            Sm.vdi_epoch_end ~dbg device_config _type sr self
        )
      with Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))

    let require_uuid vdi_info =
      match vdi_info.Smint.vdi_info_uuid with
      | Some uuid ->
          uuid
      | None ->
          failwith "SM backend failed to return <uuid> field"

    let newvdi ~__context vi =
      (* The current backends stash data directly in the db *)
      let uuid = require_uuid vi in
      vdi_info_from_db ~__context (Db.VDI.get_by_uuid ~__context ~uuid)

    let create _context ~dbg ~sr ~(vdi_info : Storage_interface.vdi_info) =
      with_dbg ~name:"VDI.create" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        Server_helpers.exec_with_new_task "VDI.create"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            let sr_uuid = s_of_sr sr in
            let sr = Db.SR.get_by_uuid ~__context ~uuid:sr_uuid in
            let vi =
              (* we want to set vdi_uuid when creating a backup VDI with
                 a specific UUID. SM picks up vdi_uuid instead of creating
                 a new random UUID; Cf. Xapi_vdi.create *)
              let vdi_uuid =
                match vdi_info.uuid with
                | Some uuid when uuid = Uuidx.(Hash.string sr_uuid |> to_string)
                  ->
                    info "%s: creating a backup VDI %s" __FUNCTION__ uuid ;
                    vdi_info.uuid
                | _ ->
                    None
              in
              Sm.call_sm_functions ~__context ~sR:sr (fun device_config _type ->
                  Sm.vdi_create ~dbg ?vdi_uuid device_config _type sr
                    vdi_info.sm_config vdi_info.ty vdi_info.virtual_size
                    vdi_info.name_label vdi_info.name_description
                    vdi_info.metadata_of_pool vdi_info.is_a_snapshot
                    vdi_info.snapshot_time
                    (s_of_vdi vdi_info.snapshot_of)
                    vdi_info.read_only
              )
            in
            newvdi ~__context vi
        )
      with
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | Sm.MasterOnly ->
          redirect sr

    (* A list of keys in sm-config that will be preserved on clone/snapshot *)
    let sm_config_keys_to_preserve_on_clone = ["base_mirror"]

    let snapshot_and_clone call_name call_f is_a_snapshot _context ~dbg ~sr
        ~vdi_info =
      with_dbg ~name:"VDI.snapshot_and_clone" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        Server_helpers.exec_with_new_task call_name
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            let vi =
              for_vdi ~dbg ~sr ~vdi:vdi_info.vdi call_name
                (fun device_config _type sr self ->
                  call_f ~dbg device_config _type vdi_info.sm_config sr self
              )
            in
            (* PR-1255: modify clone, snapshot to take the same parameters as create? *)
            let self, _ =
              find_vdi ~__context sr
                (Storage_interface.Vdi.of_string vi.Smint.vdi_info_location)
            in
            let clonee, _ = find_vdi ~__context sr vdi_info.vdi in
            let content_id =
              try
                List.assoc "content_id"
                  (Db.VDI.get_other_config ~__context ~self:clonee)
              with _ -> Uuidx.(to_string (make ()))
            in
            let snapshot_time = Date.now () in
            Db.VDI.set_name_label ~__context ~self ~value:vdi_info.name_label ;
            Db.VDI.set_name_description ~__context ~self
              ~value:vdi_info.name_description ;
            Db.VDI.set_snapshot_time ~__context ~self ~value:snapshot_time ;
            Db.VDI.set_is_a_snapshot ~__context ~self ~value:is_a_snapshot ;
            Db.VDI.remove_from_other_config ~__context ~self ~key:"content_id" ;
            Db.VDI.add_to_other_config ~__context ~self ~key:"content_id"
              ~value:content_id ;
            debug "copying sm-config" ;
            List.iter
              (fun (key, value) ->
                let preserve =
                  List.mem key sm_config_keys_to_preserve_on_clone
                in
                if preserve then (
                  Db.VDI.remove_from_sm_config ~__context ~self ~key ;
                  Db.VDI.add_to_sm_config ~__context ~self ~key ~value
                )
              )
              vdi_info.sm_config ;
            for_vdi ~dbg ~sr
              ~vdi:(Storage_interface.Vdi.of_string vi.Smint.vdi_info_location)
              "VDI.update" (fun device_config _type sr self ->
                Sm.vdi_update ~dbg device_config _type sr self
            ) ;
            let vdi = vdi_info_from_db ~__context self in
            debug "vdi = %s" (string_of_vdi_info vdi) ;
            vdi
        )
      with
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | Smint.Not_implemented_in_backend ->
          raise (Storage_error (Unimplemented call_name))
      | Sm.MasterOnly ->
          redirect sr

    let snapshot = snapshot_and_clone "VDI.snapshot" Sm.vdi_snapshot true

    let clone = snapshot_and_clone "VDI.clone" Sm.vdi_clone false

    let set_name_label _context ~dbg ~sr ~vdi ~new_name_label =
      with_dbg ~name:"VDI.set_name_label" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      Server_helpers.exec_with_new_task "VDI.set_name_label"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let self, _ = find_vdi ~__context sr vdi in
          Db.VDI.set_name_label ~__context ~self ~value:new_name_label
      )

    let set_name_description _context ~dbg ~sr ~vdi ~new_name_description =
      with_dbg ~name:"VDI.set_name_description" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      Server_helpers.exec_with_new_task "VDI.set_name_description"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let self, _ = find_vdi ~__context sr vdi in
          Db.VDI.set_name_description ~__context ~self
            ~value:new_name_description
      )

    let resize _context ~dbg ~sr ~vdi ~new_size =
      with_dbg ~name:"VDI.resize" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        let vi =
          for_vdi ~dbg ~sr ~vdi "VDI.resize" (fun device_config _type sr self ->
              Sm.vdi_resize ~dbg device_config _type sr self new_size
          )
        in
        Server_helpers.exec_with_new_task "VDI.resize"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            let self, _ =
              find_vdi ~__context sr
                (Storage_interface.Vdi.of_string vi.Smint.vdi_info_location)
            in
            Db.VDI.get_virtual_size ~__context ~self
        )
      with
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | Smint.Not_implemented_in_backend ->
          raise (Storage_error (Unimplemented "VDI.resize"))
      | Sm.MasterOnly ->
          redirect sr

    let destroy _context ~dbg ~sr ~vdi =
      with_dbg ~name:"VDI.destroy" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        for_vdi ~dbg ~sr ~vdi "VDI.destroy" (fun device_config _type sr self ->
            Sm.vdi_delete ~dbg device_config _type sr self
        ) ;
        with_lock vdi_read_write_m (fun () ->
            Hashtbl.remove vdi_read_write (sr, vdi)
        )
      with
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | No_VDI ->
          raise (Storage_error (Vdi_does_not_exist (s_of_vdi vdi)))
      | Sm.MasterOnly ->
          redirect sr

    let stat _context ~dbg ~sr ~vdi =
      with_dbg ~name:"VDI.stat" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        Server_helpers.exec_with_new_task "VDI.stat"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            for_vdi ~dbg ~sr ~vdi "VDI.stat" (fun device_config _type sr self ->
                Sm.vdi_update ~dbg device_config _type sr self ;
                vdi_info_of_vdi_rec __context
                  (Db.VDI.get_record ~__context ~self)
            )
        )
      with e ->
        error "VDI.stat caught: %s" (Printexc.to_string e) ;
        raise (Storage_error (Vdi_does_not_exist (s_of_vdi vdi)))

    let introduce _context ~dbg ~sr ~uuid ~sm_config ~location =
      with_dbg ~name:"VDI.introduce" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        Server_helpers.exec_with_new_task "VDI.introduce"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            let sr = Db.SR.get_by_uuid ~__context ~uuid:(s_of_sr sr) in
            let vi =
              Sm.call_sm_functions ~__context ~sR:sr
                (fun device_config sr_type ->
                  Sm.vdi_introduce ~dbg device_config sr_type sr uuid sm_config
                    location
              )
            in
            newvdi ~__context vi
        )
      with e ->
        error "VDI.introduce caught: %s" (Printexc.to_string e) ;
        raise (Storage_error (Vdi_does_not_exist location))

    let set_persistent _context ~dbg ~sr ~vdi ~persistent =
      with_dbg ~name:"VDI.set_persistent" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        Server_helpers.exec_with_new_task "VDI.set_persistent"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            if not persistent then (
              info
                "VDI.set_persistent: calling VDI.clone and VDI.destroy to make \
                 an empty vhd-leaf" ;
              let new_vdi =
                for_vdi ~dbg ~sr ~vdi "VDI.clone"
                  (fun device_config _type sr self ->
                    let vi = Sm.vdi_clone ~dbg device_config _type [] sr self in
                    Storage_interface.Vdi.of_string vi.Smint.vdi_info_location
                )
              in
              for_vdi ~dbg ~sr ~vdi:new_vdi "VDI.destroy"
                (fun device_config _type sr self ->
                  Sm.vdi_delete ~dbg device_config _type sr self
              )
            )
        )
      with
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | Sm.MasterOnly ->
          redirect sr

    let get_by_name _context ~dbg ~sr ~name =
      with_dbg ~name:"VDI.get_by_name" ~dbg @@ fun di ->
      info "VDI.get_by_name dbg:%s sr:%s name:%s" di.log (s_of_sr sr) name ;
      let dbg = Debug_info.to_string di in
      (* PR-1255: the backend should do this for us *)
      Server_helpers.exec_with_new_task "VDI.get_by_name"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          (* PR-1255: the backend should do this for us *)
          try
            let _, vdi = find_content ~__context ~sr name in
            let vi = vdi_info_of_vdi_rec __context vdi in
            debug "VDI.get_by_name returning successfully" ;
            vi
          with e ->
            error "VDI.get_by_name caught: %s" (Printexc.to_string e) ;
            raise (Storage_error (Vdi_does_not_exist name))
      )

    let set_content_id _context ~dbg ~sr ~vdi ~content_id =
      with_dbg ~name:"VDI.set_content_id" ~dbg @@ fun di ->
      info "VDI.get_by_content dbg:%s sr:%s vdi:%s content_id:%s" di.log
        (s_of_sr sr) (s_of_vdi vdi) content_id ;
      let dbg = Debug_info.to_string di in
      (* PR-1255: the backend should do this for us *)
      Server_helpers.exec_with_new_task "VDI.set_content_id"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let vdi, _ = find_vdi ~__context sr vdi in
          Db.VDI.remove_from_other_config ~__context ~self:vdi ~key:"content_id" ;
          Db.VDI.add_to_other_config ~__context ~self:vdi ~key:"content_id"
            ~value:content_id
      )

    let similar_content _context ~dbg ~sr ~vdi =
      with_dbg ~name:"VDI.similar_content" ~dbg @@ fun di ->
      info "VDI.similar_content dbg:%s sr:%s vdi:%s" di.log (s_of_sr sr)
        (s_of_vdi vdi) ;
      let dbg = Debug_info.to_string di in
      Server_helpers.exec_with_new_task "VDI.similar_content"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          (* PR-1255: the backend should do this for us. *)
          let sr_ref =
            Db.SR.get_by_uuid ~__context
              ~uuid:(Storage_interface.Sr.string_of sr)
          in
          (* Return a nearest-first list of similar VDIs. "near" should mean
             					   "has similar blocks" but we approximate this with distance in the tree *)
          let module StringMap = Map.Make (struct
            type t = string

            let compare = compare
          end) in
          let _vhdparent = "vhd-parent" in
          let open Xapi_database.Db_filter_types in
          let all =
            Db.VDI.get_records_where ~__context
              ~expr:(Eq (Field "SR", Literal (Ref.string_of sr_ref)))
          in
          let locations =
            List.fold_left
              (fun acc (_, vdi_rec) ->
                StringMap.add vdi_rec.API.vDI_location vdi_rec acc
              )
              StringMap.empty all
          in
          (* Compute a map of parent location -> children locations *)
          let children, parents =
            List.fold_left
              (fun (children, parents) (_, vdi_rec) ->
                if List.mem_assoc _vhdparent vdi_rec.API.vDI_sm_config then
                  let me = vdi_rec.API.vDI_location in
                  let parent =
                    List.assoc _vhdparent vdi_rec.API.vDI_sm_config
                  in
                  let other_children =
                    if StringMap.mem parent children then
                      StringMap.find parent children
                    else
                      []
                  in
                  ( StringMap.add parent (me :: other_children) children
                  , StringMap.add me parent parents
                  )
                else
                  (children, parents)
              )
              (StringMap.empty, StringMap.empty)
              all
          in
          let rec explore current_distance acc vdi =
            (* add me *)
            let acc = StringMap.add vdi current_distance acc in
            (* add the parent *)
            let parent =
              if StringMap.mem vdi parents then
                [StringMap.find vdi parents]
              else
                []
            in
            let children =
              if StringMap.mem vdi children then
                StringMap.find vdi children
              else
                []
            in
            List.fold_left
              (fun acc vdi ->
                if not (StringMap.mem vdi acc) then
                  explore (current_distance + 1) acc vdi
                else
                  acc
              )
              acc (parent @ children)
          in
          let module IntMap = Map.Make (struct
            type t = int

            let compare = compare
          end) in
          let invert map =
            StringMap.fold
              (fun vdi n acc ->
                let current =
                  if IntMap.mem n acc then IntMap.find n acc else []
                in
                IntMap.add n (vdi :: current) acc
              )
              map IntMap.empty
          in
          let _, vdi_rec = find_vdi ~__context sr vdi in
          let vdis =
            explore 0 StringMap.empty vdi_rec.API.vDI_location
            |> invert
            |> IntMap.bindings
            |> List.map snd
            |> List.concat
          in
          let vdi_recs = List.map (fun l -> StringMap.find l locations) vdis in
          (* We drop cbt_metadata VDIs that do not have any actual data *)
          let vdi_recs =
            List.filter (fun r -> r.API.vDI_type <> `cbt_metadata) vdi_recs
          in
          List.map (fun x -> vdi_info_of_vdi_rec __context x) vdi_recs
      )

    let compose _context ~dbg ~sr ~vdi1 ~vdi2 =
      with_dbg ~name:"VDI.compose" ~dbg @@ fun di ->
      info "VDI.compose dbg:%s sr:%s vdi1:%s vdi2:%s" di.log (s_of_sr sr)
        (s_of_vdi vdi1) (s_of_vdi vdi2) ;
      let dbg = Debug_info.to_string di in
      try
        Server_helpers.exec_with_new_task "VDI.compose"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            (* This call 'operates' on vdi2 *)
            let vdi1 = find_vdi ~__context sr vdi1 |> fst in
            for_vdi ~dbg ~sr ~vdi:vdi2 "VDI.compose"
              (fun device_config _type sr self ->
                Sm.vdi_compose ~dbg device_config _type sr vdi1 self
            )
        )
      with
      | Smint.Not_implemented_in_backend ->
          raise (Storage_error (Unimplemented "VDI.compose"))
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | No_VDI ->
          raise
            (Storage_error
               (Vdi_does_not_exist (Storage_interface.Vdi.string_of vdi1))
            )
      | Sm.MasterOnly ->
          redirect sr

    let add_to_sm_config _context ~dbg ~sr ~vdi ~key ~value =
      with_dbg ~name:"VDI.add_to_sm_config" ~dbg @@ fun di ->
      info "VDI.add_to_sm_config dbg:%s sr:%s vdi:%s key:%s value:%s" di.log
        (s_of_sr sr) (s_of_vdi vdi) key value ;
      let dbg = Debug_info.to_string di in
      Server_helpers.exec_with_new_task "VDI.add_to_sm_config"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let self = find_vdi ~__context sr vdi |> fst in
          Db.VDI.add_to_sm_config ~__context ~self ~key ~value
      )

    let remove_from_sm_config _context ~dbg ~sr ~vdi ~key =
      with_dbg ~name:"VDI.remove_from_sm_config" ~dbg @@ fun di ->
      info "VDI.remove_from_sm_config dbg:%s sr:%s vdi:%s key:%s" di.log
        (s_of_sr sr) (s_of_vdi vdi) key ;
      let dbg = Debug_info.to_string di in
      Server_helpers.exec_with_new_task "VDI.remove_from_sm_config"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let self = find_vdi ~__context sr vdi |> fst in
          Db.VDI.remove_from_sm_config ~__context ~self ~key
      )

    let get_url _context ~dbg ~sr ~vdi =
      with_dbg ~name:"VDI.get_url" ~dbg @@ fun di ->
      info "VDI.get_url dbg:%s sr:%s vdi:%s" di.log (s_of_sr sr) (s_of_vdi vdi) ;
      let dbg = Debug_info.to_string di in
      (* XXX: PR-1255: tapdisk shouldn't hardcode xapi urls *)
      (* peer_ip/session_ref/vdi_ref *)
      Server_helpers.exec_with_new_task "VDI.get_url"
        ~subtask_of:(Ref.of_string dbg) (fun __context ->
          let ip = Helpers.get_management_ip_addr ~__context |> Option.get in
          let rpc = Helpers.make_rpc ~__context in
          let localhost = Helpers.get_localhost ~__context in
          (* XXX: leaked *)
          let session_ref =
            XenAPI.Session.slave_login ~rpc ~host:localhost
              ~psecret:(Xapi_globs.pool_secret ())
          in
          let vdi, _ = find_vdi ~__context sr vdi in
          Printf.sprintf "%s/%s/%s" ip
            (Ref.string_of session_ref)
            (Ref.string_of vdi)
      )

    let call_cbt_function _context ~f ~f_name ~dbg ~sr ~vdi =
      with_dbg ~name:"VDI.call_cbt_function" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        for_vdi ~dbg ~sr ~vdi f_name (fun device_config _type sr self ->
            f ~dbg device_config _type sr self
        )
      with
      | Smint.Not_implemented_in_backend ->
          raise (Storage_error (Unimplemented f_name))
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | No_VDI ->
          raise
            (Storage_error
               (Vdi_does_not_exist (Storage_interface.Vdi.string_of vdi))
            )
      | Sm.MasterOnly ->
          redirect sr

    let enable_cbt context =
      call_cbt_function context ~f:Sm.vdi_enable_cbt ~f_name:"VDI.enable_cbt"

    let disable_cbt context =
      call_cbt_function context ~f:Sm.vdi_disable_cbt ~f_name:"VDI.disable_cbt"

    let data_destroy context ~dbg ~sr ~vdi =
      call_cbt_function context ~f:Sm.vdi_data_destroy
        ~f_name:"VDI.data_destroy" ~dbg ~sr ~vdi ;
      set_content_id context ~dbg ~sr ~vdi
        ~content_id:"/No content: this is a cbt_metadata VDI/"

    let list_changed_blocks _context ~dbg ~sr ~vdi_from ~vdi_to =
      with_dbg ~name:"VDI.list_changed_blocks" ~dbg @@ fun di ->
      let dbg = Debug_info.to_string di in
      try
        Server_helpers.exec_with_new_task "VDI.list_changed_blocks"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            let vdi_from = find_vdi ~__context sr vdi_from |> fst in
            for_vdi ~dbg ~sr ~vdi:vdi_to "VDI.list_changed_blocks"
              (fun device_config _type sr vdi_to ->
                Sm.vdi_list_changed_blocks ~dbg device_config _type sr ~vdi_from
                  ~vdi_to
            )
        )
      with
      | Smint.Not_implemented_in_backend ->
          raise (Storage_error (Unimplemented "VDI.list_changed_blocks"))
      | Api_errors.Server_error (code, params) ->
          raise (Storage_error (Backend_error (code, params)))
      | Sm.MasterOnly ->
          redirect sr
  end

  let get_by_name _context ~dbg:_ ~name:_ = assert false

  module DATA = struct
    let copy_into _context ~dbg:_ ~sr:_ ~vdi:_ ~url:_ ~dest:_ ~dest_vdi:_
        ~verify_dest:_ =
      assert false

    let copy _context ~dbg:_ ~sr:_ ~vdi:_ ~dp:_ ~url:_ ~dest:_ ~verify_dest:_ =
      assert false

    module MIRROR = struct
      let start _context ~dbg:_ ~sr:_ ~vdi:_ ~dp:_ ~url:_ ~dest:_ ~verify_dest:_
          =
        assert false

      let stop _context ~dbg:_ ~id:_ = assert false

      let list _context ~dbg:_ = assert false

      let stat _context ~dbg:_ ~id:_ = assert false

      let receive_start _context ~dbg:_ ~sr:_ ~vdi_info:_ ~id:_ ~similar:_ =
        assert false

      let receive_finalize _context ~dbg:_ ~id:_ = assert false

      let receive_cancel _context ~dbg:_ ~id:_ = assert false
    end
  end

  module Policy = struct
    let get_backend_vm _context ~dbg:_ ~vm:_ ~sr:_ ~vdi:_ = assert false
  end

  module TASK = struct
    let stat _context ~dbg:_ ~task:_ = assert false

    let destroy _context ~dbg:_ ~task:_ = assert false

    let cancel _context ~dbg:_ ~task:_ = assert false

    let list _context ~dbg:_ = assert false
  end

  module UPDATES = struct
    let get _context ~dbg:_ ~from:_ ~timeout:_ = assert false
  end
end
