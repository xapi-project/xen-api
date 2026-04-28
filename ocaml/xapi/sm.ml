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
(** Storage manager SMAPIv1 interface
 * @group Storage
*)

open Smint
open Printf

module D = Debug.Make (struct let name = "sm" end)

open D

(*****************************************************************************)
let driver_info_cache : (string, sr_driver_info) Hashtbl.t = Hashtbl.create 10

exception Unknown_driver of string

exception MasterOnly

let with_dbg ~name ~dbg f = Debug_info.with_dbg ~module_name:"SM" ~name ~dbg f

let supported_drivers () =
  Hashtbl.fold (fun name _ acc -> name :: acc) driver_info_cache []

(** Scans the plugin directory and registers everything it finds there *)
let register ~__context () =
  let dbg = Context.string_of_task_and_tracing __context in
  with_dbg ~name:"register" ~dbg @@ fun di ->
  let dbg = Debug_info.to_string di in
  let add_entry driver info =
    let name = String.lowercase_ascii driver in
    Hashtbl.replace driver_info_cache name info
  in
  Sm_exec.get_supported ~dbg add_entry ;
  info "Registered SMAPIv1 plugins: %s"
    (String.concat ", " (supported_drivers ()))

let info_of_driver (name : string) =
  let name = String.lowercase_ascii name in
  match Hashtbl.find_opt driver_info_cache name with
  | Some x ->
      x
  | None ->
      raise (Unknown_driver name)

let features_of_driver (name : string) =
  (info_of_driver name).sr_driver_features

let driver_filename driver =
  let info = info_of_driver driver in
  info.sr_driver_filename

(*****************************************************************************)

let debug operation driver msg = debug "SM %s %s %s" driver operation msg

let srmaster_only (_, dconf) =
  let is_srmaster =
    try List.assoc "SRmaster" dconf = "true" with _ -> false
  in
  if not is_srmaster then (
    warn "srmaster_only: Raising MasterOnly exception" ;
    raise MasterOnly
  )

let sr_create ~dbg dconf driver sr size =
  with_dbg ~dbg ~name:"sr_create" @@ fun di ->
  let dbg = Debug_info.to_string di in
  let call =
    Sm_exec.make_call ~sr_ref:sr dconf "sr_create" [Int64.to_string size]
  in
  debug "sr_create" driver (sprintf "sr=%s size=%Ld" (Ref.string_of sr) size) ;
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let sr_delete ~dbg dconf driver sr =
  with_dbg ~dbg ~name:"sr_delete" @@ fun di ->
  let dbg = Debug_info.to_string di in
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_delete" [] in
  debug "sr_delete" driver (sprintf "sr=%s" (Ref.string_of sr)) ;
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

(* Mutex for sr_attach, sr_detach, and sr_probe *)
let serialize_attach_detach =
  Locking_helpers.Named_mutex.create "sr_attach/detach"

let sr_attach ~dbg dconf driver sr =
  with_dbg ~dbg ~name:"sr_attach" @@ fun di ->
  let dbg = Debug_info.to_string di in
  Locking_helpers.Named_mutex.execute serialize_attach_detach (fun () ->
      debug "sr_attach" driver (sprintf "sr=%s" (Ref.string_of sr)) ;
      let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_attach" [] in
      Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)
  )

let sr_detach ~dbg dconf driver sr =
  with_dbg ~dbg ~name:"sr_detach" @@ fun di ->
  let dbg = Debug_info.to_string di in
  Locking_helpers.Named_mutex.execute serialize_attach_detach (fun () ->
      debug "sr_detach" driver (sprintf "sr=%s" (Ref.string_of sr)) ;
      let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_detach" [] in
      Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)
  )

let sr_probe ~dbg dconf driver sr_sm_config =
  with_dbg ~dbg ~name:"sr_probe" @@ fun di ->
  let dbg = Debug_info.to_string di in
  if Feature.(has_capability Sr_probe (features_of_driver driver)) then
    Locking_helpers.Named_mutex.execute serialize_attach_detach (fun () ->
        debug "sr_probe" driver
          (sprintf "sm_config=[%s]"
             (String.concat "; "
                (List.map (fun (k, v) -> k ^ "=" ^ v) sr_sm_config)
             )
          ) ;
        let call = Sm_exec.make_call ~sr_sm_config dconf "sr_probe" [] in
        (* sr_probe returns an XML document marshalled within an XMLRPC string *)
        XMLRPC.From.string
          (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)
    )
  else
    raise
      (Api_errors.Server_error
         ( Api_errors.sr_backend_failure
         , ["Operation 'sr_probe' not supported by this SR type"; ""; ""]
         )
      )

let sr_scan ~dbg dconf driver sr =
  with_dbg ~dbg ~name:"sr_scan" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "sr_scan" driver (sprintf "sr=%s" (Ref.string_of sr)) ;
  srmaster_only dconf ;
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_scan" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let sr_update ~dbg dconf driver sr =
  with_dbg ~dbg ~name:"sr_update" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "sr_update" driver (sprintf "sr=%s" (Ref.string_of sr)) ;
  let call = Sm_exec.make_call ~sr_ref:sr dconf "sr_update" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_create ~dbg ?vdi_uuid dconf driver sr sm_config vdi_type size name_label
    name_description metadata_of_pool is_a_snapshot snapshot_time snapshot_of
    read_only =
  with_dbg ~dbg ~name:"vdi_create" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_create" driver
    (sprintf "sr=%s sm_config=[%s] type=[%s] size=%Ld" (Ref.string_of sr)
       (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) sm_config))
       vdi_type size
    ) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ?vdi_uuid ~sr_ref:sr ~vdi_sm_config:sm_config ~vdi_type
      dconf "vdi_create"
      [
        sprintf "%Lu" size
      ; name_label
      ; name_description
      ; metadata_of_pool
      ; string_of_bool is_a_snapshot
      ; snapshot_time
      ; snapshot_of
      ; string_of_bool read_only
      ]
  in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_update ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_update" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_update" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_update" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_introduce ~dbg dconf driver sr new_uuid sm_config location =
  with_dbg ~dbg ~name:"vdi_introduce" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_introduce" driver
    (sprintf "sr=%s new_uuid=%s sm_config=[%s] location=%s" (Ref.string_of sr)
       new_uuid
       (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) sm_config))
       location
    ) ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_location:location ~vdi_sm_config:sm_config
      ~new_uuid dconf "vdi_introduce" []
  in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_delete ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_delete" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_delete" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  srmaster_only dconf ;
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_delete" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_attach ~dbg dconf driver sr vdi writable =
  with_dbg ~dbg ~name:"vdi_attach" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_attach" driver
    (sprintf "sr=%s vdi=%s writable=%b" (Ref.string_of sr) (Ref.string_of vdi)
       writable
    ) ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_attach"
      [sprintf "%b" writable]
  in
  let result = Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call in
  Sm_exec.parse_attach_result result

let vdi_detach ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_detach" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_detach" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  let call = Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_detach" [] in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_activate ~dbg dconf driver sr vdi writable =
  with_dbg ~dbg ~name:"vdi_activate" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_activate" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_activate"
      [sprintf "%b" writable]
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_deactivate ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_deactivate" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_deactivate" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_deactivate" []
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_snapshot ~dbg dconf driver driver_params sr vdi =
  with_dbg ~dbg ~name:"vdi_snapshot" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_snapshot" driver
    (sprintf "sr=%s vdi=%s driver_params=[%s]" (Ref.string_of sr)
       (Ref.string_of vdi)
       (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) driver_params))
    ) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi ~driver_params dconf
      "vdi_snapshot" []
  in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_clone ~dbg dconf driver driver_params sr vdi =
  with_dbg ~dbg ~name:"vdi_clone" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_clone" driver
    (sprintf "sr=%s vdi=%s driver_params=[%s]" (Ref.string_of sr)
       (Ref.string_of vdi)
       (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) driver_params))
    ) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi ~driver_params dconf "vdi_clone"
      []
  in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_resize ~dbg dconf driver sr vdi newsize =
  with_dbg ~dbg ~name:"vdi_resize" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_resize" driver
    (sprintf "sr=%s vdi=%s newsize=%Ld" (Ref.string_of sr) (Ref.string_of vdi)
       newsize
    ) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_resize"
      [sprintf "%Lu" newsize]
  in
  Sm_exec.parse_vdi_info (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_generate_config ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_generate_config" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_generate_config" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_generate_config" []
  in
  Sm_exec.parse_string (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_compose ~dbg dconf driver sr vdi1 vdi2 =
  with_dbg ~dbg ~name:"vdi_compose" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_compose" driver
    (sprintf "sr=%s vdi1=%s vdi2=%s" (Ref.string_of sr) (Ref.string_of vdi1)
       (Ref.string_of vdi2)
    ) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi2 dconf "vdi_compose"
      [Ref.string_of vdi1]
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_epoch_begin ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_epoch_begin" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_epoch_begin" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_epoch_begin" []
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_epoch_end ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_epoch_end" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_epoch_end" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_epoch_end" []
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_enable_cbt ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_enable_cbt" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_enable_cbt" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_enable_cbt" []
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_disable_cbt ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_disable_cbt" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_disable_cbt" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_disable_cbt" []
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_data_destroy ~dbg dconf driver sr vdi =
  with_dbg ~dbg ~name:"vdi_data_destroy" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_data_destroy" driver
    (sprintf "sr=%s vdi=%s" (Ref.string_of sr) (Ref.string_of vdi)) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi dconf "vdi_data_destroy" []
  in
  Sm_exec.parse_unit (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let vdi_list_changed_blocks ~dbg dconf driver sr ~vdi_from ~vdi_to =
  with_dbg ~dbg ~name:"vdi_list_changed_blocks" @@ fun di ->
  let dbg = Debug_info.to_string di in
  debug "vdi_list_changed_blocks" driver
    (sprintf "sr=%s vdi_from=%s vdi_to=%s" (Ref.string_of sr)
       (Ref.string_of vdi_from) (Ref.string_of vdi_to)
    ) ;
  srmaster_only dconf ;
  let call =
    Sm_exec.make_call ~sr_ref:sr ~vdi_ref:vdi_from dconf
      "vdi_list_changed_blocks"
      [Ref.string_of vdi_to]
  in
  Sm_exec.parse_string (Sm_exec.exec_xmlrpc ~dbg (driver_filename driver) call)

let session_has_internal_sr_access ~__context ~sr =
  let session_id = Context.get_session_id __context in
  (* XXX: need to move this somewhere else eventually *)
  let other_config = Db.Session.get_other_config ~__context ~self:session_id in
  List.mem_assoc Xapi_globs._sm_session other_config
  && List.assoc Xapi_globs._sm_session other_config = Ref.string_of sr

let assert_session_has_internal_sr_access ~__context ~sr =
  if not (session_has_internal_sr_access ~__context ~sr) then
    raise (Api_errors.Server_error (Api_errors.permission_denied, [""]))

(*****************************************************************************)
(* Higher-level functions                                                    *)

let get_my_pbd_for_sr __context sr_id =
  let me = Helpers.get_localhost ~__context in
  let pbd_ref_and_record =
    let open Xapi_database.Db_filter_types in
    Db.PBD.get_records_where ~__context
      ~expr:
        (And
           ( Eq (Field "host", Literal (Ref.string_of me))
           , Eq (Field "SR", Literal (Ref.string_of sr_id))
           )
        )
  in
  match pbd_ref_and_record with
  | [] ->
      raise
        (Api_errors.Server_error (Api_errors.sr_no_pbds, [Ref.string_of sr_id]))
  | x :: _ ->
      x

let assert_pbd_is_plugged ~__context ~sr =
  let _, pbd_r = get_my_pbd_for_sr __context sr in
  if not pbd_r.API.pBD_currently_attached then
    raise (Api_errors.Server_error (Api_errors.sr_no_pbds, [Ref.string_of sr]))

let sm_master x = ("SRmaster", string_of_bool x)

(* Internal only function - use 'call_sm_functions' and 'call_sm_vdi_functions' *)
let __get_my_devconf_for_sr __context sr_id =
  let srmaster = Helpers.i_am_srmaster ~__context ~sr:sr_id in
  let _, pbd = get_my_pbd_for_sr __context sr_id in
  sm_master srmaster :: pbd.API.pBD_device_config

(** Make it easier to call SM backend functions on an SR *)
let call_sm_functions ~__context ~sR f =
  let srtype = Db.SR.get_type ~__context ~self:sR
  and srconf = __get_my_devconf_for_sr __context sR in
  let subtask_of = Some (Context.get_task_id __context) in
  f (subtask_of, srconf) srtype

(** Make it easier to call SM backend functions on a VDI directly *)
let call_sm_vdi_functions ~__context ~vdi f =
  let sr = Db.VDI.get_SR ~__context ~self:vdi in
  let srtype = Db.SR.get_type ~__context ~self:sr
  and srconf = __get_my_devconf_for_sr __context sr in
  let subtask_of = Some (Context.get_task_id __context) in
  f (subtask_of, srconf) srtype sr
