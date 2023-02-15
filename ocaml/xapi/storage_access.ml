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

module Date = Xapi_stdext_date.Date
module Listext = Xapi_stdext_std.Listext

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

module XenAPI = Client.Client
open Storage_interface

module D = Debug.Make (struct let name = "storage_access" end)

open D

let s_of_vdi = Vdi.string_of

let s_of_sr = Sr.string_of

let transform_storage_exn f =
  try f () with
  | Storage_error (Backend_error (code, params)) as e ->
      Backtrace.reraise e (Api_errors.Server_error (code, params))
  | Storage_error (Backend_error_with_backtrace (code, backtrace :: params)) as
    e ->
      let backtrace = Backtrace.Interop.of_json "SM" backtrace in
      Backtrace.add e backtrace ;
      Backtrace.reraise e (Api_errors.Server_error (code, params))
  | Api_errors.Server_error _ as e ->
      raise e
  | Storage_error (No_storage_plugin_for_sr sr) as e ->
      Server_helpers.exec_with_new_task "transform_storage_exn"
        (fun __context ->
          let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
          Backtrace.reraise e
            (Api_errors.Server_error
               (Api_errors.sr_not_attached, [Ref.string_of sr])
            )
      )
  | e ->
      Backtrace.reraise e
        (Api_errors.Server_error
           (Api_errors.internal_error, [Printexc.to_string e])
        )

(* Start a set of servers for all SMAPIv1 plugins *)
let start_smapiv1_servers () =
  let drivers = Sm.supported_drivers () in
  List.iter
    (fun ty ->
      let path = !Storage_interface.default_path ^ ".d/" ^ ty in
      let queue_name = !Storage_interface.queue_name ^ "." ^ ty in
      let module S = Storage_smapiv1_wrapper.Server in
      let s = Xcp_service.make ~path ~queue_name ~rpc_fn:S.process () in
      let (_ : Thread.t) =
        Thread.create (fun () -> Xcp_service.serve_forever s) ()
      in
      ()
    )
    drivers

let make_service uuid ty =
  {
    System_domains.uuid
  ; ty= Constants._SM
  ; instance= ty
  ; url=
      Constants.path
        [Constants._services; Constants._driver; uuid; Constants._SM; ty]
  }

let check_queue_exists queue_name =
  let t =
    Xcp_client.(
      get_ok
        (Message_switch_unix.Protocol_unix.Client.connect ~switch:!switch_path
           ()
        )
    )
  in
  let results =
    match
      Message_switch_unix.Protocol_unix.Client.list ~t
        ~prefix:!Storage_interface.queue_name
        ~filter:`Alive ()
    with
    | Ok list ->
        list
    | _ ->
        failwith "Failed to contact switch"
    (* Shouldn't ever happen *)
  in
  if not (List.mem queue_name results) then
    let prefix_len = String.length !Storage_interface.queue_name + 1 in
    let driver =
      String.sub queue_name prefix_len (String.length queue_name - prefix_len)
    in
    raise Api_errors.(Server_error (sr_unknown_driver, [driver]))

(* RPC calls done during startup and received through remote interface:
   there is no need to redirect here, let the caller handle it.
   The destination uri needs to be local as [xml_http_rpc] doesn't support https calls,
   only file and http.
   Cross-host https calls are only supported by XMLRPC_protocol.rpc
*)
let external_rpc queue_name uri =
  let open Xcp_client in
  if !use_switch then check_queue_exists queue_name ;
  fun call ->
    if !use_switch then
      json_switch_rpc queue_name call
    else
      xml_http_rpc ~srcstr:(get_user_agent ()) ~dststr:queue_name uri call

(* Internal exception, never escapes the module *)
exception Message_switch_failure

(* We have to be careful in this function, because an exception raised from
   here will cause the startup sequence to fail *)

(** Synchronise the SM table with the SMAPIv1 plugins on the disk and the SMAPIv2
    plugins mentioned in the configuration file whitelist. *)
let on_xapi_start ~__context =
  let existing =
    List.map
      (fun (rf, rc) -> (rc.API.sM_type, (rf, rc)))
      (Db.SM.get_all_records ~__context)
  in
  let explicitly_configured_drivers =
    List.filter_map
      (function `Sm x -> Some x | _ -> None)
      !Xapi_globs.sm_plugins
  in
  let smapiv1_drivers = Sm.supported_drivers () in
  let configured_drivers = explicitly_configured_drivers @ smapiv1_drivers in
  let in_use_drivers =
    List.map (fun (_, rc) -> rc.API.sR_type) (Db.SR.get_all_records ~__context)
  in
  let to_keep = configured_drivers @ in_use_drivers in
  (* The SMAPIv2 drivers we know about *)
  let smapiv2_drivers = Listext.List.set_difference to_keep smapiv1_drivers in
  (* Query the message switch to detect running SMAPIv2 plugins. *)
  let running_smapiv2_drivers =
    if !Xcp_client.use_switch then (
      try
        let open Message_switch_unix.Protocol_unix in
        let ( >>| ) result f =
          match Client.error_to_msg result with
          | Error (`Msg x) ->
              error "Error %s while querying message switch queues" x ;
              raise Message_switch_failure
          | Ok x ->
              f x
        in
        Client.connect ~switch:!Xcp_client.switch_path () >>| fun t ->
        Client.list ~t ~prefix:!Storage_interface.queue_name ~filter:`Alive ()
        >>| fun running_smapiv2_driver_queues ->
        running_smapiv2_driver_queues
        (* The results include the prefix itself, but that is the main storage
           queue, we don't need it *)
        |> List.filter (( <> ) !Storage_interface.queue_name)
        |> List.map (fun driver ->
               (* Get the last component of the queue name: org.xen.xapi.storage.sr_type -> sr_type *)
               (* split_on_char returns a non-empty list *)
               String.split_on_char '.' driver |> List.rev |> List.hd
           )
      with
      | Message_switch_failure ->
          [] (* no more logging *)
      | e ->
          error "Unexpected error querying the message switch: %s"
            (Printexc.to_string e) ;
          Debug.log_backtrace e (Backtrace.get e) ;
          []
    ) else
      smapiv2_drivers
  in
  (* Add all the running SMAPIv2 drivers *)
  let to_keep = to_keep @ running_smapiv2_drivers in
  (* Delete all records which aren't configured or in-use *)
  List.iter
    (fun ty ->
      info
        "Unregistering SM plugin %s since not in the whitelist and not in-use"
        ty ;
      let self, _ = List.assoc ty existing in
      try Db.SM.destroy ~__context ~self with _ -> ()
    )
    (Listext.List.set_difference (List.map fst existing) to_keep) ;

  (* Synchronize SMAPIv1 plugins *)

  (* Create all missing SMAPIv1 plugins *)
  List.iter
    (fun ty ->
      let query_result =
        Sm.info_of_driver ty |> Smint.query_result_of_sr_driver_info
      in
      Xapi_sm.create_from_query_result ~__context query_result
    )
    (Listext.List.set_difference smapiv1_drivers (List.map fst existing)) ;
  (* Update all existing SMAPIv1 plugins *)
  List.iter
    (fun ty ->
      let query_result =
        Sm.info_of_driver ty |> Smint.query_result_of_sr_driver_info
      in
      Xapi_sm.update_from_query_result ~__context (List.assoc ty existing)
        query_result
    )
    (Listext.List.intersect smapiv1_drivers (List.map fst existing)) ;

  (* Synchronize SMAPIv2 plugins *)

  (* Create all missing SMAPIv2 plugins *)
  let with_query_result ty f =
    let query ty =
      let queue_name = !Storage_interface.queue_name ^ "." ^ ty in
      let uri () = Storage_interface.uri () ^ ".d/" ^ ty in
      let rpc = external_rpc queue_name uri in
      let module C = Storage_interface.StorageAPI (Idl.Exn.GenClient (struct
        let rpc = rpc
      end)) in
      let dbg = Context.string_of_task __context in
      C.Query.query dbg
    in
    log_and_ignore_exn (fun () ->
        let query_result = query ty in
        f query_result
    )
  in
  List.iter
    (fun ty ->
      with_query_result ty (Xapi_sm.create_from_query_result ~__context)
    )
    (Listext.List.set_difference running_smapiv2_drivers (List.map fst existing)) ;
  (* Update all existing SMAPIv2 plugins *)
  List.iter
    (fun ty ->
      with_query_result ty
        (Xapi_sm.update_from_query_result ~__context (List.assoc ty existing))
    )
    (Listext.List.intersect running_smapiv2_drivers (List.map fst existing))

let bind ~__context ~pbd =
  (* Start the VM if necessary, record its uuid *)
  let driver = System_domains.storage_driver_domain_of_pbd ~__context ~pbd in
  if Db.VM.get_power_state ~__context ~self:driver = `Halted then (
    info "PBD %s driver domain %s is offline: starting" (Ref.string_of pbd)
      (Ref.string_of driver) ;
    try
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          XenAPI.VM.start ~rpc ~session_id ~vm:driver ~start_paused:false
            ~force:false
      )
    with
    | Api_errors.Server_error (code, params)
    when code = Api_errors.vm_bad_power_state
    ->
      error "Caught VM_BAD_POWER_STATE [ %s ]" (String.concat "; " params)
    (* ignore for now *)
  ) ;
  let uuid = Db.VM.get_uuid ~__context ~self:driver in
  let sr = Db.PBD.get_SR ~__context ~self:pbd in
  let ty = Db.SR.get_type ~__context ~self:sr in
  let sr = Db.SR.get_uuid ~__context ~self:sr in
  let queue_name = !Storage_interface.queue_name ^ "." ^ ty in
  let uri () = Storage_interface.uri () ^ ".d/" ^ ty in
  let rpc = external_rpc queue_name uri in
  let service = make_service uuid ty in
  System_domains.register_service service queue_name ;
  let module Client = Storage_interface.StorageAPI (Idl.Exn.GenClient (struct
    let rpc = rpc
  end)) in
  let dbg = Context.string_of_task __context in
  let info = Client.Query.query dbg in
  Storage_mux.register (Storage_interface.Sr.of_string sr) rpc uuid info ;
  info

let unbind ~__context ~pbd =
  let driver = System_domains.storage_driver_domain_of_pbd ~__context ~pbd in
  let uuid = Db.VM.get_uuid ~__context ~self:driver in
  let sr = Db.PBD.get_SR ~__context ~self:pbd in
  let ty = Db.SR.get_type ~__context ~self:sr in
  let sr = Db.SR.get_uuid ~__context ~self:sr in
  info "SR %s will nolonger be implemented by VM %s" sr (Ref.string_of driver) ;
  Storage_mux.unregister (Storage_interface.Sr.of_string sr) ;
  let service = make_service uuid ty in
  System_domains.unregister_service service

(* Internal SM calls: need to handle redirection, we are the toplevel caller.
   The SM can decide that a call needs to be run elsewhere, e.g.
   for a SMAPIv3 plugin the snapshot should be run on the node that has the VDI activated.
*)
let rpc =
  let srcstr = Xcp_client.get_user_agent () in
  let original = Storage_mux.Server.process in
  let redirect_to_ip =
    Storage_utils.intra_pool_rpc_of_ip ~srcstr ~dststr:"smapiv2"
  in
  Storage_utils.redirectable_rpc ~original ~redirect_to_ip

module Client = StorageAPI (Idl.Exn.GenClient (struct let rpc = rpc end))

let print_delta d =
  debug "Received update: %s"
    (Jsonrpc.to_string (Storage_interface.Dynamic.rpc_of_id d))

let event_wait dbg p =
  let finished = ref false in
  let event_id = ref "" in
  while not !finished do
    debug "Calling UPDATES.get %s %s 30" dbg !event_id ;
    let deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
    List.iter (fun d -> print_delta d) deltas ;
    event_id := next_id ;
    List.iter (fun d -> if p d then finished := true) deltas
  done

let task_ended dbg id =
  match (Client.TASK.stat dbg id).Task.state with
  | Task.Completed _ | Task.Failed _ ->
      true
  | Task.Pending _ ->
      false

let success_task dbg id =
  let t = Client.TASK.stat dbg id in
  Client.TASK.destroy dbg id ;
  match t.Task.state with
  | Task.Completed _ ->
      t
  | Task.Failed x -> (
    match
      Rpcmarshal.unmarshal Storage_interface.Errors.error.Rpc.Types.ty x
    with
    | Ok e ->
        raise (Storage_error e)
    | Error (`Msg m) ->
        failwith
          (Printf.sprintf "Error unmarshalling exception from remote: %s" m)
  )
  | Task.Pending _ ->
      failwith "task pending"

let wait_for_task dbg id =
  debug "Waiting for task id=%s to finish" id ;
  let finished = function
    | Dynamic.Task id' ->
        id = id' && task_ended dbg id
    | _ ->
        false
  in
  event_wait dbg finished ; id

let vdi_of_task _dbg t =
  match t.Task.state with
  | Task.Completed {Task.result= Some (Vdi_info v); _} ->
      v
  | Task.Completed _ ->
      failwith "Runtime type error in vdi_of_task"
  | _ ->
      failwith "Task not completed"

let mirror_of_task _dbg t =
  match t.Task.state with
  | Task.Completed {Task.result= Some (Mirror_id i); _} ->
      i
  | Task.Completed _ ->
      failwith "Runtime type error in mirror_of_task"
  | _ ->
      failwith "Task not complete"

let progress_map_tbl = Hashtbl.create 10

let mirror_task_tbl = Hashtbl.create 10

let progress_map_m = Mutex.create ()

let add_to_progress_map f id =
  with_lock progress_map_m (fun () -> Hashtbl.add progress_map_tbl id f) ;
  id

let remove_from_progress_map id =
  with_lock progress_map_m (fun () -> Hashtbl.remove progress_map_tbl id) ;
  id

let get_progress_map id =
  with_lock progress_map_m (fun () ->
      try Hashtbl.find progress_map_tbl id with _ -> fun x -> x
  )

let register_mirror __context mid =
  let task = Context.get_task_id __context in
  debug "Registering mirror id %s with task %s" mid (Ref.string_of task) ;
  with_lock progress_map_m (fun () -> Hashtbl.add mirror_task_tbl mid task) ;
  mid

let unregister_mirror mid =
  with_lock progress_map_m (fun () -> Hashtbl.remove mirror_task_tbl mid) ;
  mid

let get_mirror_task mid =
  with_lock progress_map_m (fun () -> Hashtbl.find mirror_task_tbl mid)

exception Not_an_sm_task

let wrap id = TaskHelper.Sm id

let unwrap x = match x with TaskHelper.Sm id -> id | _ -> raise Not_an_sm_task

let register_task __context id =
  TaskHelper.register_task __context (wrap id) ;
  id

let unregister_task __context id =
  TaskHelper.unregister_task __context (wrap id) ;
  id

let update_task ~__context id =
  try
    let self = TaskHelper.id_to_task_exn (TaskHelper.Sm id) in
    (* throws Not_found *)
    let dbg = Context.string_of_task __context in
    let task_t = Client.TASK.stat dbg id in
    let map = get_progress_map id in
    match task_t.Task.state with
    | Task.Pending x ->
        Db.Task.set_progress ~__context ~self ~value:(map x)
    | _ ->
        ()
  with
  | Not_found ->
      (* Since this is called on all tasks, possibly after the task has been
         		   destroyed, it's safe to ignore a Not_found exception here. *)
      ()
  | e ->
      error "storage event: Caught %s while updating task" (Printexc.to_string e)

let update_mirror ~__context id =
  try
    let dbg = Context.string_of_task __context in
    let m = Client.DATA.MIRROR.stat dbg id in
    if m.Mirror.failed then
      debug "Mirror %s has failed" id ;
    let task = get_mirror_task id in
    debug "Mirror associated with task: %s" (Ref.string_of task) ;
    (* Just to get a nice error message *)
    Db.Task.remove_from_other_config ~__context ~self:task ~key:"mirror_failed" ;
    Db.Task.add_to_other_config ~__context ~self:task ~key:"mirror_failed"
      ~value:(s_of_vdi m.Mirror.source_vdi) ;
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        XenAPI.Task.cancel ~rpc ~session_id ~task
    )
  with
  | Not_found ->
      debug "Couldn't find mirror id: %s" id
  | Storage_error (Does_not_exist _) ->
      ()
  | e ->
      error "storage event: Caught %s while updating mirror"
        (Printexc.to_string e)

let rec events_watch ~__context from =
  let dbg = Context.string_of_task __context in
  let events, next = Client.UPDATES.get dbg from None in
  let open Dynamic in
  List.iter
    (function
      | Task id ->
          debug "sm event on Task %s" id ;
          update_task ~__context id
      | Vdi vdi ->
          debug "sm event on VDI %s: ignoring" (s_of_vdi vdi)
      | Dp dp ->
          debug "sm event on DP %s: ignoring" dp
      | Mirror id ->
          debug "sm event on mirror: %s" id ;
          update_mirror ~__context id
      )
    events ;
  events_watch ~__context next

let events_from_sm () =
  ignore
    (Thread.create
       (fun () ->
         Server_helpers.exec_with_new_task "sm_events" (fun __context ->
             while true do
               try events_watch ~__context ""
               with e ->
                 error "event thread caught: %s" (Printexc.to_string e) ;
                 Thread.delay 10.
             done
         )
       )
       ()
    )

let start () =
  let s =
    Xcp_service.make ~path:Xapi_globs.storage_unix_domain_socket
      ~queue_name:"org.xen.xapi.storage" ~rpc_fn:Storage_mux.Server.process ()
  in
  info "Started service on org.xen.xapi.storage" ;
  let (_ : Thread.t) =
    Thread.create (fun () -> Xcp_service.serve_forever s) ()
  in
  ()

(** [datapath_of_vbd domid userdevice] returns the name of the datapath which corresponds
    to device [userdevice] on domain [domid] *)
let datapath_of_vbd ~domid ~device = Printf.sprintf "vbd/%d/%s" domid device

let presentative_datapath_of_vbd ~__context ~vm ~vdi =
  try
    let vbds = Db.VDI.get_VBDs ~__context ~self:vdi in
    let vbd =
      List.find (fun self -> Db.VBD.get_VM ~__context ~self = vm) vbds
    in
    let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
    let device = Db.VBD.get_device ~__context ~self:vbd in
    if domid < 0 || device = "" then raise Not_found ;
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
  let has_qemu =
    Helpers.has_qemu ~__context ~self:(Db.VBD.get_VM ~__context ~self:vbd)
  in
  let dbg = Context.get_task_id __context in
  let device_number = Device_number.of_string has_qemu userdevice in
  let device = Device_number.to_linux_device device_number in
  let dp = datapath_of_vbd ~domid ~device in
  ( rpc
  , Ref.string_of dbg
  , dp
  , Storage_interface.Sr.of_string (Db.SR.get_uuid ~__context ~self:sr)
  , Storage_interface.Vdi.of_string location
  )

(** [on_vdi __context vbd domid f] calls [f rpc dp sr vdi] which is
    useful for executing Storage_interface.Client.VDI functions  *)
let on_vdi ~__context ~vbd ~domid f =
  let rpc, dbg, dp, sr, vdi = of_vbd ~__context ~vbd ~domid in
  let module C = Storage_interface.StorageAPI (Idl.Exn.GenClient (struct
    let rpc = rpc
  end)) in
  let dp = C.DP.create dbg dp in
  transform_storage_exn (fun () -> f rpc dbg dp sr vdi)

let reset ~__context ~vm =
  let dbg = Context.get_task_id __context in
  transform_storage_exn (fun () ->
      Option.iter
        (fun pbd ->
          let sr_uuid =
            Db.SR.get_uuid ~__context ~self:(Db.PBD.get_SR ~__context ~self:pbd)
          in
          let sr = Storage_interface.Sr.of_string sr_uuid in
          info "Resetting all state associated with SR: %s" sr_uuid ;
          Client.SR.reset (Ref.string_of dbg) sr ;
          Db.PBD.set_currently_attached ~__context ~self:pbd ~value:false
        )
        (System_domains.pbd_of_vm ~__context ~vm)
  )

(** [attach_and_activate __context vbd domid f] calls [f attach_info] where
    [attach_info] is the result of attaching a VDI which is also activated.
    This should be used everywhere except the migrate code, where we want fine-grained
    control of the ordering of attach/activate/deactivate/detach *)
let attach_and_activate ~__context ~vbd ~domid f =
  transform_storage_exn (fun () ->
      let read_write = Db.VBD.get_mode ~__context ~self:vbd = `RW in
      on_vdi ~__context ~vbd ~domid (fun rpc dbg dp sr vdi ->
          let module C = Storage_interface.StorageAPI (Idl.Exn.GenClient (struct
            let rpc = rpc
          end)) in
          let vm = Storage_interface.Vm.of_string (string_of_int domid) in
          let attach_info = C.VDI.attach3 dbg dp sr vdi vm read_write in
          C.VDI.activate3 dbg dp sr vdi vm ;
          f attach_info
      )
  )

(** [deactivate_and_detach __context vbd domid] idempotent function which ensures
    that any attached or activated VDI gets properly deactivated and detached. *)
let deactivate_and_detach ~__context ~vbd ~domid =
  transform_storage_exn (fun () ->
      (* It suffices to destroy the datapath: any attached or activated VDIs will be
         			   automatically detached and deactivated. *)
      on_vdi ~__context ~vbd ~domid (fun rpc dbg dp _sr _vdi ->
          let module C = Storage_interface.StorageAPI (Idl.Exn.GenClient (struct
            let rpc = rpc
          end)) in
          C.DP.destroy dbg dp false
      )
  )

let diagnostics ~__context =
  let dbg = Context.get_task_id __context |> Ref.string_of in
  String.concat "\n"
    [
      "DataPath information:"
    ; Client.DP.diagnostics ()
    ; "Backend information:"
    ; Client.Query.diagnostics dbg
    ]

let dp_destroy ~__context dp allow_leak =
  transform_storage_exn (fun () ->
      let dbg = Context.get_task_id __context in
      Client.DP.destroy (Ref.string_of dbg) dp allow_leak
  )

(* Set my PBD.currently_attached fields in the Pool database to match the local one *)
let resynchronise_pbds ~__context ~pbds =
  let dbg = Context.get_task_id __context in
  let srs = Client.SR.list (Ref.string_of dbg) in
  debug "Currently-attached SRs: [ %s ]"
    (String.concat "; " (List.map s_of_sr srs)) ;
  List.iter
    (fun self ->
      try
        let sr_uuid =
          Db.SR.get_uuid ~__context ~self:(Db.PBD.get_SR ~__context ~self)
        in
        let sr = Storage_interface.Sr.of_string sr_uuid in
        let value = List.mem sr srs in
        debug "Setting PBD %s currently_attached <- %b" (Ref.string_of self)
          value ;
        try
          ( if value then
              let (_ : query_result) = bind ~__context ~pbd:self in
              ()
          ) ;
          Db.PBD.set_currently_attached ~__context ~self ~value
        with _ ->
          (* Unchecked this will block the dbsync code *)
          error
            "Service implementing SR %s has failed. Performing emergency reset \
             of SR state"
            sr_uuid ;
          Client.SR.reset (Ref.string_of dbg) sr ;
          Db.PBD.set_currently_attached ~__context ~self ~value:false
      with Db_exn.DBCache_NotFound (_, ("PBD" | "SR"), _) as e ->
        debug "Ignoring PBD/SR that got deleted before we resynchronised: %s"
          (Printexc.to_string e)
    )
    pbds

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
    let ks = List.filter (Astring.String.is_prefix ~affix:prefix) ks in
    let ks =
      List.map
        (fun k ->
          String.sub k (String.length prefix)
            (String.length k - String.length prefix)
        )
        ks
    in
    List.map Ref.of_string ks
  in
  (* If this VDI is currently locked to this host, remove the lock.
     	   If this VDI is currently locked to a non-existent host (note host references
     	   change across pool join), remove the lock. *)
  let unlock_vdi (vdi_ref, vdi_rec) =
    (* VDI is already unlocked is the common case: avoid eggregious logspam *)
    let hosts = hosts_of vdi_rec in
    let i_locked_it = List.mem localhost hosts in
    let all = List.fold_left ( && ) true in
    let someone_leaked_it =
      hosts <> [] && all (List.map (fun h -> not (List.mem h hosts)) all_hosts)
    in
    if i_locked_it || someone_leaked_it then (
      info "Unlocking VDI %s (because %s)" (Ref.string_of vdi_ref)
        ( if i_locked_it then
            "I locked it and then restarted"
        else
          "it was leaked (pool join?)"
        ) ;
      try
        List.iter
          (fun h ->
            Db.VDI.remove_from_sm_config ~__context ~self:vdi_ref ~key:(key h)
          )
          hosts
      with e ->
        error "Failed to unlock VDI %s: %s" (Ref.string_of vdi_ref)
          (ExnHelper.string_of_exn e)
    )
  in
  let open Vdi_automaton in
  (* Lock this VDI to this host *)
  let lock_vdi (vdi_ref, vdi_rec) ro_rw =
    info "Locking VDI %s" (Ref.string_of vdi_ref) ;
    if not (List.mem_assoc (key localhost) vdi_rec.API.vDI_sm_config) then
      try
        Db.VDI.add_to_sm_config ~__context ~self:vdi_ref ~key:(key localhost)
          ~value:(string_of_ro_rw ro_rw)
      with e ->
        error "Failed to lock VDI %s: %s" (Ref.string_of vdi_ref)
          (ExnHelper.string_of_exn e)
  in
  let remember key ro_rw =
    (* The module above contains a hashtable of R/O vs R/W-ness *)
    with_lock Storage_smapiv1.vdi_read_write_m (fun () ->
        Hashtbl.replace Storage_smapiv1.vdi_read_write key (ro_rw = RW)
    )
  in
  let dbg = Ref.string_of (Context.get_task_id __context) in
  let srs = Client.SR.list dbg in
  let sr_uuids =
    List.map
      (fun sr ->
        (sr, Storage_interface.Sr.of_string (Db.SR.get_uuid ~__context ~self:sr))
      )
      (Db.SR.get_all ~__context)
  in
  List.iter
    (fun (vdi_ref, vdi_rec) ->
      let sr = List.assoc vdi_rec.API.vDI_SR sr_uuids in
      let vdi = Storage_interface.Vdi.of_string vdi_rec.API.vDI_location in
      if List.mem sr srs then
        try
          let x = Client.DP.stat_vdi dbg sr vdi () in
          match x.superstate with
          | Activated RO ->
              lock_vdi (vdi_ref, vdi_rec) RO ;
              remember (sr, vdi) RO
          | Activated RW ->
              lock_vdi (vdi_ref, vdi_rec) RW ;
              remember (sr, vdi) RW
          | Attached RO ->
              unlock_vdi (vdi_ref, vdi_rec) ;
              remember (sr, vdi) RO
          | Attached RW ->
              unlock_vdi (vdi_ref, vdi_rec) ;
              remember (sr, vdi) RW
          | Detached ->
              unlock_vdi (vdi_ref, vdi_rec)
        with e ->
          error "Unable to query state of VDI: %s, %s" (s_of_vdi vdi)
            (Printexc.to_string e)
      else
        unlock_vdi (vdi_ref, vdi_rec)
    )
    all_vdi_recs

(* This is a symptom of the ordering-sensitivity of the SM backend: it is not possible
   to upgrade RO -> RW or downgrade RW -> RO on the fly.
   One possible fix is to always attach RW and enforce read/only-ness at the VBD-level.
   However we would need to fix the LVHD "attach provisioning mode". *)
let vbd_attach_order ~__context vbds =
  (* return RW devices first since the storage layer can't upgrade a
     	   'RO attach' into a 'RW attach' *)
  let rw, ro =
    List.partition (fun self -> Db.VBD.get_mode ~__context ~self = `RW) vbds
  in
  rw @ ro

let vbd_detach_order ~__context vbds =
  List.rev (vbd_attach_order ~__context vbds)

let create_sr ~__context ~sr ~name_label ~name_description ~physical_size =
  transform_storage_exn (fun () ->
      let pbd, pbd_t = Sm.get_my_pbd_for_sr __context sr in
      let (_ : query_result) = bind ~__context ~pbd in
      let dbg = Ref.string_of (Context.get_task_id __context) in
      let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
      let result =
        Client.SR.create dbg
          (Storage_interface.Sr.of_string sr_uuid)
          name_label name_description pbd_t.API.pBD_device_config physical_size
      in
      unbind ~__context ~pbd ; result
  )

(* This is because the current backends want SR.attached <=> PBD.currently_attached=true.
   It would be better not to plug in the PBD, so that other API calls will be blocked. *)
let destroy_sr ~__context ~sr ~and_vdis =
  transform_storage_exn (fun () ->
      let pbd, pbd_t = Sm.get_my_pbd_for_sr __context sr in
      let (_ : query_result) = bind ~__context ~pbd in
      let dbg = Ref.string_of (Context.get_task_id __context) in
      let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
      Client.SR.attach dbg
        (Storage_interface.Sr.of_string sr_uuid)
        pbd_t.API.pBD_device_config ;
      (* The current backends expect the PBD to be temporarily set to currently_attached = true *)
      Db.PBD.set_currently_attached ~__context ~self:pbd ~value:true ;
      finally
        (fun () ->
          try
            List.iter
              (fun vdi ->
                let location = Db.VDI.get_location ~__context ~self:vdi in
                Client.VDI.destroy dbg
                  (Storage_interface.Sr.of_string sr_uuid)
                  (Storage_interface.Vdi.of_string location)
              )
              and_vdis ;
            Client.SR.destroy dbg (Storage_interface.Sr.of_string sr_uuid)
          with exn ->
            (* Clean up: SR is left attached if destroy fails *)
            Client.SR.detach dbg (Storage_interface.Sr.of_string sr_uuid) ;
            raise exn
        )
        (fun () ->
          (* All PBDs are clearly currently_attached = false now *)
          Db.PBD.set_currently_attached ~__context ~self:pbd ~value:false
        ) ;
      unbind ~__context ~pbd
  )

let task_cancel ~__context ~self =
  try
    let id = TaskHelper.task_to_id_exn self |> unwrap in
    let dbg = Context.string_of_task __context in
    info "storage_access: TASK.cancel %s" id ;
    Client.TASK.cancel dbg id |> ignore ;
    true
  with
  | Not_found ->
      false
  | Not_an_sm_task ->
      false
