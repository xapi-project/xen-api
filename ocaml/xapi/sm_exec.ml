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
(** Storage manager backend: external operations through exec
 * @group Storage
*)

module Unixext = Xapi_stdext_unix.Unixext

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

open Printf
open Smint

module D = Debug.Make (struct let name = "sm_exec" end)

open D

module E = Debug.Make (struct let name = "mscgen" end)

let cmd_name driver = sprintf "%s/%sSR" !Xapi_globs.sm_dir driver

let sm_username = "__sm__backend"

let with_dbg ~name ~dbg f =
  Debug_info.with_dbg ~module_name:"Sm_exec" ~name ~dbg f

(*********************************************************************************************)
(* Random utility functions *)

let env_vars =
  Array.concat
    [
      Forkhelpers.default_path_env_pair
    ; Env_record.to_string_array [Env_record.pair ("ORIGINATOR", "SM")]
    ]

type call = {
    (* All calls are performed by a specific Host with a special Session and device_config *)
    host_ref: API.ref_host
  ; session_ref: API.ref_session option
  ; device_config: (string * string) list
  ; (* SR probe takes sm config at the SR level *)
    sr_sm_config: (string * string) list option
  ; (* Most calls operate within a specific SR *)
    sr_ref: API.ref_SR option
  ; sr_uuid: string option
  ; (* Snapshot and clone supply a set of driver_params from the user *)
    driver_params: (string * string) list option
  ; (* Create and introduce supply an initial sm_config from the user *)
    vdi_sm_config: (string * string) list option
  ; vdi_type: string option
  ; (* Introduce supplies a UUID to use *)
    new_uuid: string option
  ; (* Calls operating on an existing VDI supply both a reference and location *)
    vdi_ref: API.ref_VDI option
  ; vdi_location: string option
  ; vdi_uuid: string option
  ; vdi_on_boot: string option
  ; vdi_allow_caching: string option
  ; (* Reference to the task which performs the call *)
    subtask_of: API.ref_task option
  ; local_cache_sr: string option
  ; cmd: string
  ; args: string list
}

let make_call ?driver_params ?sr_sm_config ?vdi_sm_config ?vdi_type
    ?vdi_location ?new_uuid ?sr_ref ?vdi_ref ?vdi_uuid
    (subtask_of, device_config) cmd args =
  Server_helpers.exec_with_new_task "sm_exec" (fun __context ->
      (* Only allow a subset of calls if the SR has been introduced by a DR task. *)
      Option.iter
        (fun sr ->
          if
            Db.is_valid_ref __context
              (Db.SR.get_introduced_by ~__context ~self:sr)
          then
            if
              not
                (List.mem cmd
                   [
                     "sr_attach"
                   ; "sr_detach"
                   ; "vdi_attach"
                   ; "vdi_detach"
                   ; "vdi_activate"
                   ; "vdi_deactivate"
                   ; "sr_probe"
                   ; "sr_scan"
                   ; "sr_content_type"
                   ]
                )
            then
              raise
                (Storage_interface.Storage_error
                   (Backend_error
                      ( Api_errors.operation_not_allowed
                      , [
                          Printf.sprintf
                            "The operation %s is not allowed on this SR as it \
                             is being used for disaster recovery."
                            cmd
                        ]
                      )
                   )
                )
        )
        sr_ref ;
      let vdi_location =
        if vdi_location <> None then
          vdi_location
        else
          Option.map (fun self -> Db.VDI.get_location ~__context ~self) vdi_ref
      in
      let vdi_uuid =
        match (cmd, vdi_ref, vdi_uuid) with
        | "vdi_create", None, (Some x as uuid) ->
            debug "%s: cmd=%s vdi_uuid=%s" __FUNCTION__ cmd x ;
            uuid
            (* when creating a VDI we sometimes want to provide the UUID
               rather than letting the backend pick one. This is to
               support backup VDIs CP-46179. So in that case, use the
               provided UUID but not for other commands *)
        | _, None, Some uuid ->
            warn "%s: cmd=%s vdi_uuid=%s - should not happen" __FUNCTION__ cmd
              uuid ;
            None
        | _, Some self, _ ->
            Db.VDI.get_uuid ~__context ~self |> Option.some
        | _, None, None ->
            None
      in
      let vdi_on_boot =
        Option.map
          (fun self ->
            match Db.VDI.get_on_boot ~__context ~self with
            | `persist ->
                "persist"
            | `reset ->
                "reset"
          )
          vdi_ref
      in
      let vdi_allow_caching =
        Option.map
          (fun self ->
            string_of_bool (Db.VDI.get_allow_caching ~__context ~self)
          )
          vdi_ref
      in
      let local_cache_sr =
        try
          Some
            (Db.SR.get_uuid ~__context
               ~self:
                 (Db.Host.get_local_cache_sr ~__context
                    ~self:(Helpers.get_localhost ~__context)
                 )
            )
        with _ -> None
      in
      let sr_uuid =
        Option.map (fun self -> Db.SR.get_uuid ~__context ~self) sr_ref
      in
      {
        host_ref= !Xapi_globs.localhost_ref
      ; session_ref= None
      ; (* filled in at the last minute *)
        device_config
      ; sr_ref
      ; sr_uuid
      ; driver_params
      ; sr_sm_config
      ; vdi_sm_config
      ; vdi_type
      ; vdi_ref
      ; vdi_location
      ; vdi_uuid
      ; vdi_on_boot
      ; vdi_allow_caching
      ; new_uuid
      ; subtask_of
      ; local_cache_sr
      ; cmd
      ; args
      }
  )

let xmlrpc_of_call (call : call) =
  let kvpairs kvpairs =
    XMLRPC.To.structure
      (List.map (fun (k, v) -> (k, XMLRPC.To.string v)) kvpairs)
  in
  let common =
    [
      ("host_ref", XMLRPC.To.string (Ref.string_of call.host_ref))
    ; ("command", XMLRPC.To.string call.cmd)
    ; ("args", XMLRPC.To.array (List.map XMLRPC.To.string call.args))
    ]
  in
  let dc = [("device_config", kvpairs call.device_config)] in
  let session_ref =
    Option.fold ~none:[]
      ~some:(fun x -> [("session_ref", XMLRPC.To.string (Ref.string_of x))])
      call.session_ref
  in
  let sr_sm_config =
    Option.fold ~none:[]
      ~some:(fun x -> [("sr_sm_config", kvpairs x)])
      call.sr_sm_config
  in
  let sr_ref =
    Option.fold ~none:[]
      ~some:(fun x -> [("sr_ref", XMLRPC.To.string (Ref.string_of x))])
      call.sr_ref
  in
  let sr_uuid =
    Option.fold ~none:[]
      ~some:(fun x -> [("sr_uuid", XMLRPC.To.string x)])
      call.sr_uuid
  in
  let vdi_type =
    Option.fold ~none:[]
      ~some:(fun x -> [("vdi_type", XMLRPC.To.string x)])
      call.vdi_type
  in
  let vdi_ref =
    Option.fold ~none:[]
      ~some:(fun x -> [("vdi_ref", XMLRPC.To.string (Ref.string_of x))])
      call.vdi_ref
  in
  let vdi_location =
    Option.fold ~none:[]
      ~some:(fun x -> [("vdi_location", XMLRPC.To.string x)])
      call.vdi_location
  in
  let vdi_uuid =
    Option.fold ~none:[]
      ~some:(fun x -> [("vdi_uuid", XMLRPC.To.string x)])
      call.vdi_uuid
  in
  let vdi_on_boot =
    Option.fold ~none:[]
      ~some:(fun x -> [("vdi_on_boot", XMLRPC.To.string x)])
      call.vdi_on_boot
  in
  let vdi_allow_caching =
    Option.fold ~none:[]
      ~some:(fun x -> [("vdi_allow_caching", XMLRPC.To.string x)])
      call.vdi_allow_caching
  in
  let new_uuid =
    Option.fold ~none:[]
      ~some:(fun x -> [("new_uuid", XMLRPC.To.string x)])
      call.new_uuid
  in
  let driver_params =
    Option.fold ~none:[]
      ~some:(fun x -> [("driver_params", kvpairs x)])
      call.driver_params
  in
  let vdi_sm_config =
    Option.fold ~none:[]
      ~some:(fun x -> [("vdi_sm_config", kvpairs x)])
      call.vdi_sm_config
  in
  let subtask_of =
    Option.fold ~none:[]
      ~some:(fun x -> [("subtask_of", XMLRPC.To.string (Ref.string_of x))])
      call.subtask_of
  in
  let local_cache_sr =
    Option.fold ~none:[]
      ~some:(fun x -> [("local_cache_sr", XMLRPC.To.string x)])
      call.local_cache_sr
  in
  let all =
    common
    @ dc
    @ session_ref
    @ sr_sm_config
    @ sr_ref
    @ vdi_type
    @ sr_uuid
    @ vdi_ref
    @ vdi_location
    @ vdi_uuid
    @ driver_params
    @ vdi_sm_config
    @ new_uuid
    @ subtask_of
    @ vdi_on_boot
    @ vdi_allow_caching
    @ local_cache_sr
  in
  XMLRPC.To.methodCall call.cmd [XMLRPC.To.structure all]

let methodResponse xml =
  match xml with
  | Xml.Element
      ( "methodResponse"
      , _
      , [Xml.Element ("params", _, [Xml.Element ("param", _, [param])])]
      ) ->
      XMLRPC.Success [param]
  | xml ->
      XMLRPC.From.methodResponse xml

(****************************************************************************************)
(* Functions that actually execute the python backends *)

let with_session sr f =
  Server_helpers.exec_with_new_task "sm_exec" (fun __context ->
      let create_session () =
        let host = !Xapi_globs.localhost_ref in
        let session =
          Xapi_session.login_no_password ~__context ~uname:None ~host
            ~pool:false ~is_local_superuser:true ~subject:Ref.null
            ~auth_user_sid:"" ~auth_user_name:sm_username ~rbac_permissions:[]
        in
        (* Give this session access to this particular SR *)
        Option.iter
          (fun sr ->
            Db.Session.add_to_other_config ~__context ~self:session
              ~key:Xapi_globs._sm_session ~value:(Ref.string_of sr)
          )
          sr ;
        session
      in
      let destroy_session session_id =
        Xapi_session.destroy_db_session ~__context ~self:session_id
      in
      let session_id = create_session () in
      finally (fun () -> f session_id) (fun () -> destroy_session session_id)
  )

let exec_xmlrpc ~dbg ?context:_ ?(needs_session = true) (driver : string)
    (call : call) =
  with_dbg ~name:call.cmd ~dbg @@ fun di ->
  let do_call call =
    let xml = xmlrpc_of_call call in
    let name = Printf.sprintf "sm_exec: %s" call.cmd in
    let xml, stderr =
      Xapi_database.Stats.time_this name (fun () ->
          let exe = cmd_name driver in
          (* Logging call.cmd is safe, but call.args could contain a password. *)
          try
            E.debug "smapiv2=>smapiv1 [label=\"%s\"];" call.cmd ;
            let args = [Xml.to_string xml] in
            let output, stderr =
              let env, exe, args =
                match Xapi_observer_components.is_smapi_enabled () with
                | false ->
                    (Some env_vars, exe, args)
                | true ->
                    Xapi_observer_components.env_exe_args_of ~env_vars
                      ~component:Xapi_observer_components.SMApi ~exe ~args
              in
              Forkhelpers.execute_command_get_output ?tracing:di.tracing ?env
                exe args
            in
            try (Xml.parse_string output, stderr)
            with e ->
              error
                "Failed to parse result from %s: stdout:%s stderr:%s \
                 exception:%s"
                exe output stderr (Printexc.to_string e) ;
              raise
                (Storage_interface.Storage_error
                   (Backend_error
                      ( Api_errors.sr_backend_failure
                      , [Printexc.to_string e; output; stderr]
                      )
                   )
                )
          with
          | Forkhelpers.Spawn_internal_error (log, output, Unix.WSTOPPED i) ->
              raise
                (Storage_interface.Storage_error
                   (Backend_error
                      ( Api_errors.sr_backend_failure
                      , ["exit code: " ^ string_of_int i; output; log]
                      )
                   )
                )
          | Forkhelpers.Spawn_internal_error (log, output, Unix.WSIGNALED i) ->
              raise
                (Storage_interface.Storage_error
                   (Backend_error
                      ( Api_errors.sr_backend_failure
                      , [
                          Printf.sprintf "received signal: %a" Debug.Pp.signal i
                        ; output
                        ; log
                        ]
                      )
                   )
                )
          | Forkhelpers.Spawn_internal_error (log, output, Unix.WEXITED _) ->
              raise
                (Storage_interface.Storage_error
                   (Backend_error
                      ( Api_errors.sr_backend_failure
                      , ["non-zero exit"; output; log]
                      )
                   )
                )
      )
    in
    match methodResponse xml with
    | XMLRPC.Fault (38l, _) ->
        raise Not_implemented_in_backend
    | XMLRPC.Fault (39l, _) ->
        raise
          (Storage_interface.Storage_error
             (Backend_error (Api_errors.sr_not_empty, []))
          )
    | XMLRPC.Fault (24l, _) ->
        raise
          (Storage_interface.Storage_error
             (Backend_error (Api_errors.vdi_in_use, []))
          )
    | XMLRPC.Fault (16l, _) ->
        raise
          (Storage_interface.Storage_error
             (Backend_error (Api_errors.sr_device_in_use, []))
          )
    | XMLRPC.Fault (144l, _) ->
        (* Any call which returns this 'VDIMissing' error really ought to have
           	   been provided both an SR and VDI reference... *)
        let sr = Option.fold ~none:"" ~some:Ref.string_of call.sr_ref in
        let vdi = Option.fold ~none:"" ~some:Ref.string_of call.vdi_ref in
        raise
          (Storage_interface.Storage_error
             (Backend_error (Api_errors.vdi_missing, [sr; vdi]))
          )
    | XMLRPC.Fault (code, reason) ->
        let xenapi_code =
          Api_errors.sr_backend_failure ^ "_" ^ Int32.to_string code
        in
        raise
          (Storage_interface.Storage_error
             (Backend_error (xenapi_code, [""; reason; stderr]))
          )
    | XMLRPC.Success [result] ->
        result
    | _ ->
        raise
          (Storage_interface.Storage_error
             (Backend_error
                ( Api_errors.internal_error
                , ["Unexpected response from SM plugin"]
                )
             )
          )
  in
  if needs_session then
    with_session call.sr_ref (fun session_id ->
        do_call {call with session_ref= Some session_id}
    )
  else
    do_call call

(********************************************************************)
(** Some functions to cope with the XML that the SM backends return *)

let xmlrpc_parse_failure (xml : string) (reason : string) =
  raise
    (Storage_interface.Storage_error
       (Backend_error
          ( Api_errors.sr_backend_failure
          , [""; "XML parse failure: " ^ xml; reason]
          )
       )
    )

let rethrow_parse_failures xml f =
  try f () with
  | Backend_missing_field s ->
      xmlrpc_parse_failure xml (Printf.sprintf "<struct> missing field: %s" s)
  | XMLRPC.RunTimeTypeError (s, x) ->
      xmlrpc_parse_failure xml
        (Printf.sprintf
           "XMLRPC unmarshall RunTimeTypeError: looking for %s found %s" s
           (Xml.to_string_fmt x)
        )
  | e ->
      xmlrpc_parse_failure xml (Printexc.to_string e)

let safe_assoc key pairs =
  try List.assoc key pairs with Not_found -> raise (Backend_missing_field key)

(* Used for both sr_scan, vdi_create and vdi_resize *)
let parse_vdi_info (vdi_info_struct : Xml.xml) =
  rethrow_parse_failures (Xml.to_string_fmt vdi_info_struct) (fun () ->
      let pairs =
        List.map
          (fun (key, v) -> (key, XMLRPC.From.string v))
          (XMLRPC.From.structure vdi_info_struct)
      in
      {
        vdi_info_uuid= Some (safe_assoc "uuid" pairs)
      ; vdi_info_location= safe_assoc "location" pairs
      }
  )

let parse_string (xml : Xml.xml) = XMLRPC.From.string xml

let parse_unit (xml : Xml.xml) = XMLRPC.From.nil xml

let parse_attach_result (xml : Xml.xml) =
  rethrow_parse_failures (Xml.to_string_fmt xml) (fun () ->
      let info = XMLRPC.From.structure xml in
      let params =
        try Some (XMLRPC.From.string (safe_assoc "params" info))
        with Backend_missing_field _ -> None
      in
      let params_nbd = XMLRPC.From.string (safe_assoc "params_nbd" info) in
      let o_direct =
        try XMLRPC.From.boolean (safe_assoc "o_direct" info) with _ -> true
      in
      let o_direct_reason =
        try XMLRPC.From.string (safe_assoc "o_direct_reason" info)
        with _ -> ""
      in
      let xenstore_data =
        try
          List.map
            (fun (x, y) -> (x, XMLRPC.From.string y))
            (XMLRPC.From.structure (safe_assoc "xenstore_data" info))
        with _ -> []
      in
      {params; params_nbd; o_direct; o_direct_reason; xenstore_data}
  )

let parse_sr_get_driver_info driver (xml : Xml.xml) =
  let info = XMLRPC.From.structure xml in
  (* Parse the standard strings *)
  let name = XMLRPC.From.string (safe_assoc "name" info)
  and description = XMLRPC.From.string (safe_assoc "description" info)
  and vendor = XMLRPC.From.string (safe_assoc "vendor" info)
  and copyright = XMLRPC.From.string (safe_assoc "copyright" info)
  and driver_version = XMLRPC.From.string (safe_assoc "driver_version" info)
  and required_api_version =
    XMLRPC.From.string (safe_assoc "required_api_version" info)
  in
  let strings =
    XMLRPC.From.array XMLRPC.From.string (safe_assoc "capabilities" info)
  in
  let features = Smint.Feature.parse_capability_int64 strings in
  let text_features = List.map Smint.Feature.to_string features in
  (* Parse the driver options *)
  let configuration =
    List.map
      (fun kvpairs ->
        ( XMLRPC.From.string (safe_assoc "key" kvpairs)
        , XMLRPC.From.string (safe_assoc "description" kvpairs)
        )
      )
      (XMLRPC.From.array XMLRPC.From.structure (safe_assoc "configuration" info))
  in
  {
    sr_driver_filename= driver
  ; sr_driver_name= name
  ; sr_driver_description= description
  ; sr_driver_vendor= vendor
  ; sr_driver_copyright= copyright
  ; sr_driver_version= driver_version
  ; sr_driver_required_api_version= required_api_version
  ; sr_driver_features= features
  ; sr_driver_configuration= configuration
  ; sr_driver_text_features= text_features
  ; sr_driver_required_cluster_stack= []
  ; sr_smapi_version= SMAPIv1
  }

let sr_get_driver_info ~dbg driver =
  with_dbg ~name:"sr_get_driver_info" ~dbg @@ fun di ->
  let dbg = Debug_info.to_string di in
  let call = make_call (None, []) "sr_get_driver_info" [] in
  parse_sr_get_driver_info driver
    (exec_xmlrpc ~dbg ~needs_session:false driver call)

(* Call the supplied function (passing driver name and driver_info) for every
 * backend and daemon found. *)
let get_supported ~dbg add_fn =
  with_dbg ~name:"get_supported" ~dbg @@ fun di ->
  let dbg = Debug_info.to_string di in
  let check_driver entry =
    if Astring.String.is_suffix ~affix:"SR" entry then
      let driver = String.sub entry 0 (String.length entry - 2) in
      if not (Xapi_globs.accept_sm_plugin driver) then
        info
          "Skipping SMAPIv1 plugin %s: not in sm-plugins whitelist in \
           configuration file"
          driver
      else
        try
          Unix.access (cmd_name driver) [Unix.X_OK] ;
          let i = sr_get_driver_info ~dbg driver in
          add_fn driver i
        with e ->
          error "Rejecting SM plugin: %s because of exception: %s (executable)"
            driver (Printexc.to_string e)
  in
  List.iter
    (fun (f, dir) ->
      if Sys.file_exists dir then (
        debug "Scanning directory %s for SM plugins" dir ;
        try Array.iter f (Sys.readdir dir)
        with e ->
          error "Error checking directory %s for SM backends: %s" dir
            (ExnHelper.string_of_exn e)
      ) else
        error "Not scanning %s for SM backends: directory does not exist" dir
    )
    [(check_driver, !Xapi_globs.sm_dir)]

(*********************************************************************)
