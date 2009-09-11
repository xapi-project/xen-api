(*
 * Copyright (c) 2006-2007 XenSource Inc.
 * Author: Vincent Hanquez <vincent@xensource.com>
 *
 * storage manager backend: external operations through exec
 *)
open Pervasiveext
open Stringext
open Printf
open Smint

module D=Debug.Debugger(struct let name="sm_exec" end)
open D

let sm_dir = "/opt/xensource/sm"
let sm_daemon_dir = "/var/xapi/sm"

let cmd_name driver = sprintf "%s/%sSR" sm_dir driver
let daemon_path driver = sprintf "%s/%s" sm_daemon_dir driver

(*********************************************************************************************)
(* Random utility functions *)

type call = {
  (* All calls are performed by a specific Host with a special Session and device_config *)
  host_ref: API.ref_host;
  session_ref: API.ref_session option;
  device_config: (string * string) list;
  
  (* SR probe takes sm config at the SR level *)
  sr_sm_config: (string * string) list option;

  (* Most calls operate within a specific SR *)
  sr_ref: API.ref_SR option;
  sr_uuid: string option;

  (* Snapshot and clone supply a set of driver_params from the user *)
  driver_params: (string * string) list option;

  (* Create and introduce supply an initial sm_config from the user *)
  vdi_sm_config: (string * string) list option;
  (* Introduce supplies a UUID to use *)
  new_uuid: string option;

  (* Calls operating on an existing VDI supply both a reference and location *)
  vdi_ref: API.ref_VDI option;
  vdi_location: string option;
  vdi_uuid: string option;

  (* Reference to the task which performs the call *)
  subtask_of: API.ref_task option;

  cmd: string;
  args: string list;
}

let make_call ?driver_params ?sr_sm_config ?vdi_sm_config ?vdi_location ?new_uuid ?sr_ref ?vdi_ref (subtask_of,device_config) cmd args =
  Server_helpers.exec_with_new_task "sm_exec: reading host other-config"
    (fun __context ->
       let vdi_location = 
	 if vdi_location <> None 
	 then vdi_location
	 else may (fun self -> Db.VDI.get_location ~__context ~self) vdi_ref in
       let vdi_uuid = may (fun self -> Db.VDI.get_uuid ~__context ~self) vdi_ref in
       let sr_uuid = may (fun self -> Db.SR.get_uuid ~__context ~self) sr_ref in
       { host_ref = !Xapi_globs.localhost_ref;
	 session_ref = None; (* filled in at the last minute *)
	 device_config = device_config;
	 sr_ref = sr_ref;
	 sr_uuid = sr_uuid;
	 driver_params = driver_params;
	 sr_sm_config = sr_sm_config;
	 vdi_sm_config = vdi_sm_config;
	 vdi_ref = vdi_ref;
	 vdi_location = vdi_location;
	 vdi_uuid = vdi_uuid;
	 new_uuid = new_uuid;
	 subtask_of = subtask_of;
	 cmd = cmd;
	 args = args
       })

let xmlrpc_of_call (call: call) =
  let kvpairs kvpairs = 
    XMLRPC.To.structure 
      (List.map (fun (k, v) -> k, XMLRPC.To.string v) kvpairs) in
  
  let common = [ "host_ref", XMLRPC.To.string (Ref.string_of call.host_ref);
		 "command", XMLRPC.To.string (call.cmd);
		 "args", XMLRPC.To.array (List.map XMLRPC.To.string call.args);
	       ] in
  let dc = [ "device_config", kvpairs call.device_config ] in
  let session_ref = default [] (may (fun x -> [ "session_ref", XMLRPC.To.string (Ref.string_of x) ]) call.session_ref) in
  let sr_sm_config = default [] (may (fun x -> [ "sr_sm_config", kvpairs x ]) call.sr_sm_config) in
  let sr_ref = default [] (may (fun x -> [ "sr_ref", XMLRPC.To.string (Ref.string_of x) ]) call.sr_ref) in
  let sr_uuid = default [] (may (fun x -> [ "sr_uuid", XMLRPC.To.string x ]) call.sr_uuid) in
  let vdi_ref = default [] (may (fun x -> [ "vdi_ref", XMLRPC.To.string (Ref.string_of x) ]) call.vdi_ref) in
  let vdi_location = default [] (may (fun x -> [ "vdi_location", XMLRPC.To.string x ]) call.vdi_location) in
  let vdi_uuid = default [] (may (fun x -> [ "vdi_uuid", XMLRPC.To.string x ]) call.vdi_uuid) in
  let new_uuid = default [] (may (fun x -> [ "new_uuid", XMLRPC.To.string x ]) call.new_uuid) in

  let driver_params = default [] (may (fun x -> [ "driver_params", kvpairs x ]) call.driver_params) in
  let vdi_sm_config = default [] (may (fun x -> [ "vdi_sm_config", kvpairs x ]) call.vdi_sm_config) in
  let subtask_of = default [] (may (fun x -> [ "subtask_of", XMLRPC.To.string (Ref.string_of x) ]) call.subtask_of) in

  let all = common @ dc @ session_ref @ sr_sm_config @ sr_ref @ sr_uuid @ vdi_ref @ vdi_location @ vdi_uuid @ driver_params @ vdi_sm_config @ new_uuid @ subtask_of in
  XMLRPC.To.methodCall call.cmd [ XMLRPC.To.structure all ]

let methodResponse xml =
  let rtte name xml = raise (XMLRPC.RunTimeTypeError(name, xml)) in
  match xml with
  | Xml.Element("methodResponse", _, [ Xml.Element("params", _, [ Xml.Element("param", _, [ param ]) ]) ]) ->
      XMLRPC.Success [ param ]
  | xml -> XMLRPC.From.methodResponse xml


(****************************************************************************************)
(* Functions that actually execute the python backends *)

let spawn_internal ?(cb_set=(fun _ -> ())) ?(cb_clear=(fun () -> ())) cmdarg =
  try
    Forkhelpers.execute_command_get_output ~cb_set ~cb_clear cmdarg.(0) (List.tl (Array.to_list cmdarg))
  with 
  | Forkhelpers.Spawn_internal_error(log, output, Unix.WSTOPPED i) ->
      raise (Api_errors.Server_error (Api_errors.sr_backend_failure, ["task stopped"; output; log ]))
  | Forkhelpers.Spawn_internal_error(log, output, Unix.WSIGNALED i) ->
      raise (Api_errors.Server_error (Api_errors.sr_backend_failure, ["task signaled"; output; log ]))
  | Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED i) ->
      raise (Api_errors.Server_error (Api_errors.sr_backend_failure, ["non-zero exit"; output; log ]))
      
let with_session sr f =
  Server_helpers.exec_with_new_task "sm_exec" (fun __context ->
  let create_session () =
    let host = !Xapi_globs.localhost_ref in
    let session=Xapi_session.login_no_password ~__context ~uname:None ~host ~pool:false ~is_local_superuser:true ~subject:(Ref.null) ~auth_user_sid:"" ~rbac_permissions:[] in
    (* Give this session access to this particular SR *)
    maybe (fun sr ->
	     Db.Session.add_to_other_config ~__context ~self:session 
	       ~key:Xapi_globs._sm_session ~value:(Ref.string_of sr)) sr;
    session
  in
  let destroy_session session_id =
    Xapi_session.destroy_db_session ~__context ~self:session_id
  in
  let session_id = create_session () in
  Pervasiveext.finally (fun () -> f session_id) (fun () -> destroy_session session_id))

let exec_xmlrpc ?context ?(needs_session=true) (ty : sm_type) (driver: string) (call: call) =
  let do_call call = 
    let xml = xmlrpc_of_call call in

    let name = Printf.sprintf "sm_exec: %s" call.cmd in

    let (xml,stderr) = Stats.time_this name (fun () -> 
      match ty with 
	| Daemon ->
	    (Xmlrpcclient.do_xml_rpc_unix ~version:"1.0" ~filename:(daemon_path driver) ~path:(Printf.sprintf "/%s" driver) xml,"")
	| Executable ->
	    let args = [| cmd_name driver; Xml.to_string xml |] in
	    Array.iter (fun txt -> debug "'%s'" txt) args;
	    let output, stderr = 
	      match context with
		| None           -> 
		    spawn_internal args
		| Some __context ->
		    let cb_set pid = TaskHelper.set_external_pid ~__context pid
		    and cb_clear () = TaskHelper.clear_external_pid ~__context in
		    spawn_internal ~cb_set ~cb_clear args
	    in
	    debug "SM stdout: '%s'; stderr: '%s'" output stderr;
	    ((Xml.parse_string output),stderr))
    in

    match methodResponse xml with
    | XMLRPC.Fault(38l, _) -> raise Not_implemented_in_backend
    | XMLRPC.Fault(39l, _) ->
	raise (Api_errors.Server_error (Api_errors.sr_not_empty, []))
    | XMLRPC.Fault(24l, _) -> 
	raise (Api_errors.Server_error (Api_errors.vdi_in_use, []))
    | XMLRPC.Fault(16l, _) -> 
	raise (Api_errors.Server_error (Api_errors.sr_device_in_use, []))
    | XMLRPC.Fault(144l, _) ->
	(* Any call which returns this 'VDIMissing' error really ought to have
	   been provided both an SR and VDI reference... *)
	let sr = default "" (may Ref.string_of call.sr_ref)
	and vdi = default "" (may Ref.string_of call.vdi_ref) in
	raise (Api_errors.Server_error (Api_errors.vdi_missing, [ sr; vdi ]))
	  
    | XMLRPC.Fault(code, reason) ->
	let xenapi_code = Api_errors.sr_backend_failure ^ "_" ^ (Int32.to_string code) in
	raise (Api_errors.Server_error(xenapi_code, [ ""; reason; stderr ]))
	  
    | XMLRPC.Success [ result ] -> result in
  if needs_session
  then with_session call.sr_ref (fun session_id -> do_call { call with session_ref = Some session_id })
  else do_call call


(********************************************************************)
(** Some functions to cope with the XML that the SM backends return *)    

let xmlrpc_parse_failure (xml: string) (reason: string) = 
  raise (Api_errors.Server_error (Api_errors.sr_backend_failure,
				  [ ""; "XML parse failure: " ^xml; reason ])) 

let rethrow_parse_failures xml f = 
  try f ()
  with 
  | Backend_missing_field s ->
      xmlrpc_parse_failure xml (Printf.sprintf "<struct> missing field: %s" s)
  | XMLRPC.RunTimeTypeError(s, x) ->
      xmlrpc_parse_failure xml (Printf.sprintf "XMLRPC unmarshall RunTimeTypeError: looking for %s found %s" s (Xml.to_string_fmt x))
  | e ->
      xmlrpc_parse_failure xml (Printexc.to_string e)

let safe_assoc key pairs = 
  try List.assoc key pairs 
  with Not_found -> raise (Backend_missing_field key)

(* Used for both sr_scan, vdi_create and vdi_resize *)
let parse_vdi_info (vdi_info_struct: Xml.xml) = 
  rethrow_parse_failures (Xml.to_string_fmt vdi_info_struct)
    (fun () ->
       let pairs = List.map (fun (key, v) -> key, XMLRPC.From.string v)
	 (XMLRPC.From.structure vdi_info_struct) in
       {
	 vdi_info_uuid = Some (safe_assoc "uuid" pairs);
	 vdi_info_location = safe_assoc "location" pairs
       }
    )

(* Used for sr_get_content_type *)
let parse_sr_content_type (xml: Xml.xml) = XMLRPC.From.string xml

let parse_string (xml: Xml.xml) = XMLRPC.From.string xml

let parse_unit (xml: Xml.xml) = XMLRPC.From.nil xml

let parse_sr_get_driver_info driver ty (xml: Xml.xml) = 
  let info = XMLRPC.From.structure xml in
  debug "parsed structure";
  (* Parse the standard strings *)
  let name = XMLRPC.From.string (safe_assoc "name" info) 
  and description = XMLRPC.From.string (safe_assoc "description" info)
  and vendor = XMLRPC.From.string (safe_assoc "vendor" info)
  and copyright = XMLRPC.From.string (safe_assoc "copyright" info)
  and driver_version = XMLRPC.From.string (safe_assoc "driver_version" info)
  and required_api_version = XMLRPC.From.string (safe_assoc "required_api_version" info) in
  
  (* Parse the capabilities *)
  let lookup_table = 
    [ "SR_PROBE",       Sr_probe;
      "SR_UPDATE",      Sr_update;
      "VDI_CREATE",     Vdi_create;
      "VDI_DELETE",     Vdi_delete;
      "VDI_ATTACH",     Vdi_attach;
      "VDI_DETACH",     Vdi_detach; 
      "VDI_RESIZE",     Vdi_resize;
      "VDI_RESIZE_ONLINE",Vdi_resize_online;
      "VDI_CLONE",      Vdi_clone;
      "VDI_SNAPSHOT",   Vdi_snapshot;
      "VDI_ACTIVATE",   Vdi_activate;
      "VDI_DEACTIVATE", Vdi_deactivate;
      "VDI_UPDATE",     Vdi_update;
      "VDI_INTRODUCE",  Vdi_introduce;
      "VDI_GENERATE_CONFIG", Vdi_generate_config;
    ] in
  let strings = XMLRPC.From.array XMLRPC.From.string (safe_assoc "capabilities" info) in
  List.iter (fun s -> 
	       if not(List.mem s (List.map fst lookup_table))
	       then debug "SR.capabilities: unknown capability %s" s) strings;
  let text_capabilities = List.filter (fun s -> List.mem s (List.map fst lookup_table)) strings in
  let capabilities = List.map (fun key -> safe_assoc key lookup_table) text_capabilities in
  
  (* Parse the driver options *)
  let configuration = 
    List.map (fun kvpairs -> 
		XMLRPC.From.string (safe_assoc "key" kvpairs),
		XMLRPC.From.string (safe_assoc "description" kvpairs))
      (XMLRPC.From.array XMLRPC.From.structure (safe_assoc "configuration" info)) in
  
  { sr_driver_filename = driver;
    sr_driver_name = name;
    sr_driver_description = description;
    sr_driver_vendor = vendor;
    sr_driver_copyright = copyright;
    sr_driver_version = driver_version;
    sr_driver_required_api_version = required_api_version;
    sr_driver_capabilities = capabilities;
    sr_driver_configuration = configuration;
    sr_driver_text_capabilities = text_capabilities;
    sr_driver_type = ty;
  }

let sr_get_driver_info driver ty = 
  let call = make_call (None,[]) "sr_get_driver_info" [] in
  parse_sr_get_driver_info driver ty (exec_xmlrpc ~needs_session:false ty driver call)
    
(* Call the supplied function (passing driver name and driver_info) for every
 * backend and daemon found. *)
let get_supported add_fn =
  let check_driver entry =
      if String.endswith "SR" entry then (
        let driver = String.sub entry 0 (String.length entry - 2) in
        try
          debug "Checking executable driver: %s" driver;
          Unix.access (cmd_name driver) [ Unix.X_OK ];
          let info = sr_get_driver_info driver Executable in
          add_fn driver info;
          debug "Driver %s supported (executable)" driver
        with e ->
          debug "Got exception while checking driver %s: %s" driver (Printexc.to_string e);
          log_backtrace ();
          debug "Rejected plugin: %s" driver
      ) in

  let check_daemon entry =
    try
      debug "Checking daemon driver: %s" entry;
      let info = sr_get_driver_info entry Daemon in
      add_fn entry info;
      debug "Driver %s supported (daemon)" entry
    with e ->
      debug "Got exception while checking daemon %s: %s" entry (Printexc.to_string e);
      log_backtrace ();
      debug "Rejected plugin: %s" entry
  in

  List.iter 
    (fun (f, dir) ->
      debug "Scanning directory %s for SM backends..." dir;
      try Array.iter f (Sys.readdir dir)
      with _ ->
        log_backtrace ();
        error "Error checking directory %s for SM backends" dir
    ) 
    [ check_driver, sm_dir;
      check_daemon, sm_daemon_dir; ]

(*********************************************************************)


  
    
