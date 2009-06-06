module D = Debug.Debugger(struct let name="xapi" end)
open D

(* Allow xapi 'plugins' (ie scripts) which can be dropped onto individual host dom0s
   and then remote-execed by API clients *)

(* Only scripts in the Xapi_globs.xapi_plugins_root can be called *)
let find_plugin name = 
  let all = try Array.to_list (Sys.readdir Xapi_globs.xapi_plugins_root) with _ -> [] in
  (* Sys.readdir output doesn't include "." or ".." *)
  if List.mem name all 
  then Filename.concat Xapi_globs.xapi_plugins_root name
  else raise (Api_errors.Server_error(Api_errors.xenapi_missing_plugin, [ name ]))

(* Execute the plugin with XMLRPC-over-cmdline/stdout convention, like the SM plugins.
   The args provided are a Map(String, String) and these will be passed as an XMLRPC struct *)
let call_plugin session_id plugin_name fn_name args = 
  let plugin_name = find_plugin plugin_name in

  (* Marshal the args as XMLRPC *)
  let args = List.map (fun (k, v) -> k, XMLRPC.To.string v) args in
  let call = XMLRPC.To.methodCall fn_name [ API.To.ref_session session_id; XMLRPC.To.structure args ] in
  let output, _ = 
    try
      Forkhelpers.execute_command_get_output plugin_name [ Xml.to_string call ]
    with 
    | Forkhelpers.Spawn_internal_error(log, output, Unix.WSTOPPED i) ->
	raise (Api_errors.Server_error (Api_errors.xenapi_plugin_failure, ["task stopped"; output; log ]))
    | Forkhelpers.Spawn_internal_error(log, output, Unix.WSIGNALED i) ->
	raise (Api_errors.Server_error (Api_errors.xenapi_plugin_failure, ["task signaled"; output; log ]))
    | Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED i) ->
	raise (Api_errors.Server_error (Api_errors.xenapi_plugin_failure, ["non-zero exit"; output; log ])) in
  try
    match XMLRPC.From.methodResponse (Xml.parse_string output) with
    | XMLRPC.Fault(code, reason) -> raise (Api_errors.Server_error(Api_errors.xenapi_plugin_failure, [ "fault"; Int32.to_string code; reason ]))
    | XMLRPC.Success [ result ] -> XMLRPC.From.string result 
    | XMLRPC.Failure(code, params) -> raise (Api_errors.Server_error(code, params))
  with Xml.Error e ->
    raise (Api_errors.Server_error(Api_errors.xenapi_plugin_failure, [ "parse failure"; Xml.error e ]))
