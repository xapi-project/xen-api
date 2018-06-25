(*
 * Copyright (C) Citrix Systems Inc.
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
module U = Unix
module R = Rpc
module B = Backtrace

open Core
open Async

open Xapi_storage_script_types

module Plugin_client = Xapi_storage.Plugin.Plugin(Rpc_async.GenClient ())
module Volume_client = Xapi_storage.Control.Volume(Rpc_async.GenClient ())
module Sr_client = Xapi_storage.Control.Sr(Rpc_async.GenClient ())
module Datapath_client = Xapi_storage.Data.Datapath(Rpc_async.GenClient ())

let (>>>=) = Deferred.Result.(>>=)

(** Functions for returning SMAPIv2 errors *)

(** Exception returned by fork_exec_rpc when the script invocation fails *)
exception Fork_exec_error of Storage_interface.Exception.exnty

let backend_error name args =
  let open Storage_interface in
  Exception.Backend_error (name, args)

let backend_backtrace_error name args backtrace =
  let open Storage_interface in
  match args with
  | ["Activated_on_another_host"; uuid] ->
     Exception.Activated_on_another_host(uuid)
  | _ ->
     let backtrace = rpc_of_backtrace backtrace |> Jsonrpc.to_string in
     Exception.Backend_error_with_backtrace(name, backtrace :: args)

let missing_uri () =
  backend_error "MISSING_URI" [ "Please include a URI in the device-config" ]

(** Functions to wrap calls to the above client modules and convert their
    exceptions and errors into SMAPIv2 errors of type
    [Storage_interface.Exception.exnty]. The above client modules should only
    be used with these functions, otherwise the Fork_exec_error exception
    raised by fork_exec_rpc will not be caught and the main thread will fail. *)

(* fork_exec_rpc either raises a Fork_exec_error exception or
   returns a successful RPC response *)
let return_rpc typ result =
  (* Operator to unwrap the wrapped async return type of ocaml-rpc's Rpc_async *)
  let (>*=) a b = a |> Rpc_async.M.deferred >>= b in
  Monitor.try_with
    ~extract_exn:true
    (fun () ->
       (* We need to delay the evaluation of [result] until now, because
          when fork_exec_rpc is called by GenClient.declare, it
          might immediately raise a Fork_exec_error *)
       (result ()) >*= fun result ->
         (* In practice we'll always get a successful RPC response here (Ok),
            but we still have to transform the Error to make the types match: *)
         let result = Result.map_error
             result
             ~f:(fun err -> backend_error "SCRIPT_RETURNED_RPC_ERROR" [ Rpcmarshal.marshal typ err |> R.to_string ])
         in
         return result
    )
  >>= function
  | Ok result -> return result
  | Error (Fork_exec_error err) -> return (Error err)
  (* We should not get any other exception from fork_exec_rpc: *)
  | Error e -> return (Error (backend_error "SCRIPT_FAILED" [ "Unexpected exception:" ^ (Exn.to_string e) ]))

let return_volume_rpc result = return_rpc Xapi_storage.Control.typ_of_exns result
let return_plugin_rpc result = return_rpc Xapi_storage.Common.typ_of_exnt result
let return_data_rpc result = return_rpc Xapi_storage.Common.typ_of_exnt result


let use_syslog = ref false

let log level fmt =
  Printf.ksprintf (fun s ->
    if !use_syslog then begin
      (* FIXME: this is synchronous and will block other I/O *)
      Core.Unix.Syslog.syslog ~level ~facility:Core.Unix.Syslog.Facility.DAEMON s;
    end else begin
      let w = Lazy.force Writer.stderr in
      Writer.write w s;
      Writer.newline w
    end
  ) fmt

let debug fmt = log Core.Unix.Syslog.Level.DEBUG   fmt
let info  fmt = log Core.Unix.Syslog.Level.INFO    fmt
let warn  fmt = log Core.Unix.Syslog.Level.WARNING fmt
let error fmt = log Core.Unix.Syslog.Level.ERR     fmt

let pvs_version = "3.0"
let supported_api_versions = [pvs_version; "5.0"]
let api_max = List.fold_left ~f:max supported_api_versions ~init:""

let check_plugin_version_compatible query_result =
  let Xapi_storage.Plugin.{ name; required_api_version; _ } = query_result in
  if required_api_version <> api_max then
    warn "Using deprecated SMAPIv3 API version %s, latest is %s. Update your %s plugin!" required_api_version api_max name;
  if List.mem ~equal:String.equal supported_api_versions required_api_version then
    Deferred.Result.return ()
  else
    let msg = Printf.sprintf "%s requires unknown SMAPI API version %s, supported: %s"
      name required_api_version (String.concat ~sep:"," supported_api_versions) in
    return (Error (Storage_interface.Exception.No_storage_plugin_for_sr msg))

module RRD = struct
  open Message_switch_async.Protocol_async

  let (>>|=) m f = m >>= function
    | `Ok x -> f x
    | `Error y ->
      let b = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer b in
      Client.pp_error fmt y;
      Format.pp_print_flush fmt ();
      raise (Failure (Buffer.contents b))

  let switch_rpc queue_name string_of_call response_of_string call =
    Client.connect ~switch:queue_name () >>|= fun t ->
    Client.rpc ~t ~queue:queue_name ~body:(string_of_call call) () >>|= fun s ->
    return (response_of_string s)

  let rpc = switch_rpc !Rrd_interface.queue_name Jsonrpc.string_of_call Jsonrpc.response_of_string
  module Client = Rrd_interface.RPC_API(Rpc_async.GenClient ())

end

let _nonpersistent = "NONPERSISTENT"
let _clone_on_boot_key = "clone-on-boot"
let _vdi_type_key = "vdi-type"
let _snapshot_time_key = "snapshot_time"
let _is_a_snapshot_key = "is_a_snapshot"
let _snapshot_of_key = "snapshot_of"

let is_executable path =
  Sys.is_file ~follow_symlinks:true path
  >>= function
  | `No | `Unknown ->
    return (Error (`missing path))
  | `Yes ->
    ( Unix.access path [ `Exec ]
      >>= function
      | Error exn ->
        return (Error (`not_executable (path, exn)))
      | Ok () -> return (Ok ())
    )

module Script = struct

  (** We cache (lowercase script name -> original script name) mapping for the
      scripts in the root directory of every registered plugin. *)
  let name_mapping = String.Table.create ~size:4 ()

  let update_mapping ~script_dir =
    Sys.readdir script_dir >>| Array.to_list >>| fun files ->
    (* If there are multiple files which map to the same lowercase string, we
       just take the first one, instead of failing *)
    let mapping =
      List.zip_exn files files
      |> Core.String.Caseless.Map.of_alist_reduce ~f:min
    in
    Hashtbl.set name_mapping ~key:script_dir ~data:mapping

  let path ~script_dir ~script_name =
    let find () =
      let cached_script_name =
        let (>>?=) = Option.(>>=) in
        Hashtbl.find name_mapping script_dir >>?= fun mapping ->
        Core.String.Caseless.Map.find mapping script_name
      in
      let script_name = Option.value cached_script_name ~default:script_name in
      let path = Filename.concat script_dir script_name in
      is_executable path >>| function
      | Ok () -> Ok path
      | Error _ as e -> e
    in
    find () >>= function
    | Ok path -> return (Ok path)
    | Error _ ->
      update_mapping ~script_dir >>= fun () ->
      find ()
end

let id = fun x -> x

(** Call the script named after the RPC method in the [script_dir]
    directory. The arguments (not the whole JSON-RPC call) are passed as JSON
    to its stdin, and stdout is returned. In case of a non-zero exit code,
    stdout is treated as the error report.
    The rest of the parameters are for compatiblity with the old PVS scripts:
    - The PVS storage scripts are missing some calls. If [missing] is [Some
      value], this [value] will be returned in case the required script is missing.
    - If [compat_in] or [compat_out] are defined, they will convert the input to
      the script and the output from the script, respectively, to ensure that
      the script can understand the input and that rpclib can unmarshal its
      output.
    This function either returns a successful RPC response, or raises
    Fork_exec_error with a suitable SMAPIv2 error if the call failed. *)
let fork_exec_rpc ~script_dir ?missing ?(compat_in=id) ?(compat_out=id) =
  let invoke_script call script_name =
    Process.create ~prog:script_name ~args:["--json"] ()
    >>= function
    | Error e ->
      error "%s failed: %s" script_name (Error.to_string_hum e);
      return (Error(backend_error "SCRIPT_FAILED" [ script_name; Error.to_string_hum e ]))
    | Ok p ->
      (* Send the request as json on stdin *)
      let w = Process.stdin p in
      (* We pass just the args, not the complete JSON-RPC call.
         Currently the Python code generated by rpclib requires all params to
         be named - they will be converted into a name->value Python dict.
         Rpclib currently puts all named params into a dict, so we expect
         params to be a single Dict, if all the params are named. *)
      (match call.R.params with
       | [R.Dict _ as d] ->
         return (Ok d)
       | _ -> return (Error (backend_error "INCORRECT_PARAMETERS" [ script_name; "All the call parameters should be named and should be in a RPC Dict" ]))
      ) >>>= fun args ->
      let args = compat_in args in
      Writer.write w (Jsonrpc.to_string args);
      Writer.close w
      >>= fun () ->
      Process.collect_output_and_wait p
      >>= fun output ->
      begin match output.Process.Output.exit_status with
        | Error (`Exit_non_zero code) ->
          (* Expect an exception and backtrace on stdout *)
          begin match Or_error.try_with (fun () -> Jsonrpc.of_string output.Process.Output.stdout) with
            | Error _ ->
              error "%s failed and printed bad error json: %s" script_name output.Process.Output.stdout;
              error "%s failed, stderr: %s" script_name output.Process.Output.stderr;
              return (Error (backend_error "SCRIPT_FAILED" [ script_name; "non-zero exit and bad json on stdout"; string_of_int code; output.Process.Output.stdout; output.Process.Output.stdout ]))
            | Ok response ->
              begin match Or_error.try_with (fun () -> error_of_rpc response) with
                | Error _ ->
                  error "%s failed and printed bad error json: %s" script_name output.Process.Output.stdout;
                  error "%s failed, stderr: %s" script_name output.Process.Output.stderr;
                  return (Error (backend_error "SCRIPT_FAILED" [ script_name; "non-zero exit and bad json on stdout"; string_of_int code; output.Process.Output.stdout; output.Process.Output.stdout ]))
                | Ok x -> return (Error(backend_backtrace_error x.code x.params x.backtrace))
              end
          end
        | Error (`Signal signal) ->
          error "%s caught a signal and failed" script_name;
          return (Error (backend_error "SCRIPT_FAILED" [ script_name; "signalled"; Signal.to_string signal; output.Process.Output.stdout; output.Process.Output.stdout ]))
        | Ok () ->

          (* Parse the json on stdout. We get back a JSON-RPC
             value from the scripts, not a complete JSON-RPC response *)
          begin match Or_error.try_with (fun () -> Jsonrpc.of_string output.Process.Output.stdout) with
            | Error _ ->
              error "%s succeeded but printed bad json: %s" script_name output.Process.Output.stdout;
              return (Error (backend_error "SCRIPT_FAILED" [ script_name; "bad json on stdout"; output.Process.Output.stdout ]))
            | Ok response ->
              info "%s succeeded: %s" script_name output.Process.Output.stdout;
              let response = compat_out response in
              let response = R.success response in
              return (Ok response)
          end
      end
  in

  let script_rpc call =
    info "%s" (Jsonrpc.string_of_call call);
    Script.path ~script_dir ~script_name:call.R.name >>= function
    | Error (`missing path) ->
      error "%s is not a file" path;
      (match missing with
       | None ->
         return (Error(backend_error "SCRIPT_MISSING" [ path; "Check whether the file exists and has correct permissions" ]))
       | Some m ->
         warn "Deprecated: script '%s' is missing, treating as no-op. Update your plugin!" path;
         return (Ok (R.success m)))
    | Error (`not_executable (path, exn)) ->
      error "%s is not executable" path;
      return (Error (backend_error "SCRIPT_NOT_EXECUTABLE" [ path; Exn.to_string exn ]))
    | Ok path -> invoke_script call path
  in

  (* The Errors we return from this function and the special error format
     returned by the scripts are not included in the error types of the various
     SMAPIv3 interfaces, therefore we have to propagate them as exceptions
     instead of returning an RPC call with an error, because rpclib would fail
     to unmarshal that error.
     Therefore we either return a successful RPC response, or raise
     Fork_exec_error with a suitable SMAPIv2 error if the call failed. *)
  let rpc call =
    script_rpc call >>= fun result ->
    Result.map_error ~f:(fun e -> Fork_exec_error e) result
    |> Result.ok_exn
    |> return
  in

  rpc

module Attached_SRs = struct
  type state = {
    sr: string;
    uids: string list;
  } [@@deriving sexp]

  let sr_table : state String.Table.t ref = ref (String.Table.create ())
  let state_path = ref None

  let add smapiv2 plugin uids =
    Hashtbl.set !sr_table ~key:smapiv2 ~data:{ sr = plugin; uids };
    ( match !state_path with
      | None ->
        return ()
      | Some path ->
        let contents = String.Table.sexp_of_t sexp_of_state !sr_table |> Sexplib.Sexp.to_string in
        let dir = Filename.dirname path in
        Unix.mkdir ~p:() dir >>= fun () ->
        Writer.save path ~contents
    ) >>= fun () ->
    return (Ok ())

  let find smapiv2 =
    match Hashtbl.find !sr_table smapiv2 with
    | None ->
      let open Storage_interface in
      return (Error (Exception.Sr_not_attached smapiv2))
    | Some { sr; _ } -> return (Ok sr)

  let get_uids smapiv2 =
    match Hashtbl.find !sr_table smapiv2 with
    | None ->
      let open Storage_interface in
      return (Error (Exception.Sr_not_attached smapiv2))
    | Some { uids; _ } -> return (Ok uids)

  let remove smapiv2 =
    Hashtbl.remove !sr_table smapiv2;
    return (Ok ())

  let reload path =
    state_path := Some path;
    Sys.is_file ~follow_symlinks:true path
    >>= function
    | `No | `Unknown ->
      return ()
    | `Yes ->
      Reader.file_contents path
      >>= fun contents ->
      sr_table := contents |> Sexplib.Sexp.of_string |> String.Table.t_of_sexp state_of_sexp;
      return ()
end

module Datapath_plugins = struct
  let table = String.Table.create ()

  let register ~datapath_root datapath_plugin_name =
    let result =
      let script_dir = Filename.concat datapath_root datapath_plugin_name in
      return_plugin_rpc (fun () -> Plugin_client.query (fork_exec_rpc ~script_dir) "register")
      >>>= fun response ->
      check_plugin_version_compatible response >>= function
      | Ok () ->
        info "Registered datapath plugin %s" datapath_plugin_name;
        Hashtbl.set table ~key:datapath_plugin_name ~data:(script_dir, response);
        return (Ok ())
      | Error e ->
        let err_msg = Storage_interface.Exception.rpc_of_exnty e |> Jsonrpc.to_string in
        info "Failed to register datapath plugin %s: %s" datapath_plugin_name err_msg;
        return (Error e)
    in
    (* We just do not register the plugin if we've encountered any error. In
       the future we might want to change that, so we keep the error result
       above. *)
    result >>= fun _ -> return ()

  let unregister datapath_plugin_name =
    Hashtbl.remove table datapath_plugin_name;
    return ()

  let supports_feature scheme feature =
    match Hashtbl.find table scheme with
    | None -> false
    | Some (_script_dir, query_result) -> List.mem query_result.Xapi_storage.Plugin.features feature ~equal:String.equal
end

let vdi_of_volume x =
  let find key ~default ~of_string =
    match List.Assoc.find x.Xapi_storage.Control.keys key ~equal:String.equal with
    | None -> default;
    | Some v -> v |> of_string
  in
  let find_string = find ~of_string:id in
  let open Storage_interface in {
  vdi = x.Xapi_storage.Control.key;
  uuid = x.Xapi_storage.Control.uuid;
  content_id = "";
  name_label = x.Xapi_storage.Control.name;
  name_description = x.Xapi_storage.Control.description;
  ty = find_string _vdi_type_key ~default:"";
  metadata_of_pool = "";
  is_a_snapshot = find _is_a_snapshot_key ~default:false ~of_string:bool_of_string;
  snapshot_time = find_string _snapshot_time_key ~default:"19700101T00:00:00Z";
  snapshot_of = find_string _snapshot_of_key ~default:"";
  read_only = not x.Xapi_storage.Control.read_write;
  cbt_enabled = false;
  virtual_size = x.Xapi_storage.Control.virtual_size;
  physical_utilisation = x.Xapi_storage.Control.physical_utilisation;
  sm_config = [];
  sharable = x.Xapi_storage.Control.sharable;
  persistent = true;
}

module Compat(V : sig val version : string option ref end) = struct
  (** Module for making the inputs and outputs compatible with the old PVS
      version of the storage scripts. *)

  let remove field rpc =
    match !V.version, rpc with
    | Some v, R.Dict d when v = pvs_version ->
        R.Dict (List.filter ~f:(fun (k,_) -> k <> field) d)
    | _ -> rpc

  let with_pvs_version f rpc = if !V.version = Some pvs_version then f rpc else rpc

  let add_param_to_input params =
    with_pvs_version (function
        (* Currently all parameters must be named. In this case, rpclib
           currently puts them into a Dict. *)
        | R.Dict d -> R.Dict (List.rev_append params d)
        | rpc -> rpc)

  let add_fields_to_dict fields =
    function R.Dict d -> R.Dict (List.rev_append fields d) | rpc -> rpc

  let add_fields_to_record_output fields =
    with_pvs_version (function
        | R.Dict _ as d -> add_fields_to_dict fields d
        | rpc -> rpc)

  let add_fields_to_record_list_output fields =
    with_pvs_version (function
        | R.Enum l -> R.Enum (List.map ~f:(add_fields_to_dict fields) l)
        | rpc -> rpc)

  (** Add the missing [sharable] field to the Dict in [rpc], to ensure the
      volume in the output match the new volume record type and is successfully
      parsed by rpclib. *)
  let compat_out_volume =
    add_fields_to_record_output ["sharable", R.Bool false]

  (** Add the missing [sharable] field to the Dicts in [rpc], to ensure the
      volumes in the output match the new volume record type and are
      successfully parsed by rpclib. *)
  let compat_out_volumes =
    add_fields_to_record_list_output ["sharable", R.Bool false]

  (** For the old PVS version, adds the uri parameter to the call from
      device_config, for newer versions, removes the uri key from device_config *)
  let compat_uri device_config =
    if !V.version = Some pvs_version then
      match List.Assoc.find ~equal:String.equal device_config "uri" with
      | None ->
        return (Error (missing_uri ()))
      | Some uri ->
        return (Ok (add_param_to_input ["uri", R.String uri]))
    else
        return (Ok id)

  (** Compatiblity for the old PVS version of SR.create, which had signature
      [uri -> name -> desc -> config -> unit] *)
  let sr_create device_config =
    compat_uri device_config >>>= fun compat_in ->
    let compat_out rpc =
      (* The PVS version will return nothing *)
      if rpc = R.Null then
        Rpcmarshal.marshal Xapi_storage.Control.typ_of_configuration device_config
      else rpc
    in
    return (Ok (device_config, compat_in, compat_out))
end

let choose_datapath ?(persistent = true) response =
  (* We can only use a URI with a valid scheme, since we use the scheme
     to name the datapath plugin. *)
  let possible =
    List.filter_map ~f:(fun x ->
      let uri = Uri.of_string x in
      match Uri.scheme uri with
      | None -> None
      | Some scheme -> Some (scheme, x)
    ) response.Xapi_storage.Control.uri in
  (* We can only use URIs whose schemes correspond to registered plugins *)
  let possible = List.filter_map
      ~f:(fun (scheme, uri) ->
          match Hashtbl.find Datapath_plugins.table scheme with
          | Some (script_dir, _query_result) -> Some (script_dir, scheme, uri)
          | None -> None
        )
      possible
  in
  (* If we want to be non-persistent, we prefer if the datapath plugin supports it natively *)
  let preference_order =
    if persistent
    then possible
    else
      let supports_nonpersistent, others = List.partition_tf ~f:(fun (_script_dir, scheme, _uri) ->
        Datapath_plugins.supports_feature scheme _nonpersistent
      ) possible in
      supports_nonpersistent @ others in
  match preference_order with
  | [] -> return (Error (missing_uri ()))
  | (script_dir, scheme, u) :: us -> return (Ok (fork_exec_rpc ~script_dir, scheme, u, "0"))

(* Process a message *)
let process_smapiv2_requests ~volume_script_dir =
  (* Each plugin has its own version, see the call to listen
     where `process` is partially applied. *)
  let version = ref None in
  let volume_rpc = fork_exec_rpc ~script_dir:volume_script_dir in
  let module Compat = Compat(struct let version = version end) in

  let stat ~dbg ~sr ~vdi =
    (* TODO add default value to sharable? *)
    return_volume_rpc (fun () -> Volume_client.stat (volume_rpc ~compat_out:Compat.compat_out_volume) dbg sr vdi)
  in
  let clone ~dbg ~sr ~vdi =
    return_volume_rpc (fun () -> Volume_client.clone volume_rpc dbg sr vdi)
  in
  let destroy ~dbg ~sr ~vdi =
    return_volume_rpc (fun () -> Volume_client.destroy volume_rpc dbg sr vdi)
  in
  let set ~dbg ~sr ~vdi ~key ~value =
    (* this is wrong, we loose the VDI type, but old pvsproxy didn't have
     * Volume.set and Volume.unset *)
    (* TODO handle this properly? *)
    let missing = if !version = Some pvs_version then Some (R.rpc_of_unit ()) else None in
    return_volume_rpc (fun () -> Volume_client.set (volume_rpc ?missing) dbg sr vdi key value)
  in
  let unset ~dbg ~sr ~vdi ~key =
    let missing = if !version = Some pvs_version then Some (R.rpc_of_unit ()) else None in
    return_volume_rpc (fun () -> Volume_client.unset (volume_rpc ?missing) dbg sr vdi key)
  in
  let update_keys ~dbg ~sr ~key ~value response =
    let open Deferred.Result.Monad_infix in
    match value with
    | None -> Deferred.Result.return response
    | Some value ->
       set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key ~value >>= fun () ->
       Deferred.Result.return { response with keys = (key, value) :: response.keys }
  in

  let vdi_attach_common args =
    let open Storage_interface in
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Attach.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Attach.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat ~dbg:args.Args.VDI.Attach.dbg ~sr ~vdi:args.Args.VDI.Attach.vdi
    >>= fun response ->
    (* If we have a clone-on-boot volume then use that instead *)
    ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        return (Ok response)
      | Some temporary ->
        stat ~dbg:args.Args.VDI.Attach.dbg ~sr ~vdi:temporary
    ) >>= fun response ->
    choose_datapath response
    >>= fun (rpc, datapath, uri, domain) ->
    return_data_rpc (fun () -> Datapath_client.attach rpc args.Args.VDI.Attach.dbg uri domain)
  in

  (* the actual API call for this plugin, sharing same version ref across all calls *)
  fun x ->
  let open Storage_interface in
  let call = Jsonrpc.call_of_string x in
  (match call with
  | { R.name = "Query.query"; R.params = [ args ] } ->
    let args = Args.Query.Query.request_of_rpc args in
    (* convert to new storage interface *)
    let dbg = args.Args.Query.Query.dbg in
    return_plugin_rpc (fun () -> Plugin_client.query volume_rpc dbg)
    >>>= fun response ->
    let required_api_version =
      response.Xapi_storage.Plugin.required_api_version in
    (* the first call to a plugin must be a Query.query that sets the version *)
    version := Some required_api_version;
    check_plugin_version_compatible response >>>= fun () ->
    (* Convert between the xapi-storage interface and the SMAPI *)
    let features = List.map ~f:(function
      | "VDI_DESTROY" -> "VDI_DELETE"
      | x -> x) response.Xapi_storage.Plugin.features in
    (* Look for executable scripts and automatically add capabilities *)
    let rec loop acc = function
      | [] -> return (Ok acc)
      | (script_name, capability) :: rest ->
        Script.path ~script_dir:volume_script_dir ~script_name >>= function
        | Error _ -> loop acc rest
        | Ok _ -> loop (capability :: acc) rest
    in
    loop [] [
      "SR.attach",       "SR_ATTACH";
      "SR.create",       "SR_CREATE";
      "SR.destroy",      "SR_DELETE";
      "SR.detach",       "SR_DETACH";
      "SR.ls",           "SR_SCAN";
      "SR.stat",         "SR_UPDATE";
      "SR.probe",        "SR_PROBE";
      "Volume.create",   "VDI_CREATE";
      "Volume.clone",    "VDI_CLONE";
      "Volume.snapshot", "VDI_SNAPSHOT";
      "Volume.resize",   "VDI_RESIZE";
      "Volume.destroy",  "VDI_DELETE";
      "Volume.stat",     "VDI_UPDATE";
    ]
    >>>= fun x ->
    let features = features @ x in
    (* Add the features we always have *)
    let features = features @ [
      "VDI_ATTACH"; "VDI_DETACH"; "VDI_ACTIVATE"; "VDI_DEACTIVATE";
      "VDI_INTRODUCE"
    ] in
    (* If we have the ability to clone a disk then we can provide
       clone on boot. *)
    let features =
      if List.mem features "VDI_CLONE" ~equal:String.equal
      then "VDI_RESET_ON_BOOT/2" :: features
      else features in
    let name = response.Xapi_storage.Plugin.name in
    let response = {
      driver = response.Xapi_storage.Plugin.plugin;
      name;
      description = response.Xapi_storage.Plugin.description;
      vendor = response.Xapi_storage.Plugin.vendor;
      copyright = response.Xapi_storage.Plugin.copyright;
      version = response.Xapi_storage.Plugin.version;
      required_api_version;
      features;
      configuration = response.Xapi_storage.Plugin.configuration;
      required_cluster_stack = response.Xapi_storage.Plugin.required_cluster_stack } in
    Deferred.Result.return (R.success (Args.Query.Query.rpc_of_response response))
  | { R.name = "Query.diagnostics"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.Query.Diagnostics.request_of_rpc args in
    let dbg = args.Args.Query.Diagnostics.dbg in
    return_plugin_rpc (fun () -> Plugin_client.diagnostics volume_rpc dbg)
    >>= fun response ->
    Deferred.Result.return (R.success (Args.Query.Diagnostics.rpc_of_response response))
  | { R.name = "SR.attach"; R.params = [ args ] } ->
    let args = Args.SR.Attach.request_of_rpc args in
    let device_config = args.Args.SR.Attach.device_config in
    Compat.compat_uri device_config >>>= fun compat_in ->
    let device_config =
      let uuid = args.Args.SR.Attach.sr in
      ("sr_uuid", uuid) :: device_config
    in
    return_volume_rpc (fun () -> Sr_client.attach (volume_rpc ~compat_in) args.Args.SR.Attach.dbg device_config)
    >>>= fun attach_response ->
    let sr = args.Args.SR.Attach.sr in
    (* Stat the SR to look for datasources *)
    (* SR.stat should take the attached URI *)
    return_volume_rpc (fun () -> Sr_client.stat volume_rpc args.Args.SR.Attach.dbg attach_response)
    >>>= fun stat ->
    let rec loop acc = function
      | [] -> return acc
      | datasource :: datasources ->
        let uri = Uri.of_string datasource in
        match Uri.scheme uri with
        | Some "xeno+shm" ->
          let uid = Uri.path uri in
          let uid = if String.length uid > 1 then String.sub uid ~pos:1 ~len:(String.length uid - 1) else uid in
          (RRD.Client.Plugin.Local.register RRD.rpc uid Rrd.Five_Seconds Rrd_interface.V2).Rpc_async.M.async
          >>= begin function
              | Ok  _ -> loop (uid :: acc) datasources
              | Error x -> raise Rrd_interface.(Rrdd_error x) end
        | _ ->
          loop acc datasources in
    loop [] stat.Xapi_storage.Control.datasources
    >>= fun uids ->
    (* associate the 'sr' from the plugin with the SR reference passed in *)
    Attached_SRs.add sr attach_response uids
    >>>= fun () ->
    Deferred.Result.return (R.success (Args.SR.Attach.rpc_of_response attach_response))
  | { R.name = "SR.detach"; R.params = [ args ] } ->
    let args = Args.SR.Detach.request_of_rpc args in
    begin Attached_SRs.find args.Args.SR.Detach.sr
    >>= function
    | Error _ ->
      (* ensure SR.detach is idempotent *)
      Deferred.Result.return (R.success (Args.SR.Detach.rpc_of_response ()))
    | Ok sr ->
      return_volume_rpc (fun () -> Sr_client.detach volume_rpc args.Args.SR.Detach.dbg sr)
      >>>= fun response ->
      Attached_SRs.get_uids args.Args.SR.Detach.sr
      >>>= fun uids ->
      let rec loop = function
      | [] -> return ()
      | datasource :: datasources ->
        let uri = Uri.of_string datasource in
        match Uri.scheme uri with
        | Some "xeno+shm" ->
          let uid = Uri.path uri in
          let uid = if String.length uid > 1 then String.sub uid ~pos:1 ~len:(String.length uid - 1) else uid in
          (RRD.Client.Plugin.Local.deregister RRD.rpc uid).Rpc_async.M.async
          >>= begin function
              | Ok _ -> loop datasources
              | Error x -> raise Rrd_interface.(Rrdd_error x) end
        | _ ->
          loop datasources in
      loop uids
      >>= fun () ->
      let open Deferred.Result.Monad_infix in
      Attached_SRs.remove args.Args.SR.Detach.sr
      >>= fun () ->
      Deferred.Result.return (R.success (Args.SR.Detach.rpc_of_response response))
    end
  | { R.name = "SR.probe"; R.params = [ args ] } ->
    let args = Args.SR.Probe.request_of_rpc args in
    let device_config = args.Args.SR.Probe.device_config in
    return_volume_rpc (fun () -> Sr_client.probe volume_rpc args.Args.SR.Probe.dbg device_config)
    >>>= fun response ->
        let pp_probe_result () probe_result =
          Rpcmarshal.marshal
            Xapi_storage.Control.typ_of_probe_result
            probe_result
          |> Jsonrpc.to_string
        in
        response
        |> List.map ~f:(fun probe_result ->
          let uuid = List.Assoc.find probe_result.Xapi_storage.Control.configuration ~equal:String.equal "sr_uuid" in
          let open Deferred.Or_error in
          let smapiv2_probe ?sr_info () =
            { configuration=probe_result.configuration; complete=probe_result.complete; sr=sr_info; extra_info=probe_result.extra_info }
          in
          match probe_result.Xapi_storage.Control.sr, probe_result.Xapi_storage.Control.complete, uuid with
          | _, false, Some _uuid ->
              errorf "A configuration with a uuid cannot be incomplete: %a" pp_probe_result probe_result
          | Some sr_stat, true, Some _uuid ->
              let sr_info = {
                Storage_interface.name_label = sr_stat.Xapi_storage.Control.name;
                sr_uuid = sr_stat.Xapi_storage.Control.uuid;
                name_description = sr_stat.Xapi_storage.Control.description;
                total_space = sr_stat.Xapi_storage.Control.total_space;
                free_space = sr_stat.Xapi_storage.Control.free_space;
                clustered = sr_stat.Xapi_storage.Control.clustered;
                health = match sr_stat.Xapi_storage.Control.health with
                  | Xapi_storage.Control.Healthy _ -> Healthy
                  | Xapi_storage.Control.Recovering _ -> Recovering
              } in
              return (smapiv2_probe ~sr_info ())
          | Some _sr, _, None ->
              errorf "A configuration is not attachable without a uuid: %a" pp_probe_result probe_result
          | None, false, None ->
              return (smapiv2_probe ())
          | None, true, _ ->
              return (smapiv2_probe ()))
        |> Deferred.Or_error.combine_errors
        |> Deferred.Result.map_error ~f:(fun err ->
            backend_error "SCRIPT_FAILED" ["SR.probe"; Error.to_string_hum err])
        >>>= fun results ->
        Storage_interface.Probe results |> Args.SR.Probe.rpc_of_response |> R.success |> Deferred.Result.return
  | { R.name = "SR.create"; R.params = [ args ] } ->
    let args = Args.SR.Create.request_of_rpc args in
    let name_label = args.Args.SR.Create.name_label in
    let uuid = args.Args.SR.Create.sr in
    let description = args.Args.SR.Create.name_description in
    let device_config = args.Args.SR.Create.device_config in
    Compat.sr_create device_config >>>= fun (device_config, compat_in, compat_out) ->
    return_volume_rpc (fun () ->
        Sr_client.create (volume_rpc ~compat_in ~compat_out)
          args.Args.SR.Create.dbg uuid device_config name_label description)
    >>>= fun new_device_config ->
      Deferred.Result.return (R.success (Args.SR.Create.rpc_of_response new_device_config))
  | { R.name = "SR.set_name_label"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.SR.Set_name_label.request_of_rpc args in
    Attached_SRs.find args.Args.SR.Set_name_label.sr
    >>= fun sr ->
    let name_label = args.Args.SR.Set_name_label.new_name_label in
    let dbg = args.Args.SR.Set_name_label.dbg in
    return_volume_rpc (fun () -> Sr_client.set_name volume_rpc dbg sr name_label)
    >>= fun () ->
    Deferred.Result.return (R.success (Args.SR.Set_name_label.rpc_of_response ()))
  | { R.name = "SR.set_name_description"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.SR.Set_name_description.request_of_rpc args in
    Attached_SRs.find args.Args.SR.Set_name_description.sr
    >>= fun sr ->
    let name_description = args.Args.SR.Set_name_description.new_name_description in
    let dbg = args.Args.SR.Set_name_description.dbg in
    return_volume_rpc (fun () -> Sr_client.set_description volume_rpc dbg sr name_description)
    >>= fun () ->
    Deferred.Result.return (R.success (Args.SR.Set_name_label.rpc_of_response ()))
  | { R.name = "SR.destroy"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.SR.Destroy.request_of_rpc args in
    Attached_SRs.find args.Args.SR.Destroy.sr
    >>= fun sr ->
    return_volume_rpc (fun () -> Sr_client.destroy volume_rpc args.Args.SR.Destroy.dbg sr)
    >>= fun response ->
    Deferred.Result.return (R.success (Args.SR.Destroy.rpc_of_response response))
  | { R.name = "SR.scan"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.SR.Scan.request_of_rpc args in
    Attached_SRs.find args.Args.SR.Scan.sr
    >>= fun sr ->
    return_volume_rpc (fun () -> Sr_client.ls (volume_rpc ~compat_out:Compat.compat_out_volumes) args.Args.SR.Scan.dbg sr)
    >>>= fun response ->
    let response = Array.to_list response in
    (* Filter out volumes which are clone-on-boot transients *)
    let transients = List.fold ~f:(fun set x ->
      match List.Assoc.find x.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None -> set
      | Some transient -> Set.add set transient
    ) ~init:(Core.String.Set.empty) response in
    let response = List.filter ~f:(fun x -> not(Set.mem transients x.Xapi_storage.Control.key)) response in
    let response = List.map ~f:vdi_of_volume response in
    Deferred.Result.return (R.success (Args.SR.Scan.rpc_of_response response))
  | { R.name = "VDI.create"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Create.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Create.sr
    >>= fun sr ->
    let vdi_info = args.Args.VDI.Create.vdi_info in
    let dbg = args.Args.VDI.Create.dbg in
    return_volume_rpc (fun () ->
        Volume_client.create
          (volume_rpc ~compat_out:Compat.compat_out_volume)
          dbg
          sr
          vdi_info.name_label
          vdi_info.name_description
          vdi_info.virtual_size
          vdi_info.sharable)
    >>= update_keys ~dbg ~sr ~key:_vdi_type_key ~value:(match vdi_info.ty with "" -> None | s -> Some s)
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Create.rpc_of_response response))
  | { R.name = "VDI.destroy"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Destroy.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Destroy.sr
    >>= fun sr ->
    stat ~dbg:args.Args.VDI.Destroy.dbg ~sr ~vdi:args.Args.VDI.Destroy.vdi
    >>= fun response ->
    (* Destroy any clone-on-boot volume that might exist *)
    ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        return (Ok ())
      | Some temporary ->
        (* Destroy the temporary disk we made earlier *)
        destroy ~dbg:args.Args.VDI.Destroy.dbg ~sr ~vdi:temporary
    ) >>= fun () ->
    destroy ~dbg:args.Args.VDI.Destroy.dbg ~sr ~vdi:args.Args.VDI.Destroy.vdi
    >>= fun () ->
    Deferred.Result.return (R.success (Args.VDI.Destroy.rpc_of_response ()))
  | { R.name = "VDI.snapshot"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Snapshot.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Snapshot.sr
    >>= fun sr ->
    let dbg = args.Args.VDI.Snapshot.dbg in
    let vdi_info = args.Args.VDI.Snapshot.vdi_info in
    let vdi = vdi_info.vdi in
    return_volume_rpc (fun () -> Volume_client.snapshot volume_rpc dbg sr vdi)
    >>= fun response ->
    let now = Xapi_stdext_date.Date.(to_string (of_float (Unix.gettimeofday ()))) in
    set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key:_snapshot_time_key ~value:now >>= fun () ->
    set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key:_is_a_snapshot_key ~value:(string_of_bool true) >>= fun () ->
    set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key:_snapshot_of_key ~value:vdi >>= fun () ->
    let response = { (vdi_of_volume response) with snapshot_time = now; is_a_snapshot = true; snapshot_of = vdi } in
    Deferred.Result.return (R.success (Args.VDI.Snapshot.rpc_of_response response))
  | { R.name = "VDI.clone"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Clone.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Clone.sr
    >>= fun sr ->
    let vdi_info = args.Args.VDI.Clone.vdi_info in
    clone ~dbg:args.Args.VDI.Clone.dbg ~sr ~vdi:vdi_info.vdi
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Clone.rpc_of_response response))
  | { R.name = "VDI.set_name_label"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Set_name_label.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Set_name_label.sr
    >>= fun sr ->
    let vdi = args.Args.VDI.Set_name_label.vdi in
    let new_name_label = args.Args.VDI.Set_name_label.new_name_label in
    let dbg = args.Args.VDI.Set_name_label.dbg in
    return_volume_rpc (fun () -> Volume_client.set_name volume_rpc dbg sr vdi new_name_label)
    >>= fun () ->
    Deferred.Result.return (R.success (Args.VDI.Set_name_label.rpc_of_response ()))
  | { R.name = "VDI.set_name_description"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Set_name_description.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Set_name_description.sr
    >>= fun sr ->
    let vdi = args.Args.VDI.Set_name_description.vdi in
    let new_name_description = args.Args.VDI.Set_name_description.new_name_description in
    let dbg = args.Args.VDI.Set_name_description.dbg in
    return_volume_rpc (fun () -> Volume_client.set_description volume_rpc dbg sr vdi new_name_description)
    >>= fun () ->
    Deferred.Result.return (R.success (Args.VDI.Set_name_description.rpc_of_response ()))
  | { R.name = "VDI.resize"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Resize.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Resize.sr
    >>= fun sr ->
    let vdi = args.Args.VDI.Resize.vdi in
    let new_size = args.Args.VDI.Resize.new_size in
    let dbg = args.Args.VDI.Resize.dbg in
    return_volume_rpc (fun () -> Volume_client.resize volume_rpc dbg sr vdi new_size)
    >>= fun () ->
    (* Now call Volume.stat to discover the size *)
    stat ~dbg ~sr ~vdi
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Resize.rpc_of_response response.Xapi_storage.Control.virtual_size))
  | { R.name = "VDI.stat"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Stat.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Stat.sr
    >>= fun sr ->
    let vdi = args.Args.VDI.Stat.vdi in
    stat ~dbg:args.Args.VDI.Stat.dbg ~sr ~vdi
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Stat.rpc_of_response response))
  | { R.name = "VDI.introduce"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Introduce.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Introduce.sr
    >>= fun sr ->
    let vdi = args.Args.VDI.Introduce.location in
    stat ~dbg:args.Args.VDI.Introduce.dbg ~sr ~vdi
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Introduce.rpc_of_response response))
  | { R.name = "VDI.attach2"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    vdi_attach_common args >>= fun response ->
    let convert_implementation = function
      | Xapi_storage.Data.XenDisk { params; extra; backend_type } -> XenDisk { params; extra; backend_type }
      | Xapi_storage.Data.BlockDevice { path } -> BlockDevice { path }
      | Xapi_storage.Data.File { path } -> File { path }
      | Xapi_storage.Data.Nbd { uri } -> Nbd { uri }
    in
    let convert_backend = function Xapi_storage.Data.{ implementations } ->
      { implementations = List.map ~f:convert_implementation response.Xapi_storage.Data.implementations
      }
    in
    Deferred.Result.return (R.success (Args.VDI.Attach2.rpc_of_response (convert_backend response)))
  | { R.name = "VDI.activate"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Activate.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Activate.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat ~dbg:args.Args.VDI.Activate.dbg ~sr ~vdi:args.Args.VDI.Activate.vdi
    >>= fun response ->
    (* If we have a clone-on-boot volume then use that instead *)
    ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        return (Ok response)
      | Some temporary ->
        stat ~dbg:args.Args.VDI.Activate.dbg ~sr ~vdi:temporary
    ) >>= fun response ->
    choose_datapath response
    >>= fun (rpc, datapath, uri, domain) ->
    return_data_rpc (fun () -> Datapath_client.activate rpc args.Args.VDI.Activate.dbg uri domain)
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Activate.rpc_of_response ()))
  | { R.name = "VDI.deactivate"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Deactivate.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Deactivate.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat ~dbg:args.Args.VDI.Deactivate.dbg ~sr ~vdi:args.Args.VDI.Deactivate.vdi
    >>= fun response ->
    ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        return (Ok response)
      | Some temporary ->
        stat ~dbg:args.Args.VDI.Deactivate.dbg ~sr ~vdi:temporary
    ) >>= fun response ->
    choose_datapath response
    >>= fun (rpc, datapath, uri, domain) ->
    return_data_rpc (fun () -> Datapath_client.deactivate rpc args.Args.VDI.Deactivate.dbg uri domain)
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Deactivate.rpc_of_response ()))
  | { R.name = "VDI.detach"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Detach.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Detach.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat ~dbg:args.Args.VDI.Detach.dbg ~sr ~vdi:args.Args.VDI.Detach.vdi
    >>= fun response ->
    ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        return (Ok response)
      | Some temporary ->
        stat ~dbg:args.Args.VDI.Detach.dbg ~sr ~vdi:temporary
    ) >>= fun response ->
    choose_datapath response
    >>= fun (rpc, datapath, uri, domain) ->
    return_data_rpc (fun () -> Datapath_client.detach rpc args.Args.VDI.Detach.dbg uri domain)
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Detach.rpc_of_response ()))
  | { R.name = "SR.stat"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.SR.Stat.request_of_rpc args in
    Attached_SRs.find args.Args.SR.Stat.sr
    >>= fun sr ->
    return_volume_rpc (fun () -> Sr_client.stat volume_rpc args.Args.SR.Stat.dbg sr)
    >>= fun response ->
    let response = {
      sr_uuid = response.Xapi_storage.Control.uuid;
      name_label = response.Xapi_storage.Control.name;
      name_description = response.Xapi_storage.Control.description;
      total_space = response.Xapi_storage.Control.total_space;
      free_space = response.Xapi_storage.Control.free_space;
      clustered = response.Xapi_storage.Control.clustered;
      health = match response.Xapi_storage.Control.health with
        | Xapi_storage.Control.Healthy _ -> Healthy
        | Xapi_storage.Control.Recovering _ -> Recovering
        ;
    } in
    Deferred.Result.return (R.success (Args.SR.Stat.rpc_of_response response))
  | { R.name = "VDI.epoch_begin"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Epoch_begin.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Epoch_begin.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    let persistent = args.Args.VDI.Epoch_begin.persistent in
    stat ~dbg:args.Args.VDI.Epoch_begin.dbg ~sr ~vdi:args.Args.VDI.Epoch_begin.vdi
    >>= fun response ->
    choose_datapath ~persistent response
    >>= fun (rpc, datapath, uri, domain) ->
    (* If non-persistent and the datapath plugin supports NONPERSISTENT
       then we delegate this to the datapath plugin. Otherwise we will
       make a temporary clone now and attach/detach etc this file. *)
    if Datapath_plugins.supports_feature datapath _nonpersistent then begin
      (* We delegate handling non-persistent disks to the datapath plugin. *)
      return_data_rpc (fun () -> Datapath_client.open_ rpc args.Args.VDI.Epoch_begin.dbg uri persistent)
      >>= fun () ->
      Deferred.Result.return (R.success (Args.VDI.Epoch_begin.rpc_of_response ()))
    end else if not persistent then begin
      (* We create a non-persistent disk here with Volume.clone, and store
         the name of the cloned disk in the metadata of the original. *)
      ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
        | None ->
          return (Ok ())
        | Some temporary ->
          (* Destroy the temporary disk we made earlier *)
          destroy ~dbg:args.Args.VDI.Epoch_begin.dbg ~sr ~vdi:temporary
      ) >>= fun () ->
      clone ~dbg:args.Args.VDI.Epoch_begin.dbg ~sr ~vdi:args.Args.VDI.Epoch_begin.vdi
      >>= fun vdi ->
      set ~dbg:args.Args.VDI.Epoch_begin.dbg ~sr ~vdi:args.Args.VDI.Epoch_begin.vdi ~key:_clone_on_boot_key ~value:vdi.Xapi_storage.Control.key
      >>= fun () ->
      Deferred.Result.return (R.success (Args.VDI.Epoch_begin.rpc_of_response ()))
    end else Deferred.Result.return (R.success (Args.VDI.Epoch_begin.rpc_of_response ()))
  | { R.name = "VDI.epoch_end"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Epoch_end.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Epoch_end.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat ~dbg:args.Args.VDI.Epoch_end.dbg ~sr ~vdi:args.Args.VDI.Epoch_end.vdi
    >>= fun response ->
    choose_datapath response
    >>= fun (rpc, datapath, uri, domain) ->
    if Datapath_plugins.supports_feature datapath _nonpersistent then begin
      return_data_rpc (fun () -> Datapath_client.close rpc args.Args.VDI.Epoch_end.dbg uri)
      >>= fun () ->
      Deferred.Result.return (R.success (Args.VDI.Epoch_end.rpc_of_response ()))
    end else begin
      match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        Deferred.Result.return (R.success (Args.VDI.Epoch_end.rpc_of_response ()))
      | Some temporary ->
        (* Destroy the temporary disk we made earlier *)
        destroy ~dbg:args.Args.VDI.Epoch_end.dbg ~sr ~vdi:temporary
        >>= fun () ->
        unset ~dbg:args.Args.VDI.Epoch_end.dbg ~sr ~vdi:args.Args.VDI.Epoch_end.vdi ~key:_clone_on_boot_key
        >>= fun () ->
        Deferred.Result.return (R.success (Args.VDI.Epoch_end.rpc_of_response ()))
    end
  | { R.name = "VDI.set_persistent"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    (* We don't do anything until the VDI.epoch_begin *)
    Deferred.Result.return (R.success (Args.VDI.Set_persistent.rpc_of_response ()))
  | { R.name = name; _ } ->
    Deferred.return (Error (backend_error "UNIMPLEMENTED" [ name ])))
  >>= function
  | Result.Error error ->
    let response = R.failure (Exception.rpc_of_exnty error) in
    info "returning error %s" (Jsonrpc.string_of_response response);
    return (Jsonrpc.string_of_response response)
  | Result.Ok rpc ->
    return (Jsonrpc.string_of_response rpc)

(** Active servers, one per sub-directory of the volume_root_dir *)
let servers = String.Table.create () ~size:4

(* XXX: need a better error-handling strategy *)
let get_ok = function
  | `Ok x -> x
  | `Error e ->
    let b = Buffer.create 16 in
    let fmt = Format.formatter_of_buffer b in
    Message_switch_unix.Protocol_unix.Server.pp_error fmt e;
    Format.pp_print_flush fmt ();
    failwith (Buffer.contents b)


let rec diff a b = match a with
  | [] -> []
  | a :: aa ->
    if List.mem b a ~equal:(=) then diff aa b else a :: (diff aa b)

let watch_volume_plugins ~volume_root ~switch_path ~pipe =
  let create volume_plugin_name =
    if Hashtbl.mem servers volume_plugin_name
      then return ()
      else begin
        info "Adding %s" volume_plugin_name;
        let volume_script_dir = Filename.concat volume_root volume_plugin_name in
        Message_switch_async.Protocol_async.Server.listen ~process:(process_smapiv2_requests ~volume_script_dir) ~switch:switch_path ~queue:(Filename.basename volume_plugin_name) ()
        >>= fun result ->
        let server = get_ok result in
        Hashtbl.add_exn servers ~key:volume_plugin_name ~data:server;
        return ()
      end in
  let destroy volume_plugin_name =
    info "Removing %s" volume_plugin_name;
    if Hashtbl.mem servers volume_plugin_name then begin
      let t = Hashtbl.find_exn servers volume_plugin_name in
      Message_switch_async.Protocol_async.Server.shutdown ~t () >>= fun () ->
      Hashtbl.remove servers volume_plugin_name;
      return ()
    end else return () in
  let sync () =
    Sys.readdir volume_root
    >>= fun names ->
    let needed : string list = Array.to_list names in
    let got_already : string list = Hashtbl.keys servers in
    Deferred.all_unit (List.map ~f:create (diff needed got_already))
    >>= fun () ->
    Deferred.all_unit (List.map ~f:destroy (diff got_already needed)) in
  sync ()
  >>= fun () ->
  let open Async_inotify.Event in
  let rec loop () =
    ( Pipe.read pipe >>= function
    | `Eof ->
      info "Received EOF from inotify event pipe";
      Shutdown.exit 1
    | `Ok (Created path)
    | `Ok (Moved (Into path)) ->
      create (Filename.basename path)
    | `Ok (Unlinked path)
    | `Ok (Moved (Away path)) ->
      destroy (Filename.basename path)
    | `Ok (Modified _) ->
      return ()
    | `Ok (Moved (Move (path_a, path_b))) ->
      destroy (Filename.basename path_a)
      >>= fun () ->
      create (Filename.basename path_b)
    | `Ok Queue_overflow ->
      sync ()
    ) >>= fun () ->
    loop () in
  loop ()

let watch_datapath_plugins ~datapath_root ~pipe =
  let sync () =
    Sys.readdir datapath_root
    >>= fun names ->
    let needed : string list = Array.to_list names in
    let got_already : string list = Hashtbl.keys servers in
    Deferred.all_unit (List.map ~f:(Datapath_plugins.register ~datapath_root) (diff needed got_already))
    >>= fun () ->
    Deferred.all_unit (List.map ~f:Datapath_plugins.unregister (diff got_already needed)) in
  sync ()
  >>= fun () ->
  let open Async_inotify.Event in
  let rec loop () =
    ( Pipe.read pipe >>= function
    | `Eof ->
      info "Received EOF from inotify event pipe";
      Shutdown.exit 1
    | `Ok (Created path)
    | `Ok (Moved (Into path)) ->
      Datapath_plugins.register ~datapath_root (Filename.basename path)
    | `Ok (Unlinked path)
    | `Ok (Moved (Away path)) ->
      Datapath_plugins.unregister (Filename.basename path)
    | `Ok (Modified _) ->
      return ()
    | `Ok (Moved (Move (path_a, path_b))) ->
      Datapath_plugins.unregister (Filename.basename path_a)
      >>= fun () ->
      Datapath_plugins.register ~datapath_root (Filename.basename path_b)
    | `Ok Queue_overflow ->
      sync ()
    ) >>= fun () ->
    loop () in
  loop ()

let self_test_plugin ~root_dir plugin =
  let volume_script_dir = Filename.(concat (concat root_dir "volume") plugin) in
  let process = process_smapiv2_requests ~volume_script_dir in
  let module Test = Storage_interface.ClientM(struct
    type 'a t = 'a Deferred.t
    let return = return
    let bind a f = Deferred.bind a ~f
    let fail = raise
    let rpc call =
      call |> Jsonrpc.string_of_call |> process
      >>= fun r ->
      debug "RPC: %s" r;
      return (Jsonrpc.response_of_string r)
  end) in
  let dbg = "debug" in
  Monitor.try_with (fun () ->
    Test.Query.query ~dbg >>= fun query_result ->
    Test.Query.diagnostics ~dbg >>= fun _msg ->

    let sr = "dummySR" in
    let name_label = "dummy name" in
    let name_description = "dummy description" in
    let device_config = ["uri", "file:///dev/null"] in
    let physical_size = 0L in
    Test.SR.create ~dbg ~sr ~name_label ~name_description ~device_config ~physical_size >>= fun device_config ->
    Test.SR.detach ~dbg ~sr >>= fun () ->
    Test.SR.attach ~dbg ~sr ~device_config >>= fun () ->

    let vdi_info = {
      Storage_interface.vdi = "vdi-uuid-1";
      uuid = None;
      content_id = "";
      name_label = "vdi name";
      name_description = "vdi description";
      ty = "redolog";
      metadata_of_pool = "";
      is_a_snapshot = false;
      snapshot_time = "";
      snapshot_of = "";
      read_only = false;
      cbt_enabled = false;
      virtual_size = 0L;
      physical_utilisation = 0L;
      persistent = false;
      sm_config = [];
      sharable = false;
    } in

    Test.VDI.create ~dbg ~sr ~vdi_info >>= fun vdi_info ->
    Test.VDI.stat ~dbg ~sr ~vdi:vdi_info.vdi >>= fun _vdi_info ->
    Test.VDI.destroy ~dbg ~sr ~vdi:vdi_info.vdi >>= fun () ->

    Test.SR.stat ~dbg ~sr >>= fun _sr_info ->
    Test.SR.scan ~dbg ~sr >>= fun _sr_list ->

    if List.mem query_result.features "SR_PROBE" ~equal:String.equal then
      Test.SR.probe ~dbg ~queue:plugin~device_config ~sm_config:[] >>= fun result ->
      return ()
    else
      return ()
    )

let self_test ~root_dir =
  (self_test_plugin ~root_dir "org.xen.xapi.storage.dummy"
   >>>= fun () ->
   self_test_plugin ~root_dir "org.xen.xapi.storage.dummyv5")
  >>= function
    | Ok () ->
        info "test thread shutdown cleanly";
        Async_unix.exit 0
    | Error x ->
        error "test thread failed with %s" (Exn.to_string x);
        Async_unix.exit 2

let main ~root_dir ~state_path ~switch_path =
  Attached_SRs.reload state_path
  >>= fun () ->
  let datapath_root = Filename.concat root_dir "datapath" in
  Async_inotify.create ~recursive:false ~watch_new_dirs:false datapath_root
  >>= fun (watch, _) ->
  let datapath = Async_inotify.pipe watch in
  let volume_root = Filename.concat root_dir "volume" in
  Async_inotify.create ~recursive:false ~watch_new_dirs:false volume_root
  >>= fun (watch, _) ->
  let volume = Async_inotify.pipe watch in

  let rec loop () =
    Monitor.try_with
      (fun () ->
        Deferred.all_unit [
          watch_volume_plugins ~volume_root ~switch_path ~pipe:volume;
          watch_datapath_plugins ~datapath_root ~pipe:datapath
        ]
      )
    >>= function
    | Ok () ->
      info "main thread shutdown cleanly";
      return ()
    | Error x ->
      error "main thread failed with %s" (Exn.to_string x);
      Clock.after (Time.Span.of_sec 5.) >>= fun () ->
      loop () in
  loop ()

open Xcp_service

let description = String.concat ~sep:" " [
  "Allow xapi storage adapters to be written as individual scripts.";
  "To add a storage adapter, create a sub-directory in the --root directory";
  "with the name of the adapter (e.g. org.xen.xcp.storage.mylvm) and place";
  "the scripts inside.";
]

type backend_error = string * (string list) [@@deriving sexp]

(** registers pretty printers for `Backend_error*` exceptions.
 *  Otherwise we only log Backend_error_with_backtrace(_), with the pretty
 *  printer we can log the actual arguments of the exception.
 * *)
let register_exn_pretty_printers () =
  Sexplib.Conv.Exn_converter.add ~finalise:false [%extension_constructor Storage_interface.Backend_error]
  (function
    | Storage_interface.Backend_error e -> sexp_of_backend_error e
    | _ -> assert false);
  Sexplib.Conv.Exn_converter.add ~finalise:false [%extension_constructor Storage_interface.Backend_error_with_backtrace]
  (function
    | Storage_interface.Backend_error_with_backtrace e -> sexp_of_backend_error e
    | _ -> assert false)

let _ =
  register_exn_pretty_printers ();
  let root_dir = ref "/var/lib/xapi/storage-scripts" in
  let state_path = ref "/var/run/nonpersistent/xapi-storage-script/state.db" in

  let resources = [
    { Xcp_service.name = "root";
      description = "directory whose sub-directories contain sets of per-operation scripts, one sub-directory per queue name";
      essential = true;
      path = root_dir;
      perms = [ U.X_OK ];
    }; { Xcp_service.name = "state";
      description = "file containing attached SR information, should be deleted on host boot";
      essential = false;
      path = state_path;
      perms = [ ];
    }
  ] in

  let self_test_only = ref false in
  let options = [
    "self-test-only", Arg.Set self_test_only,    (fun () -> string_of_bool !self_test_only), "Do only a self-test and exit";
  ] in

  (match configure2
    ~name:"xapi-script-storage"
    ~version:Version.version
    ~doc:description
    ~resources
    ~options
    () with
  | `Ok () -> ()
  | `Error x ->
    error "Error: %s\n%!" x;
    Pervasives.exit 1);

  if !Xcp_service.daemon then begin
    Xcp_service.maybe_daemonize ();
    use_syslog := true;
    info "Daemonisation successful.";
  end;
  let (_: unit Deferred.t) =
    let rec loop () =
      Monitor.try_with
        (fun () ->
          if !self_test_only then
            self_test ~root_dir:!root_dir
          else
            main ~root_dir:!root_dir ~state_path:!state_path ~switch_path:!Xcp_client.switch_path
        )
      >>= function
      | Ok () ->
        info "main thread shutdown cleanly";
        return ()
      | Error x ->
        error "main thread failed with %s" (Exn.to_string x);
        Clock.after (Time.Span.of_sec 5.) >>= fun () ->
        loop () in
    loop () in
  never_returns (Scheduler.go ())

