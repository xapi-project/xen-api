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
exception Fork_exec_error of Storage_interface.Errors.error

let backend_error name args =
  let open Storage_interface in
  Storage_interface.Errors.Backend_error (name, args)

let backend_backtrace_error name args backtrace =
  let open Storage_interface in
  match args with
  | ["Activated_on_another_host"; uuid] ->
    Storage_interface.Errors.Activated_on_another_host(uuid)
  | _ ->
    let backtrace = rpc_of_backtrace backtrace |> Jsonrpc.to_string in
    Storage_interface.Errors.Backend_error_with_backtrace(name, backtrace :: args)

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

let id = fun x -> x

type compat_in = R.t -> R.t
(** A function that changes the input to make it compatible with an older
    script *)

type compat_out = R.t -> R.t
(** A function that changes the output of an older script to make it
    compatible with the new interface and ensure it is unmarshalled without
    error. *)

module Compat(V : sig val version : string option ref end) : sig
  (** Module for making the inputs and outputs compatible with the old PVS
      version of the storage scripts. *)

  type device_config = (Core.String.t, string) Core.List.Assoc.t

  val compat_out_volume : compat_out
  (** Add the missing [sharable] field to the Dict in [rpc], to ensure the
      volume in the output match the new volume record type and is successfully
      parsed by rpclib. *)

  val compat_out_volumes : compat_out
  (** Add the missing [sharable] field to the Dicts in [rpc], to ensure the
      volumes in the output match the new volume record type and are
      successfully parsed by rpclib. *)

  val sr_create : device_config -> (device_config * compat_in * compat_out, Storage_interface.Errors.error) Deferred.Result.t
  (** Compatiblity for the old PVS version of SR.create, which had signature
      [uri -> name -> desc -> config -> unit] *)

  val sr_attach : device_config -> (compat_in, Storage_interface.Errors.error) Deferred.Result.t
  (** Compatiblity for the old PVS version of SR.attach, which had signature
      [uri -> sr (=string)] *)
end = struct

  type device_config = (Core.String.t, string) Core.List.Assoc.t
  type compat_in = R.t -> R.t
  type compat_out = R.t -> R.t

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

  let compat_out_volume =
    add_fields_to_record_output ["sharable", R.Bool false]

  let compat_out_volumes =
    add_fields_to_record_list_output ["sharable", R.Bool false]

  (** Adds the uri parameter to the call from device_config when talking to the
      old PVS scripts *)
  let compat_uri device_config =
    if !V.version = Some pvs_version then
      match List.Assoc.find ~equal:String.equal device_config "uri" with
      | None ->
        return (Error (missing_uri ()))
      | Some uri ->
        return (Ok (add_param_to_input ["uri", R.String uri]))
    else
      return (Ok id)

  let sr_create device_config =
    compat_uri device_config >>>= fun compat_in ->
    let compat_out =
      if !V.version = Some pvs_version then begin
        fun rpc ->
          (* The PVS version will return nothing *)
          if rpc = R.Null then
            Rpcmarshal.marshal Xapi_storage.Control.typ_of_configuration device_config
          else rpc
      end
      else id
    in
    return (Ok (device_config, compat_in, compat_out))

  let sr_attach = compat_uri
end

let check_plugin_version_compatible query_result =
  let Xapi_storage.Plugin.{ name; required_api_version; _ } = query_result in
  if required_api_version <> api_max then
    warn "Using deprecated SMAPIv3 API version %s, latest is %s. Update your %s plugin!" required_api_version api_max name;
  if List.mem ~equal:String.equal supported_api_versions required_api_version then
    Deferred.Result.return ()
  else
    let msg = Printf.sprintf "%s requires unknown SMAPI API version %s, supported: %s"
        name required_api_version (String.concat ~sep:"," supported_api_versions) in
    return (Error (Storage_interface.Errors.No_storage_plugin_for_sr msg))

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
let fork_exec_rpc : script_dir:string -> ?missing:R.t -> ?compat_in:compat_in -> ?compat_out:compat_out -> R.call -> R.response Deferred.t =
  fun ~script_dir ?missing ?(compat_in=id) ?(compat_out=id) ->
    let invoke_script call script_name : (R.response, Storage_interface.Errors.error) Deferred.Result.t =
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

    let script_rpc call : (R.response, Storage_interface.Errors.error) Deferred.Result.t =
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
    let rpc : R.call -> R.response Deferred.t = fun call ->
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
      return (Error (Errors.Sr_not_attached smapiv2))
    | Some { sr; _ } -> return (Ok sr)

  let get_uids smapiv2 =
    match Hashtbl.find !sr_table smapiv2 with
    | None ->
      let open Storage_interface in
      return (Error (Errors.Sr_not_attached smapiv2))
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
        let err_msg = Storage_interface.(rpc_of Errors.error) e |> Jsonrpc.to_string in
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


(* Bind the implementations *)
let bind ~volume_script_dir =
  (* Each plugin has its own version, see the call to listen
     where `process` is partially applied. *)
  let module S = Storage_interface.StorageAPI(Rpc_async.GenServer ()) in

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

  let vdi_attach_common dbg sr vdi =
    let open Deferred.Result.Monad_infix in
    Attached_SRs.find sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat ~dbg ~sr ~vdi
    >>= fun response ->
    (* If we have a clone-on-boot volume then use that instead *)
    ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        return (Ok response)
      | Some temporary ->
        stat ~dbg ~sr ~vdi:temporary
    ) >>= fun response ->
    choose_datapath response
    >>= fun (rpc, datapath, uri, domain) ->
    return_data_rpc (fun () -> Datapath_client.attach rpc dbg uri domain)
  in

  let wrap th = Rpc_async.M.{async=th} in
  (* the actual API call for this plugin, sharing same version ref across all calls *) 
  let query_impl = (fun dbg ->
      let th =
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
        Deferred.Result.return {
            Storage_interface.driver = response.Xapi_storage.Plugin.plugin;
            name;
            description = response.Xapi_storage.Plugin.description;
            vendor = response.Xapi_storage.Plugin.vendor;
            copyright = response.Xapi_storage.Plugin.copyright;
            version = response.Xapi_storage.Plugin.version;
            required_api_version;
            features;
            configuration = response.Xapi_storage.Plugin.configuration;
            required_cluster_stack = response.Xapi_storage.Plugin.required_cluster_stack } in
      wrap th)
  in
  S.Query.query query_impl;

  let query_diagnostics_impl dbg =
    let th =
      let open Deferred.Result.Monad_infix in
      return_plugin_rpc (fun () -> Plugin_client.diagnostics volume_rpc dbg)
      >>= fun response ->
      Deferred.Result.return response
    in
    wrap th
  in
  S.Query.diagnostics query_diagnostics_impl;

  let sr_attach_impl dbg sr device_config =
    let th =
      Compat.sr_attach device_config >>>= fun compat_in ->
      return_volume_rpc (fun () -> Sr_client.attach (volume_rpc ~compat_in) dbg device_config)
      >>>= fun attach_response ->
      (* Stat the SR to look for datasources *)
      (* SR.stat should take the attached URI *)
      return_volume_rpc (fun () -> Sr_client.stat volume_rpc dbg attach_response)
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
      Deferred.Result.return ()
    in wrap th
  in
  S.SR.attach sr_attach_impl;

  let sr_detach_impl dbg sr =
    let th =
      Attached_SRs.find sr
      >>= function
      | Error _ ->
        (* ensure SR.detach is idempotent *)
        Deferred.Result.return ()
      | Ok sr ->
        return_volume_rpc (fun () -> Sr_client.detach volume_rpc dbg sr)
        >>>= fun response ->
        Attached_SRs.get_uids sr
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
        Attached_SRs.remove sr
        >>= fun () ->
        Deferred.Result.return response
    in wrap th in
  S.SR.detach sr_detach_impl;

  let sr_probe_impl dbg queue device_config sm_config =
    let th =
      return_volume_rpc (fun () -> Sr_client.probe volume_rpc dbg device_config)
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
        { Storage_interface.configuration=probe_result.configuration; complete=probe_result.complete; sr=sr_info; extra_info=probe_result.extra_info }
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
      Deferred.Result.return (Storage_interface.Probe results)
    in wrap th in
  S.SR.probe sr_probe_impl;

  let sr_create_impl dbg uuid name_label description device_config size =
    let th =
      Compat.sr_create device_config >>>= fun (device_config, compat_in, compat_out) ->
      return_volume_rpc (fun () ->
        Sr_client.create (volume_rpc ~compat_in ~compat_out)
        dbg uuid device_config name_label description)
      >>>= fun new_device_config ->
      Deferred.Result.return new_device_config
    in wrap th
  in
  S.SR.create sr_create_impl;

  let sr_set_name_label_impl dbg sr new_name_label =
    (Attached_SRs.find sr >>>= fun sr ->
    return_volume_rpc (fun () -> Sr_client.set_name volume_rpc dbg sr new_name_label))
    |> wrap in
  S.SR.set_name_label sr_set_name_label_impl;

  let sr_set_name_description_impl dbg sr new_name_description =
    (Attached_SRs.find sr >>>= fun sr ->
    return_volume_rpc (fun () -> Sr_client.set_description volume_rpc dbg sr new_name_description))
    |> wrap in
  S.SR.set_name_description sr_set_name_description_impl;

  let sr_destroy_impl dbg sr =
    (Attached_SRs.find sr >>>= fun sr ->
    return_volume_rpc (fun () -> Sr_client.destroy volume_rpc dbg sr))
    |> wrap in
  S.SR.destroy sr_destroy_impl;

  let sr_scan_impl dbg sr =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      return_volume_rpc (fun () -> Sr_client.ls (volume_rpc ~compat_out:Compat.compat_out_volumes) dbg sr)
      >>>= fun response ->
      let response = Array.to_list response in
      (* Filter out volumes which are clone-on-boot transients *)
      let transients = List.fold ~f:(fun set x ->
        match List.Assoc.find x.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
        | None -> set
        | Some transient -> Set.add set transient
        ) ~init:(Core.String.Set.empty) response in
      let response = List.filter ~f:(fun x -> not(Set.mem transients x.Xapi_storage.Control.key)) response in
      Deferred.Result.return (List.map ~f:vdi_of_volume response)
    end |> wrap
  in
  S.SR.scan sr_scan_impl;

  let vdi_create_impl dbg sr (vdi_info : Storage_interface.vdi_info) =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      return_volume_rpc (fun () ->
        Volume_client.create
          (volume_rpc ~compat_out:Compat.compat_out_volume)
          dbg
          sr
          vdi_info.Storage_interface.name_label
          vdi_info.name_description
          vdi_info.virtual_size
          vdi_info.sharable)
      >>>= update_keys ~dbg ~sr ~key:_vdi_type_key ~value:(match vdi_info.ty with "" -> None | s -> Some s)
      >>>= fun response ->
      Deferred.Result.return (vdi_of_volume response)
    end |> wrap
  in
  S.VDI.create vdi_create_impl;

  let vdi_destroy_impl dbg sr vdi =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      stat ~dbg ~sr ~vdi >>>= fun response ->
      (* Destroy any clone-on-boot volume that might exist *)
      ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
      | None ->
        return (Ok ())
      | Some temporary ->
        (* Destroy the temporary disk we made earlier *)
        destroy ~dbg ~sr ~vdi
      ) >>>= fun () ->
      destroy ~dbg ~sr ~vdi
    end |> wrap
  in
  S.VDI.destroy vdi_destroy_impl;

  let vdi_snapshot_impl dbg sr vdi_info =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      let vdi = vdi_info.Storage_interface.vdi in
      return_volume_rpc (fun () -> Volume_client.snapshot volume_rpc dbg sr vdi)
      >>>= fun response ->
      let now = Xapi_stdext_date.Date.(to_string (of_float (Unix.gettimeofday ()))) in
      set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key:_snapshot_time_key ~value:now >>>= fun () ->
      set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key:_is_a_snapshot_key ~value:(string_of_bool true) >>>= fun () ->
      set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key:_snapshot_of_key ~value:vdi >>>= fun () ->
      let response = { (vdi_of_volume response) with snapshot_time = now; is_a_snapshot = true; snapshot_of = vdi } in
      Deferred.Result.return response
    end |> wrap
  in
  S.VDI.snapshot vdi_snapshot_impl;

  let vdi_clone_impl dbg sr vdi_info =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      clone ~dbg ~sr ~vdi:vdi_info.Storage_interface.vdi
      >>>= fun response ->
      Deferred.Result.return (vdi_of_volume response)
    end |> wrap
  in
  S.VDI.clone vdi_clone_impl;

  let vdi_set_name_label_impl dbg sr vdi new_name_label =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      return_volume_rpc (fun () -> Volume_client.set_name volume_rpc dbg sr vdi new_name_label)
    end |> wrap
  in
  S.VDI.set_name_label vdi_set_name_label_impl;

  let vdi_set_name_description_impl dbg sr vdi new_name_description =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      return_volume_rpc (fun () -> Volume_client.set_description volume_rpc dbg sr vdi new_name_description)
    end |> wrap
  in
  S.VDI.set_name_description vdi_set_name_description_impl;

  let vdi_resize_impl dbg sr vdi new_size =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      return_volume_rpc (fun () -> Volume_client.resize volume_rpc dbg sr vdi new_size) >>>= fun () ->
      (* Now call Volume.stat to discover the size *)
      stat ~dbg ~sr ~vdi
      >>>= fun response ->
      Deferred.Result.return response.Xapi_storage.Control.virtual_size
    end |> wrap
  in
  S.VDI.resize vdi_resize_impl;

  let vdi_stat_impl dbg sr vdi =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      stat ~dbg ~sr ~vdi >>>= fun response ->
      Deferred.Result.return (vdi_of_volume response)
    end |> wrap
  in
  S.VDI.stat vdi_stat_impl;

  let vdi_introduce_impl dbg sr uuid sm_config location =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      let vdi = location in
      stat ~dbg ~sr ~vdi >>>= fun response ->
      Deferred.Result.return (vdi_of_volume response)
    end |> wrap
  in
  S.VDI.introduce vdi_introduce_impl;

  let vdi_attach2_impl dbg _dp sr vdi _readwrite =
    begin
      vdi_attach_common dbg sr vdi >>>= fun response ->
      let convert_implementation = function
      | Xapi_storage.Data.XenDisk { params; extra; backend_type } -> Storage_interface.XenDisk { params; extra; backend_type }
      | BlockDevice { path } -> BlockDevice { path }
      | File { path } -> File { path }
      | Nbd { uri } -> Nbd { uri }
      in
      Deferred.Result.return { Storage_interface.implementations = List.map ~f:convert_implementation response.Xapi_storage.Data.implementations }
    end |> wrap
  in
  S.VDI.attach2 vdi_attach2_impl;

  let vdi_activate_impl dbg _dp sr vdi =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      (* Discover the URIs using Volume.stat *)
      stat ~dbg ~sr ~vdi >>>= fun response ->
      (* If we have a clone-on-boot volume then use that instead *)
      ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
        | None ->
          return (Ok response)
        | Some temporary ->
          stat ~dbg ~sr ~vdi:temporary
      ) >>>= fun response ->
      choose_datapath response
      >>>= fun (rpc, datapath, uri, domain) ->
      return_data_rpc (fun () -> Datapath_client.activate rpc dbg uri domain)
    end |> wrap
  in
  S.VDI.activate vdi_activate_impl;

  let vdi_deactivate_impl dbg _dp sr vdi =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      (* Discover the URIs using Volume.stat *)
      stat ~dbg ~sr ~vdi >>>= fun response ->
      ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
        | None ->
          return (Ok response)
        | Some temporary ->
          stat ~dbg ~sr ~vdi:temporary) >>>= fun response ->
      choose_datapath response >>>= fun (rpc, datapath, uri, domain) ->
      return_data_rpc (fun () -> Datapath_client.deactivate rpc dbg uri domain)
    end |> wrap
  in
  S.VDI.deactivate vdi_deactivate_impl;

  let vdi_detach_impl dbg _dp sr vdi =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      (* Discover the URIs using Volume.stat *)
      stat ~dbg ~sr ~vdi >>>= fun response ->
      ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
        | None ->
          return (Ok response)
        | Some temporary ->
          stat ~dbg ~sr ~vdi:temporary) >>>= fun response ->
      choose_datapath response >>>= fun (rpc, datapath, uri, domain) ->
      return_data_rpc (fun () -> Datapath_client.detach rpc dbg uri domain)
    end |> wrap
  in
  S.VDI.detach vdi_detach_impl;

  let sr_stat_impl dbg sr =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      return_volume_rpc (fun () -> Sr_client.stat volume_rpc dbg sr)
      >>>= fun response ->
      Deferred.Result.return {
        Storage_interface.sr_uuid = response.Xapi_storage.Control.uuid;
        name_label = response.Xapi_storage.Control.name;
        name_description = response.Xapi_storage.Control.description;
        total_space = response.Xapi_storage.Control.total_space;
        free_space = response.Xapi_storage.Control.free_space;
        clustered = response.Xapi_storage.Control.clustered;
        health = match response.Xapi_storage.Control.health with
          | Xapi_storage.Control.Healthy _ -> Healthy
          | Xapi_storage.Control.Recovering _ -> Recovering
        ;
      }
    end |> wrap
  in
  S.SR.stat sr_stat_impl;

  let vdi_epoch_begin_impl dbg sr vdi persistent =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      (* Discover the URIs using Volume.stat *)
      stat ~dbg ~sr ~vdi >>>= fun response ->
      choose_datapath ~persistent response >>>= fun (rpc, datapath, uri, domain) ->
      (* If non-persistent and the datapath plugin supports NONPERSISTENT
         then we delegate this to the datapath plugin. Otherwise we will
         make a temporary clone now and attach/detach etc this file. *)
      if Datapath_plugins.supports_feature datapath _nonpersistent then begin
        (* We delegate handling non-persistent disks to the datapath plugin. *)
        return_data_rpc (fun () -> Datapath_client.open_ rpc dbg uri persistent)
      end else if not persistent then begin
        (* We create a non-persistent disk here with Volume.clone, and store
           the name of the cloned disk in the metadata of the original. *)
        ( match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
          | None ->
            Deferred.Result.return ()
          | Some temporary ->
            (* Destroy the temporary disk we made earlier *)
            destroy ~dbg ~sr ~vdi:temporary
        ) >>>= fun () ->
        clone ~dbg ~sr ~vdi >>>= fun vdi' ->
        set ~dbg ~sr ~vdi ~key:_clone_on_boot_key ~value:vdi'.Xapi_storage.Control.key
      end else Deferred.Result.return ()
    end |> wrap
  in
  S.VDI.epoch_begin vdi_epoch_begin_impl;

  let vdi_epoch_end_impl dbg sr vdi =
    begin
      Attached_SRs.find sr >>>= fun sr ->
      (* Discover the URIs using Volume.stat *)
      stat ~dbg ~sr ~vdi >>>= fun response ->
      choose_datapath response >>>= fun (rpc, datapath, uri, domain) ->
      if Datapath_plugins.supports_feature datapath _nonpersistent then begin
        return_data_rpc (fun () -> Datapath_client.close rpc dbg uri)
      end else begin
        match List.Assoc.find response.Xapi_storage.Control.keys _clone_on_boot_key ~equal:String.equal with
        | None ->
          Deferred.Result.return ()
        | Some temporary ->
          (* Destroy the temporary disk we made earlier *)
          destroy ~dbg ~sr ~vdi:temporary >>>= fun () ->
          unset ~dbg ~sr ~vdi ~key:_clone_on_boot_key >>>= fun () ->
          Deferred.Result.return ()
      end
    end |> wrap
  in
  S.VDI.epoch_end vdi_epoch_end_impl;

  let vdi_set_persistent_impl dbg sr vdi persistent =
    Deferred.Result.return () |> wrap
  in
  S.VDI.set_persistent vdi_set_persistent_impl;

  let u = fun _ -> failwith "Unimplemented" in
  S.get_by_name u;
  S.VDI.compose u;
  S.VDI.get_by_name u;
  S.DATA.MIRROR.receive_start u;
   S.SR.reset u;
   S.UPDATES.get u;
   S.SR.update_snapshot_info_dest u;
   S.VDI.data_destroy u;
   S.DATA.MIRROR.list u;
   S.TASK.stat u;
   S.VDI.remove_from_sm_config u;
   S.DP.diagnostics u;
   S.TASK.destroy u;
   S.VDI.list_changed_blocks u;
   S.DP.destroy u;
   S.VDI.add_to_sm_config u;
   S.VDI.similar_content u;
   S.DATA.copy u;
   S.DP.stat_vdi u;
   S.DATA.MIRROR.receive_finalize u;
   S.DP.create u;
   S.VDI.set_content_id u;
   S.VDI.disable_cbt u;
   S.DP.attach_info u;
   S.TASK.cancel u;
   S.SR.list u;
   S.VDI.attach u;
   S.DATA.MIRROR.stat u;
   S.TASK.list u;
   S.VDI.get_url u;
   S.VDI.enable_cbt u;
   S.DATA.MIRROR.start u;
   S.Policy.get_backend_vm u;
   S.DATA.copy_into u;
   S.DATA.MIRROR.receive_cancel u;
   S.SR.update_snapshot_info_src u;
   S.DATA.MIRROR.stop u;
  Rpc_async.server S.implementation

let process_smapiv2_requests server txt =
  let request = Jsonrpc.call_of_string txt in
  server request >>= fun response ->
  Deferred.return (Jsonrpc.string_of_response response)

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

      Message_switch_async.Protocol_async.Server.listen ~process:(process_smapiv2_requests (bind ~volume_script_dir)) ~switch:switch_path ~queue:(Filename.basename volume_plugin_name) ()
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
  let process = process_smapiv2_requests (bind ~volume_script_dir) in
  let rpc call = call |> Jsonrpc.string_of_call |> process
        >>= fun r ->
        debug "RPC: %s" r;
        return (Jsonrpc.response_of_string r)
  in
  let module Test = Storage_interface.StorageAPI(Rpc_async.GenClient()) in
  let dbg = "debug" in
  Monitor.try_with (fun () ->
      let open Rpc_async.M in
      begin
        Test.Query.query rpc dbg >>= fun query_result ->
        Test.Query.diagnostics rpc dbg >>= fun _msg ->

      let sr = "dummySR" in
      let name_label = "dummy name" in
      let name_description = "dummy description" in
      let device_config = ["uri", "file:///dev/null"] in
      let physical_size = 0L in
      Test.SR.create rpc dbg sr name_label name_description device_config physical_size >>= fun device_config ->
      Test.SR.detach rpc dbg sr >>= fun () ->
      Test.SR.attach rpc dbg sr device_config >>= fun () ->

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

      Test.VDI.create rpc dbg sr vdi_info >>= fun vdi_info ->
      Test.VDI.stat rpc dbg sr vdi_info.vdi >>= fun _vdi_info ->
      Test.VDI.destroy rpc dbg sr vdi_info.vdi >>= fun () ->

      Test.SR.stat rpc dbg sr >>= fun _sr_info ->
      Test.SR.scan rpc dbg sr >>= fun _sr_list ->

      if List.mem query_result.features "SR_PROBE" ~equal:String.equal then
        Test.SR.probe rpc dbg plugin device_config [] >>= fun result ->
        return ()
      else
        return ()
      end |> deferred)
    >>= function | Ok x -> Async.Deferred.return x | Error y -> failwith "self test failed"

let self_test ~root_dir =
  (self_test_plugin ~root_dir "org.xen.xapi.storage.dummy"
   >>>= fun () ->
   self_test_plugin ~root_dir "org.xen.xapi.storage.dummyv5")
  >>= function
  | Ok () ->
    info "test thread shutdown cleanly";
    Async_unix.exit 0
  | Error x ->
    error "test thread failed with %s" (Storage_interface.(rpc_of Errors.error) x |> Jsonrpc.to_string);
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
  Sexplib.Conv.Exn_converter.add ~finalise:false [%extension_constructor Storage_interface.Storage_error]
    (function
      | Storage_interface.Storage_error (Backend_error e) -> sexp_of_backend_error e
      | Storage_interface.Storage_error (Backend_error_with_backtrace e) -> sexp_of_backend_error e
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

