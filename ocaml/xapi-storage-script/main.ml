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
module R = Rpc
module Plugin_client = Xapi_storage.Plugin.Plugin (Rpc_lwt.GenClient ())
module Volume_client = Xapi_storage.Control.Volume (Rpc_lwt.GenClient ())
module Sr_client = Xapi_storage.Control.Sr (Rpc_lwt.GenClient ())
module Datapath_client = Xapi_storage.Data.Datapath (Rpc_lwt.GenClient ())
open Private.Lib

let ( >>= ) = Lwt.bind

let ( let* ) = Lwt.bind

let ( >>| ) = Fun.flip Lwt.map

let ( >>>= ) = Lwt_result.bind

let return = Lwt_result.return

let fail = Lwt_result.fail

let ( // ) = Filename.concat

module Deferred = struct
  let errorf fmt =
    Printf.ksprintf (fun m -> Lwt.return (Base.Or_error.error_string m)) fmt

  let combine_errors lst = Lwt.all lst >>| Base.Or_error.combine_errors

  let try_with f = Lwt.try_bind f return fail
end

type config = {mutable use_observer: bool}

let config = {use_observer= false}

(** Functions for returning SMAPIv2 errors *)

(** Exception returned by fork_exec_rpc when the script invocation fails *)
exception Fork_exec_error of Storage_interface.Errors.error

let backend_error name args =
  let open Storage_interface in
  Errors.Backend_error (name, args)

let backend_backtrace_error name args backtrace =
  let open Storage_interface in
  match args with
  | ["Activated_on_another_host"; uuid] ->
      Errors.Activated_on_another_host uuid
  | _ ->
      let backtrace = Types.rpc_of_backtrace backtrace |> Jsonrpc.to_string in
      Errors.Backend_error_with_backtrace (name, backtrace :: args)

let missing_uri () =
  backend_error "MISSING_URI" ["Please include a URI in the device-config"]

(** return a unique 'domain' string for Dom0, so that we can plug disks
  multiple times (e.g. for copy).

  XAPI should give us a unique 'dp' (datapath) string, e.g. a UUID for storage migration,
  or vbd/domid/device.
  For regular guests keep the domain as passed by XAPI (an integer).
 *)
let domain_of ~dp ~vm' =
  let vm = Storage_interface.Vm.string_of vm' in
  match vm with
  | "0" ->
      (* SM tries to use this in filesystem paths, so cannot have /,
         and systemd might be a bit unhappy with - *)
      "u0-" ^ dp |> String.map (function '/' | '-' -> '_' | c -> c)
  | _ ->
      vm

(** Functions to wrap calls to the above client modules and convert their
    exceptions and errors into SMAPIv2 errors of type
    [Storage_interface.Exception.exnty]. The above client modules should only
    be used with these functions, otherwise the Fork_exec_error exception
    raised by fork_exec_rpc will not be caught and the main thread will fail. *)

(* fork_exec_rpc either raises a Fork_exec_error exception or
   returns a successful RPC response *)
let return_rpc typ result =
  Lwt.catch
    (fun () ->
      (* We need to delay the evaluation of [result] until now, because
         when fork_exec_rpc is called by GenClient.declare, it
         might immediately raise a Fork_exec_error *)
      Fun.flip Lwt.map
        (Rpc_lwt.T.get (result ()))
        (* In practice we'll always get a successful RPC response here (Ok),
           but we still have to transform the Error to make the types match: *)
        (Base.Result.map_error ~f:(fun err ->
             backend_error "SCRIPT_RETURNED_RPC_ERROR"
               [Rpcmarshal.marshal typ err |> R.to_string]
         )
        )
    )
    (function
      | Fork_exec_error err ->
          fail err
      | e ->
          let msg = ["Unexpected exception:" ^ Base.Exn.to_string e] in
          fail (backend_error "SCRIPT_FAILED" msg)
      )

let return_volume_rpc result =
  return_rpc Xapi_storage.Control.typ_of_exns result

let return_plugin_rpc result = return_rpc Xapi_storage.Common.typ_of_exnt result

let return_data_rpc result = return_rpc Xapi_storage.Common.typ_of_exnt result

(* Reporter taken from
   https://erratique.ch/software/logs/doc/Logs_lwt/index.html#report_ex
   under ISC License *)
let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b
    , fun () ->
        let m = Buffer.contents b in
        Buffer.reset b ; m
    )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  (* The default pretty-printer adds the binary name to the loglines, which
     results in appearing twice per logline, override it instead *)
  let pp_header =
    let pf = Format.fprintf in
    let pp_header ppf (l, h) =
      if l = Logs.App then
        match h with None -> () | Some h -> pf ppf "[%s] " h
      else
        match h with
        | None ->
            pf ppf "[%a] " Logs.pp_level l
        | Some h ->
            pf ppf "[%s] " h
    in
    pp_header
  in
  let reporter = Logs.format_reporter ~app ~dst ~pp_header () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App ->
            Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ ->
            Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over () |> Lwt.return in
      Lwt.finalize write unblock |> Lwt.ignore_result ;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  {Logs.report}

let debug = Logs_lwt.debug

let info = Logs_lwt.info

let warn = Logs_lwt.warn

let error = Logs_lwt.err

let pvs_version = "3.0"

let supported_api_versions = [pvs_version; "5.0"]

let api_max = List.fold_left Base.String.max "" supported_api_versions

(** A function that changes the input to make it compatible with an older
    script *)
type compat_in = R.t -> R.t

(** A function that changes the output of an older script to make it
    compatible with the new interface and ensure it is unmarshalled without
    error. *)
type compat_out = R.t -> R.t

module Compat (V : sig
  val version : string option ref
end) : sig
  (** Module for making the inputs and outputs compatible with the old PVS
      version of the storage scripts. *)

  type device_config = (string * string) list

  val compat_out_volume : compat_out
  (** Add the missing [sharable] field to the Dict in [rpc], to ensure the
      volume in the output match the new volume record type and is successfully
      parsed by rpclib. *)

  val compat_out_volumes : compat_out
  (** Add the missing [sharable] field to the Dicts in [rpc], to ensure the
      volumes in the output match the new volume record type and are
      successfully parsed by rpclib. *)

  val sr_create :
       device_config
    -> ( device_config * compat_in * compat_out
       , Storage_interface.Errors.error
       )
       Lwt_result.t
  (** Compatiblity for the old PVS version of SR.create, which had signature
      [uri -> name -> desc -> config -> unit] *)

  val sr_attach :
    device_config -> (compat_in, Storage_interface.Errors.error) Lwt_result.t
  (** Compatiblity for the old PVS version of SR.attach, which had signature
      [uri -> sr (=string)] *)
end = struct
  type device_config = (string * string) list

  let with_pvs_version f rpc =
    match !V.version with
    | Some v when Base.String.(v = pvs_version) ->
        f rpc
    | _ ->
        rpc

  let add_param_to_input params =
    with_pvs_version (function
      (* Currently all parameters must be named. In this case, rpclib
         currently puts them into a Dict. *)
      | R.Dict d ->
          R.Dict (List.rev_append params d)
      | rpc ->
          rpc
      )

  let add_fields_to_dict fields = function
    | R.Dict d ->
        R.Dict (List.rev_append fields d)
    | rpc ->
        rpc

  let add_fields_to_record_output fields =
    with_pvs_version (function
      | R.Dict _ as d ->
          add_fields_to_dict fields d
      | rpc ->
          rpc
      )

  let add_fields_to_record_list_output fields =
    with_pvs_version (function
      | R.Enum l ->
          R.Enum (List.map (add_fields_to_dict fields) l)
      | rpc ->
          rpc
      )

  let compat_out_volume =
    add_fields_to_record_output [("sharable", R.Bool false)]

  let compat_out_volumes =
    add_fields_to_record_list_output [("sharable", R.Bool false)]

  (** Adds the uri parameter to the call from device_config when talking to the
      old PVS scripts *)
  let compat_uri device_config =
    match !V.version with
    | Some version when Base.String.(version = pvs_version) -> (
      match Base.List.Assoc.find ~equal:String.equal device_config "uri" with
      | None ->
          fail (missing_uri ())
      | Some uri ->
          return (add_param_to_input [("uri", R.String uri)])
    )
    | _ ->
        return Fun.id

  let sr_create device_config =
    compat_uri device_config >>>= fun compat_in ->
    let compat_out =
      match !V.version with
      | Some v when Base.String.(v = pvs_version) -> (
          function
          (* The PVS version will return nothing *)
          | R.Null ->
              Rpcmarshal.marshal Xapi_storage.Control.typ_of_configuration
                device_config
          | rpc ->
              rpc
        )
      | _ ->
          Fun.id
    in
    return (device_config, compat_in, compat_out)

  let sr_attach = compat_uri
end

let check_plugin_version_compatible query_result =
  let Xapi_storage.Plugin.{name; required_api_version; _} = query_result in
  ( if Base.String.(required_api_version <> api_max) then
      warn (fun m ->
          m
            "Using deprecated SMAPIv3 API version %s, latest is %s. Update \
             your %s plugin!"
            required_api_version api_max name
      )
    else
      Lwt.return_unit
  )
  >>= fun () ->
  if List.mem required_api_version supported_api_versions then
    return ()
  else
    let msg =
      Printf.sprintf "%s requires unknown SMAPI API version %s, supported: %s"
        name required_api_version
        (String.concat "," supported_api_versions)
    in
    fail (Storage_interface.Errors.No_storage_plugin_for_sr msg)

module RRD = struct
  open Message_switch_lwt.Protocol_lwt

  let ( >>|= ) m f =
    m >>= fun x ->
    Client.error_to_msg x
    |> Result.fold ~ok:f ~error:(function `Msg err -> failwith err)

  let switch_rpc queue_name string_of_call response_of_string call =
    Client.connect ~switch:queue_name () >>|= fun t ->
    Client.rpc ~t ~queue:queue_name ~body:(string_of_call call) () >>|= fun s ->
    Lwt.return (response_of_string s)

  let rpc =
    switch_rpc !Rrd_interface.queue_name Jsonrpc.string_of_call
      Jsonrpc.response_of_string

  module Client = Rrd_interface.RPC_API (Rpc_lwt.GenClient ())
end

let _nonpersistent = "NONPERSISTENT"

let _clone_on_boot_key = "clone-on-boot"

let _vdi_type_key = "vdi-type"

let _snapshot_time_key = "snapshot_time"

let _is_a_snapshot_key = "is_a_snapshot"

let _snapshot_of_key = "snapshot_of"

module Script = struct
  (** We cache (lowercase script name -> original script name) mapping for the
      scripts in the root directory of every registered plugin. *)
  let name_mapping = Base.Hashtbl.create ~size:4 (module Base.String)

  let update_mapping ~script_dir =
    Sys.readdir script_dir >>= fun files ->
    (* If there are multiple files which map to the same lowercase string, we
       just take the first one, instead of failing *)
    let mapping =
      List.combine files files
      |> Base.Map.of_alist_reduce
           (module Base.String.Caseless)
           ~f:Base.String.min
    in
    return @@ Base.Hashtbl.set name_mapping ~key:script_dir ~data:mapping

  let path ~script_dir ~script_name =
    let find () =
      let cached_script_name =
        let ( let* ) = Option.bind in
        let* mapping = Base.Hashtbl.find name_mapping script_dir in
        Base.Map.find mapping script_name
      in
      let script_name = Option.value cached_script_name ~default:script_name in
      let path = script_dir // script_name in
      Sys.assert_is_executable path >>>= fun () -> return path
    in
    find () >>= function
    | Ok path ->
        return path
    | Error _ ->
        update_mapping ~script_dir >>>= fun () -> find ()
end

let observer_config_dir =
  let ( // ) = Filename.concat in
  let dir, component =
    Constants.(observer_config_dir, observer_component_smapi)
  in
  dir // component // "enabled"

(** Determine if SM API observation is enabled from the
    filesystem. Ordinarily, determining if a component is enabled
    would consist of querying the 'components' field of an observer
    from the xapi database. *)
let observer_is_component_enabled () =
  let is_enabled () =
    let is_config_file path = Filename.check_suffix path ".observer.conf" in
    let* files = Sys.readdir observer_config_dir in
    Lwt.return (List.exists is_config_file files)
  in
  let* result = Deferred.try_with is_enabled in
  Lwt.return (Option.value (Result.to_option result) ~default:false)

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
let fork_exec_rpc :
       script_dir:string
    -> ?missing:R.t
    -> ?compat_in:compat_in
    -> ?compat_out:compat_out
    -> ?dbg:string
    -> R.call
    -> R.response Lwt.t =
 fun ~script_dir ?missing ?(compat_in = Fun.id) ?(compat_out = Fun.id) ?dbg ->
  let invoke_script call script_name :
      (R.response, Storage_interface.Errors.error) Lwt_result.t =
    let traceparent = Option.bind dbg Debug_info.traceparent_of_dbg in
    let args = ["--json"] in
    let script_name, args, env =
      match (traceparent, config.use_observer) with
      | Some traceparent, true ->
          ( Constants.python3_path
          , "-m" :: "observer" :: script_name :: args
          , [
              ("TRACEPARENT", traceparent)
            ; ("OBSERVER_CONFIG_DIR", observer_config_dir)
            ; ("PYTHONPATH", Filename.dirname script_name)
            ]
          )
      | _ ->
          (script_name, args, [])
    in
    (* We pass just the args, not the complete JSON-RPC call.
       Currently the Python code generated by rpclib requires all params to
       be named - they will be converted into a name->value Python dict.
       Rpclib currently puts all named params into a dict, so we expect
       params to be a single Dict, if all the params are named. *)
    ( match call.R.params with
    | [(R.Dict _ as d)] ->
        return d
    | _ ->
        fail
          (backend_error "INCORRECT_PARAMETERS"
             [
               script_name
             ; "All the call parameters should be named and should be in a RPC \
                Dict"
             ]
          )
    )
    >>>= fun input ->
    let input = compat_in input |> Jsonrpc.to_string in
    debug (fun m -> m "Running %s" @@ Filename.quote_command script_name args)
    >>= fun () ->
    Process.run ~env ~prog:script_name ~args ~input >>= fun output ->
    let fail_because ~cause description =
      fail
        (backend_error "SCRIPT_FAILED"
           [
             script_name
           ; description
           ; cause
           ; output.Process.Output.stdout
           ; output.Process.Output.stdout
           ]
        )
    in
    match output.Process.Output.exit_status with
    | Error (Exit_non_zero code) -> (
      (* Expect an exception and backtrace on stdout *)
      match
        Base.Or_error.try_with (fun () ->
            Jsonrpc.of_string output.Process.Output.stdout
        )
      with
      | Error _ ->
          error (fun m ->
              m "%s[%d] failed and printed bad error json: %s" script_name
                output.pid output.Process.Output.stdout
          )
          >>= fun () ->
          error (fun m ->
              m "%s[%d] failed, stderr: %s" script_name output.pid
                output.Process.Output.stderr
          )
          >>= fun () ->
          fail_because "non-zero exit and bad json on stdout"
            ~cause:(string_of_int code)
      | Ok response -> (
        match
          Base.Or_error.try_with (fun () -> Types.error_of_rpc response)
        with
        | Error _ ->
            error (fun m ->
                m "%s[%d] failed and printed bad error json: %s" script_name
                  output.pid output.Process.Output.stdout
            )
            >>= fun () ->
            error (fun m ->
                m "%s[%d] failed, stderr: %s" script_name output.pid
                  output.Process.Output.stderr
            )
            >>= fun () ->
            fail_because "non-zero exit and bad json on stdout"
              ~cause:(string_of_int code)
        | Ok x ->
            fail (backend_backtrace_error x.code x.params x.backtrace)
      )
    )
    | Error (Signal signal) ->
        error (fun m ->
            m "%s[%d] caught a signal and failed" script_name output.pid
        )
        >>= fun () -> fail_because "signalled" ~cause:(Signal.to_string signal)
    | Ok () -> (
      (* Parse the json on stdout. We get back a JSON-RPC
         value from the scripts, not a complete JSON-RPC response *)
      match
        Base.Or_error.try_with (fun () ->
            Jsonrpc.of_string output.Process.Output.stdout
        )
      with
      | Error _ ->
          error (fun m ->
              m "%s[%d] succeeded but printed bad json: %s" script_name
                output.pid output.Process.Output.stdout
          )
          >>= fun () ->
          fail
            (backend_error "SCRIPT_FAILED"
               [script_name; "bad json on stdout"; output.Process.Output.stdout]
            )
      | Ok response ->
          info (fun m ->
              m "%s[%d] succeeded: %s" script_name output.pid
                output.Process.Output.stdout
          )
          >>= fun () ->
          let response = compat_out response in
          let response = R.success response in
          return response
    )
  in
  let script_rpc call :
      (R.response, Storage_interface.Errors.error) Lwt_result.t =
    info (fun m -> m "%s" (Jsonrpc.string_of_call call)) >>= fun () ->
    Script.path ~script_dir ~script_name:call.R.name >>= function
    | Error (`missing path) -> (
        error (fun m -> m "%s is not a file" path) >>= fun () ->
        match missing with
        | None ->
            fail
              (backend_error "SCRIPT_MISSING"
                 [
                   path
                 ; "Check whether the file exists and has correct permissions"
                 ]
              )
        | Some m ->
            warn (fun m ->
                m
                  "Deprecated: script '%s' is missing, treating as no-op. \
                   Update your plugin!"
                  path
            )
            >>= fun () -> return (R.success m)
      )
    | Error (`not_executable (path, exn)) ->
        error (fun m -> m "%s is not executable" path) >>= fun () ->
        fail
          (backend_error "SCRIPT_NOT_EXECUTABLE" [path; Base.Exn.to_string exn])
    | Ok path ->
        invoke_script call path
  in
  (* The Errors we return from this function and the special error format
     returned by the scripts are not included in the error types of the various
     SMAPIv3 interfaces, therefore we have to propagate them as exceptions
     instead of returning an RPC call with an error, because rpclib would fail
     to unmarshal that error.
     Therefore we either return a successful RPC response, or raise
     Fork_exec_error with a suitable SMAPIv2 error if the call failed. *)
  let rpc : R.call -> R.response Lwt.t =
   fun call ->
    script_rpc call >>= fun result ->
    Base.Result.map_error ~f:(fun e -> Fork_exec_error e) result
    |> Base.Result.ok_exn
    |> Lwt.return
  in
  rpc

let string_of_sexp = Sexplib0.Sexp_conv.string_of_sexp

let sexp_of_string = Sexplib0.Sexp_conv.sexp_of_string

let list_of_sexp = Sexplib0.Sexp_conv.list_of_sexp

let sexp_of_list = Sexplib0.Sexp_conv.sexp_of_list

module Attached_SRs = struct
  type state = {sr: string; uids: string list} [@@deriving sexp]

  let sr_table : (string, state) Base.Hashtbl.t ref =
    ref (Base.Hashtbl.create (module Base.String))

  let state_path = ref None

  let add smapiv2 plugin uids =
    let key = Storage_interface.Sr.string_of smapiv2 in
    Base.Hashtbl.set !sr_table ~key ~data:{sr= plugin; uids} ;
    ( match !state_path with
    | None ->
        Lwt.return_unit
    | Some path ->
        let contents =
          Base.Hashtbl.sexp_of_t sexp_of_string sexp_of_state !sr_table
          |> Sexplib.Sexp.to_string
        in
        let dir = Filename.dirname path in
        Sys.mkdir_p dir >>= fun () -> Sys.save path ~contents
    )
    >>= fun () -> return ()

  let find smapiv2 =
    let key = Storage_interface.Sr.string_of smapiv2 in
    match Base.Hashtbl.find !sr_table key with
    | None ->
        let open Storage_interface in
        fail (Errors.Sr_not_attached key)
    | Some {sr; _} ->
        return sr

  let get_uids smapiv2 =
    let key = Storage_interface.Sr.string_of smapiv2 in
    match Base.Hashtbl.find !sr_table key with
    | None ->
        let open Storage_interface in
        fail (Errors.Sr_not_attached key)
    | Some {uids; _} ->
        return uids

  let remove smapiv2 =
    let key = Storage_interface.Sr.string_of smapiv2 in
    Base.Hashtbl.remove !sr_table key ;
    return ()

  let list () =
    let srs =
      Base.Hashtbl.fold !sr_table
        ~f:(fun ~key ~data:_ ac -> Storage_interface.Sr.of_string key :: ac)
        ~init:[]
    in
    return srs

  let reload path =
    state_path := Some path ;
    Sys.file_kind ~follow_symlinks:true path >>= function
    | Regular ->
        Sys.read_file_contents path >>= fun contents ->
        sr_table :=
          contents
          |> Sexplib.Sexp.of_string
          |> Base.Hashtbl.Poly.t_of_sexp string_of_sexp state_of_sexp ;
        Lwt.return_unit
    | _ ->
        Lwt.return_unit
end

module Datapath_plugins = struct
  let table = Base.Hashtbl.create (module Base.String)

  let register ~datapath_root datapath_plugin_name =
    let result =
      let script_dir = datapath_root // datapath_plugin_name in
      return_plugin_rpc (fun () ->
          Plugin_client.query (fork_exec_rpc ~script_dir) "register"
      )
      >>>= fun response ->
      check_plugin_version_compatible response >>= function
      | Ok () ->
          info (fun m -> m "Registered datapath plugin %s" datapath_plugin_name)
          >>= fun () ->
          Base.Hashtbl.set table ~key:datapath_plugin_name
            ~data:(script_dir, response) ;
          return ()
      | Error e ->
          let err_msg =
            Storage_interface.(rpc_of Errors.error) e |> Jsonrpc.to_string
          in
          info (fun m ->
              m "Failed to register datapath plugin %s: %s" datapath_plugin_name
                err_msg
          )
          >>= fun () -> fail e
    in
    (* We just do not register the plugin if we've encountered any error. In
       the future we might want to change that, so we keep the error result
       above. *)
    result >>= fun _ -> Lwt.return_unit

  let unregister datapath_plugin_name =
    Base.Hashtbl.remove table datapath_plugin_name ;
    Lwt.return_unit

  let supports_feature scheme feature =
    match Base.Hashtbl.find table scheme with
    | None ->
        false
    | Some (_script_dir, query_result) ->
        List.mem feature query_result.Xapi_storage.Plugin.features
end

let vdi_of_volume x =
  let find key ~default ~of_string =
    match List.assoc_opt key x.Xapi_storage.Control.keys with
    | None ->
        default
    | Some v ->
        v |> of_string
  in
  let find_string = find ~of_string:Fun.id in
  let open Storage_interface in
  {
    vdi= Vdi.of_string x.Xapi_storage.Control.key
  ; uuid= x.Xapi_storage.Control.uuid
  ; content_id= ""
  ; name_label= x.Xapi_storage.Control.name
  ; name_description= x.Xapi_storage.Control.description
  ; ty= find_string _vdi_type_key ~default:""
  ; metadata_of_pool= ""
  ; is_a_snapshot=
      find _is_a_snapshot_key ~default:false ~of_string:bool_of_string
  ; snapshot_time= find_string _snapshot_time_key ~default:"19700101T00:00:00Z"
  ; snapshot_of= Vdi.of_string (find_string _snapshot_of_key ~default:"")
  ; read_only= not x.Xapi_storage.Control.read_write
  ; cbt_enabled= Option.value x.Xapi_storage.Control.cbt_enabled ~default:false
  ; virtual_size= x.Xapi_storage.Control.virtual_size
  ; physical_utilisation= x.Xapi_storage.Control.physical_utilisation
  ; sm_config= []
  ; sharable= x.Xapi_storage.Control.sharable
  ; persistent= true
  }

let choose_datapath ?(persistent = true) domain response =
  (* We can only use a URI with a valid scheme, since we use the scheme
     to name the datapath plugin. *)
  let possible =
    List.filter_map
      (fun x ->
        let uri = Uri.of_string x in
        match Uri.scheme uri with
        | None ->
            None
        | Some scheme ->
            Some (scheme, x)
      )
      response.Xapi_storage.Control.uri
  in
  (* We can only use URIs whose schemes correspond to registered plugins *)
  let possible =
    List.filter_map
      (fun (scheme, uri) ->
        match Base.Hashtbl.find Datapath_plugins.table scheme with
        | Some (script_dir, _query_result) ->
            Some (script_dir, scheme, uri)
        | None ->
            None
      )
      possible
  in
  (* If we want to be non-persistent, we prefer if the datapath plugin supports it natively *)
  let preference_order =
    if persistent then
      possible
    else
      let supports_nonpersistent, others =
        List.partition
          (fun (_script_dir, scheme, _uri) ->
            Datapath_plugins.supports_feature scheme _nonpersistent
          )
          possible
      in
      supports_nonpersistent @ others
  in
  match preference_order with
  | [] ->
      fail (missing_uri ())
  | (script_dir, scheme, u) :: _us ->
      return (fork_exec_rpc ~script_dir, scheme, u, domain)

let wrap = Rpc_lwt.T.put

let volume_rpc ~volume_script_dir = fork_exec_rpc ~script_dir:volume_script_dir

let version = ref None

(** This module contains the metadata needed for translations to SMAPIv3 to work*)
module type META = sig
  val volume_script_dir : string

  val version : string option ref
  (** This field will be populated once Query.query is called when each plugin
  is registered, after which it will be used in the [Compat] module, which is used 
  in various volume function implementations. 
  It is an alias to the global reference cell declared above *)
end

module QueryImpl =
functor
  (M : META)
  ->
  struct
    let volume_rpc = volume_rpc ~volume_script_dir:M.volume_script_dir

    (* the actual API call for this plugin, sharing same version ref across all calls *)
    let query_impl dbg =
      let th =
        return_plugin_rpc (fun () -> Plugin_client.query (volume_rpc ~dbg) dbg)
        >>>= fun response ->
        let required_api_version =
          response.Xapi_storage.Plugin.required_api_version
        in
        (* the first call to a plugin must be a Query.query that sets the version *)
        version := Some required_api_version ;
        check_plugin_version_compatible response >>>= fun () ->
        (* Convert between the xapi-storage interface and the SMAPI *)
        let features =
          List.map
            (function "VDI_DESTROY" -> "VDI_DELETE" | x -> x)
            response.Xapi_storage.Plugin.features
        in
        (* Look for executable scripts and automatically add capabilities *)
        let rec loop acc = function
          | [] ->
              return acc
          | (script_name, capability) :: rest -> (
              Script.path ~script_dir:M.volume_script_dir ~script_name
              >>= function
              | Error _ ->
                  loop acc rest
              | Ok _ ->
                  loop (capability :: acc) rest
            )
        in
        loop []
          [
            ("SR.attach", "SR_ATTACH")
          ; ("SR.create", "SR_CREATE")
          ; ("SR.destroy", "SR_DELETE")
          ; ("SR.detach", "SR_DETACH")
          ; ("SR.ls", "SR_SCAN")
          ; ("SR.stat", "SR_UPDATE")
          ; ("SR.probe", "SR_PROBE")
          ; ("Volume.create", "VDI_CREATE")
          ; ("Volume.clone", "VDI_CLONE")
          ; ("Volume.snapshot", "VDI_SNAPSHOT")
          ; ("Volume.resize", "VDI_RESIZE")
          ; ("Volume.destroy", "VDI_DELETE")
          ; ("Volume.stat", "VDI_UPDATE")
          ]
        >>>= fun x ->
        let features = features @ x in
        (* Add the features we always have *)
        let features =
          features
          @ [
              "VDI_ATTACH"
            ; "VDI_DETACH"
            ; "VDI_ACTIVATE"
            ; "VDI_DEACTIVATE"
            ; "VDI_INTRODUCE"
            ]
        in
        (* If we have the ability to clone a disk then we can provide
           clone on boot. *)
        let features =
          if List.mem "VDI_CLONE" features then
            "VDI_RESET_ON_BOOT/2" :: features
          else
            features
        in
        let name = response.Xapi_storage.Plugin.name in
        return
          {
            Storage_interface.driver= response.Xapi_storage.Plugin.plugin
          ; name
          ; description= response.Xapi_storage.Plugin.description
          ; vendor= response.Xapi_storage.Plugin.vendor
          ; copyright= response.Xapi_storage.Plugin.copyright
          ; version= response.Xapi_storage.Plugin.version
          ; required_api_version
          ; features
          ; configuration= response.Xapi_storage.Plugin.configuration
          ; required_cluster_stack=
              response.Xapi_storage.Plugin.required_cluster_stack
          }
      in
      wrap th

    let query_diagnostics_impl dbg =
      let th =
        return_plugin_rpc (fun () ->
            Plugin_client.diagnostics (volume_rpc ~dbg) dbg
        )
        >>>= fun response -> return response
      in
      wrap th
  end

module SRImpl =
functor
  (M : META)
  ->
  struct
    let volume_rpc = volume_rpc ~volume_script_dir:M.volume_script_dir

    module Compat = Compat (struct let version = M.version end)

    let sr_attach_impl dbg sr device_config =
      let th =
        Compat.sr_attach device_config >>>= fun compat_in ->
        return_volume_rpc (fun () ->
            Sr_client.attach (volume_rpc ~dbg ~compat_in) dbg device_config
        )
        >>>= fun attach_response ->
        (* Stat the SR to look for datasources *)
        (* SR.stat should take the attached URI *)
        return_volume_rpc (fun () ->
            Sr_client.stat (volume_rpc ~dbg) dbg attach_response
        )
        >>>= fun stat ->
        let rec loop acc = function
          | [] ->
              Lwt.return acc
          | datasource :: datasources -> (
              let uri = Uri.of_string datasource in
              match Uri.scheme uri with
              | Some "xeno+shm" -> (
                  let uid = Uri.path_unencoded uri in
                  let uid =
                    if String.length uid > 1 then
                      String.sub uid 1 (String.length uid - 1)
                    else
                      uid
                  in
                  RRD.Client.Plugin.Local.register RRD.rpc uid Rrd.Five_Seconds
                    Rrd_interface.V2
                  |> Rpc_lwt.T.get
                  >>= function
                  | Ok _ ->
                      loop (uid :: acc) datasources
                  | Error x ->
                      raise Rrd_interface.(Rrdd_error x)
                )
              | _ ->
                  loop acc datasources
            )
        in
        loop [] stat.Xapi_storage.Control.datasources >>= fun uids ->
        (* associate the 'sr' from the plugin with the SR reference passed in *)
        Attached_SRs.add sr attach_response uids >>>= fun () -> return ()
      in
      wrap th

    let sr_detach_impl dbg sr =
      let th =
        Attached_SRs.find sr >>= function
        | Error _ ->
            (* ensure SR.detach is idempotent *)
            return ()
        | Ok sr' ->
            return_volume_rpc (fun () ->
                Sr_client.detach (volume_rpc ~dbg) dbg sr'
            )
            >>>= fun response ->
            Attached_SRs.get_uids sr >>>= fun uids ->
            let rec loop = function
              | [] ->
                  Lwt.return_unit
              | datasource :: datasources -> (
                  let uri = Uri.of_string datasource in
                  match Uri.scheme uri with
                  | Some "xeno+shm" -> (
                      let uid = Uri.path_unencoded uri in
                      let uid =
                        if String.length uid > 1 then
                          String.sub uid 1 (String.length uid - 1)
                        else
                          uid
                      in
                      RRD.Client.Plugin.Local.deregister RRD.rpc uid
                      |> Rpc_lwt.T.get
                      >>= function
                      | Ok _ ->
                          loop datasources
                      | Error x ->
                          raise Rrd_interface.(Rrdd_error x)
                    )
                  | _ ->
                      loop datasources
                )
            in
            loop uids >>= fun () ->
            Attached_SRs.remove sr >>>= fun () -> return response
      in
      wrap th

    let sr_probe_impl dbg _queue device_config _sm_config =
      let th =
        return_volume_rpc (fun () ->
            Sr_client.probe (volume_rpc ~dbg) dbg device_config
        )
        >>>= fun response ->
        let pp_probe_result () probe_result =
          Rpcmarshal.marshal Xapi_storage.Control.typ_of_probe_result
            probe_result
          |> Jsonrpc.to_string
        in
        response
        |> List.map (fun probe_result ->
               let uuid =
                 List.assoc_opt "sr_uuid"
                   probe_result.Xapi_storage.Control.configuration
               in
               let smapiv2_probe ?sr_info () =
                 {
                   Storage_interface.configuration= probe_result.configuration
                 ; complete= probe_result.complete
                 ; sr= sr_info
                 ; extra_info= probe_result.extra_info
                 }
               in
               match
                 ( probe_result.Xapi_storage.Control.sr
                 , probe_result.Xapi_storage.Control.complete
                 , uuid
                 )
               with
               | _, false, Some _uuid ->
                   Deferred.errorf
                     "A configuration with a uuid cannot be incomplete: %a"
                     pp_probe_result probe_result
               | Some sr_stat, true, Some _uuid ->
                   let sr_info =
                     {
                       Storage_interface.name_label=
                         sr_stat.Xapi_storage.Control.name
                     ; sr_uuid= sr_stat.Xapi_storage.Control.uuid
                     ; name_description=
                         sr_stat.Xapi_storage.Control.description
                     ; total_space= sr_stat.Xapi_storage.Control.total_space
                     ; free_space= sr_stat.Xapi_storage.Control.free_space
                     ; clustered= sr_stat.Xapi_storage.Control.clustered
                     ; health=
                         ( match sr_stat.Xapi_storage.Control.health with
                         | Xapi_storage.Control.Healthy _ ->
                             Healthy
                         | Xapi_storage.Control.Recovering _ ->
                             Recovering
                         | Xapi_storage.Control.Unreachable _ ->
                             Unreachable
                         | Xapi_storage.Control.Unavailable _ ->
                             Unavailable
                         )
                     }
                   in
                   return (smapiv2_probe ~sr_info ())
               | Some _sr, _, None ->
                   Deferred.errorf
                     "A configuration is not attachable without a uuid: %a"
                     pp_probe_result probe_result
               | None, false, None ->
                   return (smapiv2_probe ())
               | None, true, _ ->
                   return (smapiv2_probe ())
           )
        |> Deferred.combine_errors
        |> Lwt_result.map_error (fun err ->
               backend_error "SCRIPT_FAILED"
                 ["SR.probe"; Base.Error.to_string_hum err]
           )
        >>>= fun results -> return (Storage_interface.Probe results)
      in
      wrap th

    let sr_create_impl dbg sr_uuid name_label description device_config _size =
      let th =
        let uuid = Storage_interface.Sr.string_of sr_uuid in
        Compat.sr_create device_config
        >>>= fun (device_config, compat_in, compat_out) ->
        return_volume_rpc (fun () ->
            Sr_client.create
              (volume_rpc ~dbg ~compat_in ~compat_out)
              dbg uuid device_config name_label description
        )
        >>>= fun new_device_config -> return new_device_config
      in
      wrap th

    let sr_set_name_label_impl dbg sr new_name_label =
      Attached_SRs.find sr
      >>>= (fun sr ->
             return_volume_rpc (fun () ->
                 Sr_client.set_name (volume_rpc ~dbg) dbg sr new_name_label
             )
           )
      |> wrap

    let sr_set_name_description_impl dbg sr new_name_description =
      Attached_SRs.find sr
      >>>= (fun sr ->
             return_volume_rpc (fun () ->
                 Sr_client.set_description (volume_rpc ~dbg) dbg sr
                   new_name_description
             )
           )
      |> wrap

    let sr_destroy_impl dbg sr =
      Attached_SRs.find sr
      >>>= (fun sr ->
             return_volume_rpc (fun () ->
                 Sr_client.destroy (volume_rpc ~dbg) dbg sr
             )
           )
      |> wrap

    let sr_scan_impl dbg sr =
      Attached_SRs.find sr
      >>>= (fun sr ->
             return_volume_rpc (fun () ->
                 Sr_client.ls
                   (volume_rpc ~dbg ~compat_out:Compat.compat_out_volumes)
                   dbg sr
             )
             >>>= fun response ->
             let response = Array.to_list response in
             (* Filter out volumes which are clone-on-boot transients *)
             let transients =
               List.fold_left
                 (fun set x ->
                   match
                     List.assoc_opt _clone_on_boot_key
                       x.Xapi_storage.Control.keys
                   with
                   | None ->
                       set
                   | Some transient ->
                       Base.Set.add set transient
                 )
                 (Base.Set.empty (module Base.String))
                 response
             in
             let response =
               List.filter
                 (fun x ->
                   not (Base.Set.mem transients x.Xapi_storage.Control.key)
                 )
                 response
             in
             return (List.map vdi_of_volume response)
           )
      |> wrap

    let sr_scan2_impl dbg sr =
      let sr_uuid = Storage_interface.Sr.string_of sr in
      let get_sr_info sr =
        return_volume_rpc (fun () -> Sr_client.stat (volume_rpc ~dbg) dbg sr)
        >>>= fun response ->
        return
          {
            Storage_interface.sr_uuid= response.Xapi_storage.Control.uuid
          ; name_label= response.Xapi_storage.Control.name
          ; name_description= response.Xapi_storage.Control.description
          ; total_space= response.Xapi_storage.Control.total_space
          ; free_space= response.Xapi_storage.Control.free_space
          ; clustered= response.Xapi_storage.Control.clustered
          ; health=
              ( match response.Xapi_storage.Control.health with
              | Xapi_storage.Control.Healthy _ ->
                  Healthy
              | Xapi_storage.Control.Recovering _ ->
                  Recovering
              | Xapi_storage.Control.Unreachable _ ->
                  Unreachable
              | Xapi_storage.Control.Unavailable _ ->
                  Unavailable
              )
          }
      in
      let get_volume_info sr sr_info =
        return_volume_rpc (fun () ->
            Sr_client.ls
              (volume_rpc ~dbg ~compat_out:Compat.compat_out_volumes)
              dbg sr
        )
        >>>= fun response ->
        let response = Array.to_list response in
        (* Filter out volumes which are clone-on-boot transients *)
        let transients =
          List.fold_left
            (fun set x ->
              match
                Base.List.Assoc.find x.Xapi_storage.Control.keys
                  _clone_on_boot_key ~equal:String.equal
              with
              | None ->
                  set
              | Some transient ->
                  Base.Set.add set transient
            )
            (Base.Set.empty (module Base.String))
            response
        in
        let response =
          List.filter
            (fun x -> not (Base.Set.mem transients x.Xapi_storage.Control.key))
            response
        in
        return (List.map vdi_of_volume response, sr_info)
      in
      let rec stat_with_retry ?(times = 3) sr =
        get_sr_info sr >>>= fun sr_info ->
        match sr_info.health with
        | Healthy ->
            let* () =
              debug (fun m -> m "%s sr %s is healthy" __FUNCTION__ sr_uuid)
            in
            get_volume_info sr sr_info
        | Unreachable when times > 0 ->
            let* () =
              debug (fun m ->
                  m "%s: sr %s is unreachable, remaining %d retries"
                    __FUNCTION__ sr_uuid times
              )
            in
            Clock.after ~seconds:1. >>= fun () ->
            stat_with_retry ~times:(times - 1) sr
        | health ->
            let* () =
              debug (fun m ->
                  m "%s: sr unhealthy because it is %s" __FUNCTION__
                    (Storage_interface.show_sr_health health)
              )
            in
            fail Storage_interface.(Errors.Sr_unhealthy (sr_uuid, health))
      in
      Attached_SRs.find sr >>>= stat_with_retry |> wrap

    let sr_stat_impl dbg sr =
      Attached_SRs.find sr
      >>>= (fun sr ->
             return_volume_rpc (fun () ->
                 Sr_client.stat (volume_rpc ~dbg) dbg sr
             )
             >>>= fun response ->
             return
               {
                 Storage_interface.sr_uuid= response.Xapi_storage.Control.uuid
               ; name_label= response.Xapi_storage.Control.name
               ; name_description= response.Xapi_storage.Control.description
               ; total_space= response.Xapi_storage.Control.total_space
               ; free_space= response.Xapi_storage.Control.free_space
               ; clustered= response.Xapi_storage.Control.clustered
               ; health=
                   ( match response.Xapi_storage.Control.health with
                   | Xapi_storage.Control.Healthy _ ->
                       Healthy
                   | Xapi_storage.Control.Recovering _ ->
                       Recovering
                   | Xapi_storage.Control.Unreachable _ ->
                       Unreachable
                   | Xapi_storage.Control.Unavailable _ ->
                       Unavailable
                   )
               }
           )
      |> wrap

    let sr_list _dbg = Attached_SRs.list () >>>= (fun srs -> return srs) |> wrap

    let sr_reset _ _ = return () |> wrap
  end

module VDIImpl =
functor
  (M : META)
  ->
  struct
    let volume_rpc = volume_rpc ~volume_script_dir:M.volume_script_dir

    module Compat = Compat (struct let version = M.version end)

    let set ~dbg ~sr ~vdi ~key ~value =
      (* this is wrong, we loose the VDI type, but old pvsproxy didn't have
       * Volume.set and Volume.unset *)
      (* TODO handle this properly? *)
      let missing =
        Option.bind !version (fun v ->
            if String.equal v pvs_version then Some (R.rpc_of_unit ()) else None
        )
      in
      return_volume_rpc (fun () ->
          Volume_client.set (volume_rpc ~dbg ?missing) dbg sr vdi key value
      )

    let unset ~dbg ~sr ~vdi ~key =
      let missing =
        Option.bind !version (fun v ->
            if String.equal v pvs_version then Some (R.rpc_of_unit ()) else None
        )
      in
      return_volume_rpc (fun () ->
          Volume_client.unset (volume_rpc ~dbg ?missing) dbg sr vdi key
      )

    let stat ~dbg ~sr ~vdi =
      (* TODO add default value to sharable? *)
      return_volume_rpc (fun () ->
          Volume_client.stat
            (volume_rpc ~dbg ~compat_out:Compat.compat_out_volume)
            dbg sr vdi
      )

    let update_keys ~dbg ~sr ~key ~value response =
      match value with
      | None ->
          return response
      | Some value ->
          set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key ~key ~value
          >>>= fun () ->
          return {response with keys= (key, value) :: response.keys}

    let clone ~dbg ~sr ~vdi =
      return_volume_rpc (fun () ->
          Volume_client.clone (volume_rpc ~dbg) dbg sr vdi
      )

    let destroy ~dbg ~sr ~vdi =
      return_volume_rpc (fun () ->
          Volume_client.destroy (volume_rpc ~dbg) dbg sr vdi
      )

    let vdi_attach_common dbg sr vdi domain =
      Attached_SRs.find sr >>>= fun sr ->
      (* Discover the URIs using Volume.stat *)
      stat ~dbg ~sr ~vdi >>>= fun response ->
      (* If we have a clone-on-boot volume then use that instead *)
      ( match
          List.assoc_opt _clone_on_boot_key response.Xapi_storage.Control.keys
        with
      | None ->
          return response
      | Some temporary ->
          stat ~dbg ~sr ~vdi:temporary
      )
      >>>= fun response ->
      choose_datapath domain response >>>= fun (rpc, _datapath, uri, domain) ->
      return_data_rpc (fun () ->
          Datapath_client.attach (rpc ~dbg) dbg uri domain
      )

    let vdi_create_impl dbg sr (vdi_info : Storage_interface.vdi_info) =
      Attached_SRs.find sr
      >>>= (fun sr ->
             return_volume_rpc (fun () ->
                 Volume_client.create
                   (volume_rpc ~dbg ~compat_out:Compat.compat_out_volume)
                   dbg sr vdi_info.Storage_interface.name_label
                   vdi_info.name_description vdi_info.virtual_size
                   vdi_info.sharable
             )
             >>>= update_keys ~dbg ~sr ~key:_vdi_type_key
                    ~value:(match vdi_info.ty with "" -> None | s -> Some s)
             >>>= fun response -> return (vdi_of_volume response)
           )
      |> wrap

    let vdi_destroy_impl dbg sr vdi' =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       Attached_SRs.find sr >>>= fun sr ->
       stat ~dbg ~sr ~vdi >>>= fun response ->
       (* Destroy any clone-on-boot volume that might exist *)
       ( match
           List.assoc_opt _clone_on_boot_key response.Xapi_storage.Control.keys
         with
       | None ->
           return ()
       | Some _temporary ->
           (* Destroy the temporary disk we made earlier *)
           destroy ~dbg ~sr ~vdi
       )
       >>>= fun () -> destroy ~dbg ~sr ~vdi
      )
      |> wrap

    let vdi_snapshot_impl dbg sr vdi_info =
      Attached_SRs.find sr
      >>>= (fun sr ->
             let vdi =
               Storage_interface.Vdi.string_of vdi_info.Storage_interface.vdi
             in
             return_volume_rpc (fun () ->
                 Volume_client.snapshot (volume_rpc ~dbg) dbg sr vdi
             )
             >>>= fun response ->
             let now = Xapi_stdext_date.Date.(to_rfc3339 (now ())) in
             set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key
               ~key:_snapshot_time_key ~value:now
             >>>= fun () ->
             set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key
               ~key:_is_a_snapshot_key ~value:(string_of_bool true)
             >>>= fun () ->
             set ~dbg ~sr ~vdi:response.Xapi_storage.Control.key
               ~key:_snapshot_of_key ~value:vdi
             >>>= fun () ->
             let response =
               {
                 (vdi_of_volume response) with
                 snapshot_time= now
               ; is_a_snapshot= true
               ; snapshot_of= Storage_interface.Vdi.of_string vdi
               }
             in
             return response
           )
      |> wrap

    let vdi_clone_impl dbg sr vdi_info =
      Attached_SRs.find sr
      >>>= (fun sr ->
             clone ~dbg ~sr
               ~vdi:
                 (Storage_interface.Vdi.string_of vdi_info.Storage_interface.vdi)
             >>>= fun response -> return (vdi_of_volume response)
           )
      |> wrap

    let vdi_set_name_label_impl dbg sr vdi' new_name_label =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       Attached_SRs.find sr >>>= fun sr ->
       return_volume_rpc (fun () ->
           Volume_client.set_name (volume_rpc ~dbg) dbg sr vdi new_name_label
       )
      )
      |> wrap

    let vdi_set_name_description_impl dbg sr vdi' new_name_description =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       Attached_SRs.find sr >>>= fun sr ->
       return_volume_rpc (fun () ->
           Volume_client.set_description (volume_rpc ~dbg) dbg sr vdi
             new_name_description
       )
      )
      |> wrap

    let vdi_resize_impl dbg sr vdi' new_size =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       Attached_SRs.find sr >>>= fun sr ->
       return_volume_rpc (fun () ->
           Volume_client.resize (volume_rpc ~dbg) dbg sr vdi new_size
       )
       >>>= fun () ->
       (* Now call Volume.stat to discover the size *)
       stat ~dbg ~sr ~vdi >>>= fun response ->
       return response.Xapi_storage.Control.virtual_size
      )
      |> wrap

    let vdi_stat_impl dbg sr vdi' =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       Attached_SRs.find sr >>>= fun sr ->
       stat ~dbg ~sr ~vdi >>>= fun response -> return (vdi_of_volume response)
      )
      |> wrap

    let vdi_introduce_impl dbg sr _uuid _sm_config location =
      Attached_SRs.find sr
      >>>= (fun sr ->
             let vdi = location in
             stat ~dbg ~sr ~vdi >>>= fun response ->
             return (vdi_of_volume response)
           )
      |> wrap

    let vdi_attach3_impl dbg dp sr vdi' vm' _readwrite =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       let domain = domain_of ~dp ~vm' in
       vdi_attach_common dbg sr vdi domain >>>= fun response ->
       let convert_implementation = function
         | Xapi_storage.Data.XenDisk {params; extra; backend_type} ->
             Storage_interface.XenDisk {params; extra; backend_type}
         | BlockDevice {path} ->
             BlockDevice {path}
         | File {path} ->
             File {path}
         | Nbd {uri} ->
             Nbd {uri}
       in
       return
         {
           Storage_interface.implementations=
             List.map convert_implementation
               response.Xapi_storage.Data.implementations
         }
      )
      |> wrap

    let vdi_activate_common dbg dp sr vdi' vm' readonly =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       let domain = domain_of ~dp ~vm' in
       Attached_SRs.find sr >>>= fun sr ->
       (* Discover the URIs using Volume.stat *)
       stat ~dbg ~sr ~vdi >>>= fun response ->
       (* If we have a clone-on-boot volume then use that instead *)
       ( match
           List.assoc_opt _clone_on_boot_key response.Xapi_storage.Control.keys
         with
       | None ->
           return response
       | Some temporary ->
           stat ~dbg ~sr ~vdi:temporary
       )
       >>>= fun response ->
       choose_datapath domain response >>>= fun (rpc, _datapath, uri, domain) ->
       return_data_rpc (fun () ->
           let rpc = rpc ~dbg in
           if readonly then
             Datapath_client.activate_readonly rpc dbg uri domain
           else
             Datapath_client.activate rpc dbg uri domain
       )
      )
      |> wrap

    let vdi_activate3_impl dbg dp sr vdi' vm' =
      vdi_activate_common dbg dp sr vdi' vm' false

    let vdi_activate_readonly_impl dbg dp sr vdi' vm' =
      vdi_activate_common dbg dp sr vdi' vm' true

    let vdi_deactivate_impl dbg dp sr vdi' vm' =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       let domain = domain_of ~dp ~vm' in
       Attached_SRs.find sr >>>= fun sr ->
       (* Discover the URIs using Volume.stat *)
       stat ~dbg ~sr ~vdi >>>= fun response ->
       ( match
           List.assoc_opt _clone_on_boot_key response.Xapi_storage.Control.keys
         with
       | None ->
           return response
       | Some temporary ->
           stat ~dbg ~sr ~vdi:temporary
       )
       >>>= fun response ->
       choose_datapath domain response >>>= fun (rpc, _datapath, uri, domain) ->
       return_data_rpc (fun () ->
           Datapath_client.deactivate (rpc ~dbg) dbg uri domain
       )
      )
      |> wrap

    let vdi_detach_impl dbg dp sr vdi' vm' =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       let domain = domain_of ~dp ~vm' in
       Attached_SRs.find sr >>>= fun sr ->
       (* Discover the URIs using Volume.stat *)
       stat ~dbg ~sr ~vdi >>>= fun response ->
       ( match
           List.assoc_opt _clone_on_boot_key response.Xapi_storage.Control.keys
         with
       | None ->
           return response
       | Some temporary ->
           stat ~dbg ~sr ~vdi:temporary
       )
       >>>= fun response ->
       choose_datapath domain response >>>= fun (rpc, _datapath, uri, domain) ->
       return_data_rpc (fun () ->
           Datapath_client.detach (rpc ~dbg) dbg uri domain
       )
      )
      |> wrap

    let vdi_epoch_begin_impl dbg sr vdi' vm' persistent =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       let domain = Storage_interface.Vm.string_of vm' in
       Attached_SRs.find sr >>>= fun sr ->
       (* Discover the URIs using Volume.stat *)
       stat ~dbg ~sr ~vdi >>>= fun response ->
       choose_datapath ~persistent domain response
       >>>= fun (rpc, datapath, uri, _domain) ->
       (* If non-persistent and the datapath plugin supports NONPERSISTENT
          then we delegate this to the datapath plugin. Otherwise we will
          make a temporary clone now and attach/detach etc this file. *)
       if Datapath_plugins.supports_feature datapath _nonpersistent then
         (* We delegate handling non-persistent disks to the datapath plugin. *)
         return_data_rpc (fun () ->
             Datapath_client.open_ (rpc ~dbg) dbg uri persistent
         )
       else if not persistent then
         (* We create a non-persistent disk here with Volume.clone, and store
            the name of the cloned disk in the metadata of the original. *)
         ( match
             List.assoc_opt _clone_on_boot_key
               response.Xapi_storage.Control.keys
           with
         | None ->
             return ()
         | Some temporary ->
             (* Destroy the temporary disk we made earlier *)
             destroy ~dbg ~sr ~vdi:temporary
         )
         >>>= fun () ->
         clone ~dbg ~sr ~vdi >>>= fun vdi' ->
         set ~dbg ~sr ~vdi ~key:_clone_on_boot_key
           ~value:vdi'.Xapi_storage.Control.key
       else
         return ()
      )
      |> wrap

    let vdi_epoch_end_impl dbg sr vdi' vm' =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       let domain = Storage_interface.Vm.string_of vm' in
       Attached_SRs.find sr >>>= fun sr ->
       (* Discover the URIs using Volume.stat *)
       stat ~dbg ~sr ~vdi >>>= fun response ->
       choose_datapath domain response >>>= fun (rpc, datapath, uri, _domain) ->
       if Datapath_plugins.supports_feature datapath _nonpersistent then
         return_data_rpc (fun () -> Datapath_client.close (rpc ~dbg) dbg uri)
       else
         match
           List.assoc_opt _clone_on_boot_key response.Xapi_storage.Control.keys
         with
         | None ->
             return ()
         | Some temporary ->
             (* Destroy the temporary disk we made earlier *)
             destroy ~dbg ~sr ~vdi:temporary >>>= fun () ->
             unset ~dbg ~sr ~vdi ~key:_clone_on_boot_key >>>= fun () ->
             return ()
      )
      |> wrap

    let vdi_set_persistent_impl _dbg _sr _vdi _persistent = return () |> wrap

    let ( let* ) = Lwt_result.bind

    let vdi_enable_cbt_impl dbg sr vdi =
      wrap
      @@
      let* sr = Attached_SRs.find sr in
      let vdi = Storage_interface.Vdi.string_of vdi in
      return_volume_rpc (fun () ->
          Volume_client.enable_cbt (volume_rpc ~dbg) dbg sr vdi
      )

    let vdi_disable_cbt_impl dbg sr vdi =
      wrap
      @@
      let* sr = Attached_SRs.find sr in
      let vdi = Storage_interface.Vdi.string_of vdi in
      return_volume_rpc (fun () ->
          Volume_client.disable_cbt (volume_rpc ~dbg) dbg sr vdi
      )

    let vdi_list_changed_blocks_impl dbg sr vdi vdi' =
      wrap
      @@
      let* sr = Attached_SRs.find sr in
      let vdi, vdi' = Storage_interface.Vdi.(string_of vdi, string_of vdi') in
      let ( let* ) = Lwt.bind in
      let* result =
        return_volume_rpc (fun () ->
            (* Negative lengths indicate that we want the full length. *)
            Volume_client.list_changed_blocks (volume_rpc ~dbg) dbg sr vdi vdi'
              0L (-1)
        )
      in
      let proj_bitmap r = r.Xapi_storage.Control.bitmap in
      Lwt.return (Result.map proj_bitmap result)

    let vdi_data_destroy_impl dbg sr vdi =
      wrap
      @@
      let* sr = Attached_SRs.find sr in
      let vdi = Storage_interface.Vdi.string_of vdi in
      let* () =
        return_volume_rpc (fun () ->
            Volume_client.data_destroy (volume_rpc ~dbg) dbg sr vdi
        )
      in
      set ~dbg ~sr ~vdi ~key:_vdi_type_key ~value:"cbt_metadata"
  end

module DPImpl =
functor
  (M : META)
  ->
  struct
    module VDI = VDIImpl (M)

    let dp_destroy2 dbg dp sr vdi' vm' _allow_leak =
      (let vdi = Storage_interface.Vdi.string_of vdi' in
       let domain = domain_of ~dp ~vm' in
       Attached_SRs.find sr >>>= fun sr ->
       (* Discover the URIs using Volume.stat *)
       VDI.stat ~dbg ~sr ~vdi >>>= fun response ->
       ( match
           List.assoc_opt _clone_on_boot_key response.Xapi_storage.Control.keys
         with
       | None ->
           return response
       | Some temporary ->
           VDI.stat ~dbg ~sr ~vdi:temporary
       )
       >>>= fun response ->
       choose_datapath domain response >>>= fun (rpc, _datapath, uri, domain) ->
       return_data_rpc (fun () ->
           Datapath_client.deactivate (rpc ~dbg) dbg uri domain
       )
       >>>= fun () ->
       return_data_rpc (fun () ->
           Datapath_client.detach (rpc ~dbg) dbg uri domain
       )
      )
      |> wrap
  end

(* Bind the implementations *)
let bind ~volume_script_dir =
  (* Each plugin has its own version, see the call to listen
     where `process` is partially applied. *)
  let module S = Storage_interface.StorageAPI (Rpc_lwt.GenServer ()) in
  let module RuntimeMeta = struct
    let volume_script_dir = volume_script_dir

    let version = version
  end in
  let module Query = QueryImpl (RuntimeMeta) in
  S.Query.query Query.query_impl ;
  S.Query.diagnostics Query.query_diagnostics_impl ;

  let module SR = SRImpl (RuntimeMeta) in
  S.SR.attach SR.sr_attach_impl ;
  S.SR.detach SR.sr_detach_impl ;
  S.SR.probe SR.sr_probe_impl ;
  S.SR.create SR.sr_create_impl ;
  S.SR.set_name_label SR.sr_set_name_label_impl ;
  S.SR.set_name_description SR.sr_set_name_description_impl ;
  S.SR.destroy SR.sr_destroy_impl ;
  S.SR.scan SR.sr_scan_impl ;
  S.SR.scan2 SR.sr_scan2_impl ;
  S.SR.stat SR.sr_stat_impl ;
  S.SR.list SR.sr_list ;
  (* SR.reset is a no op in SMAPIv3 *)
  S.SR.reset SR.sr_reset ;

  let module VDI = VDIImpl (RuntimeMeta) in
  S.VDI.create VDI.vdi_create_impl ;
  S.VDI.destroy VDI.vdi_destroy_impl ;
  S.VDI.snapshot VDI.vdi_snapshot_impl ;
  S.VDI.clone VDI.vdi_clone_impl ;
  S.VDI.set_name_label VDI.vdi_set_name_label_impl ;
  S.VDI.set_name_description VDI.vdi_set_name_description_impl ;
  S.VDI.resize VDI.vdi_resize_impl ;
  S.VDI.stat VDI.vdi_stat_impl ;
  S.VDI.introduce VDI.vdi_introduce_impl ;
  S.VDI.attach3 VDI.vdi_attach3_impl ;
  S.VDI.activate3 VDI.vdi_activate3_impl ;
  S.VDI.activate_readonly VDI.vdi_activate_readonly_impl ;
  S.VDI.deactivate VDI.vdi_deactivate_impl ;
  S.VDI.detach VDI.vdi_detach_impl ;
  S.VDI.epoch_begin VDI.vdi_epoch_begin_impl ;
  S.VDI.epoch_end VDI.vdi_epoch_end_impl ;
  S.VDI.set_persistent VDI.vdi_set_persistent_impl ;
  S.VDI.enable_cbt VDI.vdi_enable_cbt_impl ;
  S.VDI.disable_cbt VDI.vdi_disable_cbt_impl ;
  S.VDI.list_changed_blocks VDI.vdi_list_changed_blocks_impl ;
  S.VDI.data_destroy VDI.vdi_data_destroy_impl ;

  let module DP = DPImpl (RuntimeMeta) in
  S.DP.destroy2 DP.dp_destroy2 ;

  let u name _ = failwith ("Unimplemented: " ^ name) in
  S.get_by_name (u "get_by_name") ;
  S.VDI.compose (u "VDI.compose") ;
  S.VDI.get_by_name (u "VDI.get_by_name") ;
  S.DATA.MIRROR.receive_start (u "DATA.MIRROR.receive_start") ;
  S.UPDATES.get (u "UPDATES.get") ;
  S.SR.update_snapshot_info_dest (u "SR.update_snapshot_info_dest") ;
  S.DATA.MIRROR.list (u "DATA.MIRROR.list") ;
  S.TASK.stat (u "TASK.stat") ;
  S.VDI.remove_from_sm_config (u "VDI.remove_from_sm_config") ;
  S.DP.diagnostics (u "DP.diagnostics") ;
  S.TASK.destroy (u "TASK.destroy") ;
  S.DP.destroy (u "DP.destroy") ;
  S.VDI.add_to_sm_config (u "VDI.add_to_sm_config") ;
  S.VDI.similar_content (u "VDI.similar_content") ;
  S.DATA.copy (u "DATA.copy") ;
  S.DP.stat_vdi (u "DP.stat_vdi") ;
  S.DATA.MIRROR.receive_finalize (u "DATA.MIRROR.receive_finalize") ;
  S.DP.create (u "DP.create") ;
  S.VDI.set_content_id (u "VDI.set_content_id") ;
  S.DP.attach_info (u "DP.attach_info") ;
  S.TASK.cancel (u "TASK.cancel") ;
  S.VDI.attach (u "VDI.attach") ;
  S.VDI.attach2 (u "VDI.attach2") ;
  S.VDI.activate (u "VDI.activate") ;
  S.DATA.MIRROR.stat (u "DATA.MIRROR.stat") ;
  S.TASK.list (u "TASK.list") ;
  S.VDI.get_url (u "VDI.get_url") ;
  S.DATA.MIRROR.start (u "DATA.MIRROR.start") ;
  S.Policy.get_backend_vm (u "Policy.get_backend_vm") ;
  S.DATA.MIRROR.receive_cancel (u "DATA.MIRROR.receive_cancel") ;
  S.SR.update_snapshot_info_src (u "SR.update_snapshot_info_src") ;
  S.DATA.MIRROR.stop (u "DATA.MIRROR.stop") ;
  Rpc_lwt.server S.implementation

let process_smapiv2_requests server txt =
  let request = Jsonrpc.call_of_string txt in
  let to_err e =
    Storage_interface.(rpc_of Errors.error Errors.(Internal_error e))
  in
  Lwt.try_bind
    (fun () -> server request)
    (fun response -> Lwt.return (Jsonrpc.string_of_response response))
    (fun exn ->
      Printexc.to_string exn |> to_err |> Jsonrpc.to_string |> Lwt.return
    )

(** Active servers, one per sub-directory of the volume_root_dir *)
let servers = Base.Hashtbl.create ~size:4 (module Base.String)

(* XXX: need a better error-handling strategy *)
let get_ok x =
  Message_switch_unix.Protocol_unix.Server.error_to_msg x
  |> Result.fold ~ok:Fun.id ~error:(function `Msg err -> failwith err)

let rec diff a b =
  match a with
  | [] ->
      []
  | a :: aa ->
      if List.mem a b then diff aa b else a :: diff aa b

(* default false due to bugs in SMAPIv3 plugins,
   once they are fixed this should be set to true *)
let concurrent = ref true

type reload = All | Files of string list | Nothing

let actions_from events =
  List.fold_left
    (fun acc event ->
      match (event, acc) with
      | DirWatcher.Modified path, Nothing ->
          Files [path]
      | Modified path, Files files ->
          Files (path :: files)
      | Changed, _ | _, All ->
          All
    )
    Nothing events

let reload_all root ~create ~destroy =
  let* needed = Sys.readdir root in
  let got_already = Base.Hashtbl.keys servers in
  let* () = Lwt.join (List.map create (diff needed got_already)) in
  Lwt.join (List.map destroy (diff got_already needed))

let reload_file ~create ~destroy path =
  let name = Filename.basename path in
  let* () = destroy name in
  create name

let reload root ~create ~destroy = function
  | All ->
      reload_all root ~create ~destroy
  | Files files ->
      Lwt_list.iter_p (reload_file ~create ~destroy) files
  | Nothing ->
      Lwt.return_unit

let rec watch_loop pipe root ~create ~destroy =
  let* () = Lwt_unix.sleep 0.5 in
  let* () =
    let* events = DirWatcher.read pipe in
    reload root ~create ~destroy (actions_from events)
  in
  watch_loop pipe root ~create ~destroy

let watch_plugins ~pipe ~root ~create ~destroy =
  reload_all root ~create ~destroy >>= fun () ->
  watch_loop pipe root ~create ~destroy

let watch_volume_plugins ~volume_root ~switch_path ~pipe () =
  let create volume_plugin_name =
    if Base.Hashtbl.mem servers volume_plugin_name then
      Lwt.return_unit
    else
      info (fun m -> m "Adding %s" volume_plugin_name) >>= fun () ->
      let volume_script_dir = volume_root // volume_plugin_name in
      Message_switch_lwt.Protocol_lwt.Server.(
        if !concurrent then listen_p else listen
      )
        ~process:(process_smapiv2_requests (bind ~volume_script_dir))
        ~switch:switch_path
        ~queue:(Filename.basename volume_plugin_name)
        ()
      >>= fun result ->
      let server = get_ok result in
      Base.Hashtbl.add_exn servers ~key:volume_plugin_name ~data:server ;
      Lwt.return_unit
  in
  let destroy volume_plugin_name =
    info (fun m -> m "Removing %s" volume_plugin_name) >>= fun () ->
    match Base.Hashtbl.find servers volume_plugin_name with
    | Some t ->
        Message_switch_lwt.Protocol_lwt.Server.shutdown ~t () >>= fun () ->
        Base.Hashtbl.remove servers volume_plugin_name ;
        Lwt.return_unit
    | None ->
        Lwt.return_unit
  in
  watch_plugins ~pipe ~root:volume_root ~create ~destroy

let watch_datapath_plugins ~datapath_root ~pipe () =
  let create = Datapath_plugins.register ~datapath_root in
  let destroy = Datapath_plugins.unregister in
  watch_plugins ~pipe ~root:datapath_root ~create ~destroy

let self_test_plugin ~root_dir plugin =
  let volume_script_dir = Filename.(concat (concat root_dir "volume") plugin) in
  let process = process_smapiv2_requests (bind ~volume_script_dir) in
  let rpc call =
    call |> Jsonrpc.string_of_call |> process >>= fun r ->
    debug (fun m -> m "RPC: %s" r) >>= fun () ->
    Lwt.return (Jsonrpc.response_of_string r)
  in
  let module Test = Storage_interface.StorageAPI (Rpc_lwt.GenClient ()) in
  let dbg = "debug" in
  Deferred.try_with (fun () ->
      let open Rpc_lwt.ErrM in
      Test.Query.query rpc dbg
      >>= (fun query_result ->
            Test.Query.diagnostics rpc dbg >>= fun _msg ->
            let sr = Storage_interface.Sr.of_string "dummySR" in
            let name_label = "dummy name" in
            let name_description = "dummy description" in
            let device_config = [("uri", "file:///dev/null")] in
            let physical_size = 0L in
            Test.SR.create rpc dbg sr name_label name_description device_config
              physical_size
            >>= fun device_config ->
            Test.SR.detach rpc dbg sr >>= fun () ->
            Test.SR.attach rpc dbg sr device_config >>= fun () ->
            let vdi_info =
              {
                Storage_interface.vdi=
                  Storage_interface.Vdi.of_string "vdi-uuid-1"
              ; uuid= None
              ; content_id= ""
              ; name_label= "vdi name"
              ; name_description= "vdi description"
              ; ty= "redolog"
              ; metadata_of_pool= ""
              ; is_a_snapshot= false
              ; snapshot_time= ""
              ; snapshot_of= Storage_interface.Vdi.of_string ""
              ; read_only= false
              ; cbt_enabled= false
              ; virtual_size= 0L
              ; physical_utilisation= 0L
              ; persistent= false
              ; sm_config= []
              ; sharable= false
              }
            in
            Test.VDI.create rpc dbg sr vdi_info >>= fun vdi_info ->
            Test.VDI.stat rpc dbg sr vdi_info.vdi >>= fun _vdi_info ->
            Test.VDI.destroy rpc dbg sr vdi_info.vdi >>= fun () ->
            Test.SR.stat rpc dbg sr >>= fun _sr_info ->
            Test.SR.scan rpc dbg sr >>= fun _sr_list ->
            if List.mem "SR_PROBE" query_result.features then
              Test.SR.probe rpc dbg plugin device_config [] >>= fun _result ->
              return ()
            else
              return ()
          )
      |> Rpc_lwt.T.get
  )
  >>= function
  | Ok x ->
      Lwt.return x
  | Error e ->
      failwith (Printf.sprintf "self test failed with %s" (Printexc.to_string e))

let self_test ~root_dir =
  self_test_plugin ~root_dir "org.xen.xapi.storage.dummyv5" >>= function
  | Ok () ->
      info (fun m -> m "test thread shutdown cleanly") >>= fun () -> exit 0
  | Error x ->
      error (fun m ->
          m "test thread failed with %s"
            (Storage_interface.(rpc_of Errors.error) x |> Jsonrpc.to_string)
      )
      >>= fun () -> exit 2

let main ~root_dir ~state_path ~switch_path =
  Attached_SRs.reload state_path >>= fun () ->
  let datapath_root = root_dir // "datapath" in
  DirWatcher.create datapath_root >>= fun datapath ->
  let volume_root = root_dir // "volume" in
  DirWatcher.create volume_root >>= fun volume ->
  let rec retry_loop ((name, promise) as thread) () =
    Deferred.try_with promise >>= function
    | Ok () ->
        Lwt.return_unit
    | Error x ->
        error (fun m -> m "%s thread failed with %s" name (Base.Exn.to_string x))
        >>= fun () -> Clock.after ~seconds:5. >>= retry_loop thread
  in
  [
    ( "volume plugins"
    , watch_volume_plugins ~volume_root ~switch_path ~pipe:volume
    )
  ; ("datapath plugins", watch_datapath_plugins ~datapath_root ~pipe:datapath)
  ]
  |> List.map (fun thread -> retry_loop thread ())
  |> Lwt.join

open Xcp_service

let description =
  String.concat " "
    [
      "Allow xapi storage adapters to be written as individual scripts."
    ; "To add a storage adapter, create a sub-directory in the --root directory"
    ; "with the name of the adapter (e.g. org.xen.xcp.storage.mylvm) and place"
    ; "the scripts inside."
    ]

type backend_error = string * string list [@@deriving sexp]

(** registers pretty printers for `Backend_error*` exceptions.
 *  Otherwise we only log Backend_error_with_backtrace(_), with the pretty
 *  printer we can log the actual arguments of the exception.
 * *)
let register_exn_pretty_printers () =
  Sexplib.Conv.Exn_converter.add ~finalise:false
    [%extension_constructor Storage_interface.Storage_error] (function
    | Storage_interface.Storage_error (Backend_error e) ->
        sexp_of_backend_error e
    | Storage_interface.Storage_error (Backend_error_with_backtrace e) ->
        sexp_of_backend_error e
    | _ ->
        assert false
    )

let () =
  register_exn_pretty_printers () ;
  let root_dir = ref "/var/lib/xapi/storage-scripts" in
  let state_path = ref "/var/run/nonpersistent/xapi-storage-script/state.db" in
  let resources =
    [
      {
        Xcp_service.name= "root"
      ; description=
          "directory whose sub-directories contain sets of per-operation \
           scripts, one sub-directory per queue name"
      ; essential= true
      ; path= root_dir
      ; perms= [Unix.X_OK]
      }
    ; {
        Xcp_service.name= "state"
      ; description=
          "file containing attached SR information, should be deleted on host \
           boot"
      ; essential= false
      ; path= state_path
      ; perms= []
      }
    ]
  in
  let self_test_only = ref false in
  let options =
    [
      ( "self-test-only"
      , Arg.Set self_test_only
      , (fun () -> string_of_bool !self_test_only)
      , "Do only a self-test and exit"
      )
    ; ( "concurrent"
      , Arg.Set concurrent
      , (fun () -> string_of_bool !concurrent)
      , "Issue SMAPIv3 calls concurrently"
      )
    ]
  in
  configure2 ~name:"xapi-script-storage" ~version:Xapi_version.version
    ~doc:description ~resources ~options () ;

  Logs.set_reporter (lwt_reporter ()) ;
  Logs.set_level ~all:true (Some Logs.Info) ;
  let main =
    let* observer_enabled = observer_is_component_enabled () in
    config.use_observer <- observer_enabled ;
    if !self_test_only then
      self_test ~root_dir:!root_dir
    else
      main ~root_dir:!root_dir ~state_path:!state_path
        ~switch_path:!Xcp_client.switch_path
  in
  Lwt_main.run main
