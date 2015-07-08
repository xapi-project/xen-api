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

open Core.Std
open Async.Std

open Types

let use_syslog = ref false

let info fmt =
  Printf.ksprintf (fun s ->
    if !use_syslog then begin
      (* FIXME: this is synchronous and will block other I/O *)
      Core.Syslog.syslog ~level:Core.Syslog.Level.INFO s;
      return ()
    end else begin
      let w = Lazy.force Writer.stderr in
      Writer.write w s;
      Writer.newline w;
      Writer.flushed w;
    end
  ) fmt

let backend_error name args =
  let open Storage_interface in
  let exnty = Exception.Backend_error (name, args) in
  Exception.rpc_of_exnty exnty

let backend_backtrace_error name args error =
  let error = rpc_of_error error |> Jsonrpc.to_string in
  let open Storage_interface in
  let exnty = Exception.Backend_error_with_backtrace(name, error :: args) in
  Exception.rpc_of_exnty exnty

let missing_uri () =
  backend_error "MISSING_URI" [ "Please include a URI in the device-config" ]

let (>>>=) = Deferred.Result.(>>=)

let fork_exec_rpc root_dir script_name args response_of_rpc =
  ( Sys.is_file ~follow_symlinks:true script_name
    >>= function
    | `No | `Unknown ->
      return (Error(backend_error "SCRIPT_MISSING" [ script_name; "Check whether the file exists and has correct permissions" ]))
    | `Yes -> return (Ok ())
  ) >>>= fun () ->
  ( Unix.access script_name [ `Exec ]
    >>= function
    | Error exn ->
      return (Error (backend_error "SCRIPT_NOT_EXECUTABLE" [ script_name; Exn.to_string exn ]))
    | Ok () -> return (Ok ())
  ) >>>= fun () ->
  Process.create ~prog:script_name ~args:["--json"] ~working_dir:root_dir ()
  >>= function
  | Error e ->
    return (Error(backend_error "SCRIPT_FAILED" [ script_name; Error.to_string_hum e ]))
  | Ok p ->
    (* Send the request as json on stdin *)
    let w = Process.stdin p in
    Writer.write w (Jsonrpc.to_string args);
    Writer.close w
    >>= fun () ->
    Process.wait p
    >>= fun output ->
    begin match output.Process.Output.exit_status with
    | Error (`Exit_non_zero code) ->
      (* Expect an exception and backtrace on stderr *)
      begin match Or_error.try_with (fun () -> Jsonrpc.of_string output.Process.Output.stderr) with
      | Error _ ->
        return (Error (backend_error "SCRIPT_FAILED" [ script_name; "non-zero exit and bad json on stderr"; string_of_int code; output.Process.Output.stdout; output.Process.Output.stderr ]))
      | Ok response ->
        begin match Or_error.try_with (fun () -> error_of_rpc response) with
        | Error _ -> return (Error (backend_error "SCRIPT_FAILED" [ script_name; "non-zero exit and bad json on stderr"; string_of_int code; output.Process.Output.stdout; output.Process.Output.stderr ]))
        | Ok x -> return (Error(backend_backtrace_error "SCRIPT_FAILED" [ script_name; "non-zero exit"; string_of_int code; output.Process.Output.stdout ] x))
        end
      end
    | Error (`Signal signal) ->
      return (Error (backend_error "SCRIPT_FAILED" [ script_name; "signalled"; Signal.to_string signal; output.Process.Output.stdout; output.Process.Output.stderr ]))
    | Ok () ->

      (* Parse the json on stdout *)
      begin match Or_error.try_with (fun () -> Jsonrpc.of_string output.Process.Output.stdout) with
      | Error _ ->
        return (Error (backend_error "SCRIPT_FAILED" [ script_name; "bad json on stdout"; output.Process.Output.stdout ]))
      | Ok response ->
        begin match Or_error.try_with (fun () -> response_of_rpc response) with
        | Error _ ->
          return (Error (backend_error "SCRIPT_FAILED" [ script_name; "json did not match schema"; output.Process.Output.stdout ]))
        | Ok x -> return (Ok x)
        end
      end
    end

module Attached_SRs = struct
  let sr_table : string String.Table.t ref = ref (String.Table.create ())
  let state_path = ref None

  let add smapiv2 plugin =
    Hashtbl.replace !sr_table smapiv2 plugin;
    ( match !state_path with
      | None ->
        return ()
      | Some path ->
        let contents = String.Table.sexp_of_t (fun x -> Sexplib.Sexp.Atom x) !sr_table |> Sexplib.Sexp.to_string in
        Writer.save path ~contents
    ) >>= fun () ->
    return (Ok ())

  let find smapiv2 =
    match Hashtbl.find !sr_table smapiv2 with
    | None ->
      let open Storage_interface in
      let exnty = Exception.Sr_not_attached smapiv2 in
      return (Error (Exception.rpc_of_exnty exnty))
    | Some sr -> return (Ok sr)

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
      sr_table := contents |> Sexplib.Sexp.of_string |> String.Table.t_of_sexp (function Sexplib.Sexp.Atom x -> x | _ -> assert false);
      return ()
end

let vdi_of_volume x =
  let open Storage_interface in {
  vdi = x.Storage.Volume.Types.key;
  content_id = "";
  name_label = x.Storage.Volume.Types.name;
  name_description = x.Storage.Volume.Types.description;
  ty = "";
  metadata_of_pool = "";
  is_a_snapshot = false;
  snapshot_time = "19700101T00:00:00Z";
  snapshot_of = "";
  read_only = not x.Storage.Volume.Types.read_write;
  virtual_size = x.Storage.Volume.Types.virtual_size;
  physical_utilisation = x.Storage.Volume.Types.physical_utilisation;
  sm_config = [];
  persistent = true;
}

let choose_datapath = function
  | [] -> return (Error (missing_uri ()))
  | uri :: _ ->
    let uri' = Uri.of_string uri in
    let domain = "0" in
    begin match Uri.scheme uri' with
    | None -> return (Error (missing_uri ()))
    | Some scheme -> return (Ok (scheme, uri, domain))
    end

let script root_dir name kind script = match kind with
| `Volume -> Filename.(concat (concat root_dir name) script)
| `Datapath datapath -> Filename.(concat (concat (concat (dirname root_dir) "datapath") datapath) script)

let stat root_dir name dbg sr vdi =
  let args = Storage.Volume.Types.Volume.Stat.In.make dbg sr vdi in
  let args = Storage.Volume.Types.Volume.Stat.In.rpc_of_t args in
  let open Deferred.Result.Monad_infix in
  fork_exec_rpc root_dir (script root_dir name `Volume "Volume.stat") args Storage.Volume.Types.Volume.Stat.Out.t_of_rpc
  >>= fun response ->
  choose_datapath response.Storage.Volume.Types.uri

(* Process a message *)
let process root_dir name x =
  let open Storage_interface in
  let call = Jsonrpc.call_of_string x in
  (match call with
  | { R.name = "Query.query"; R.params = [ args ] } ->
    let args = Args.Query.Query.request_of_rpc args in
    (* convert to new storage interface *)
    let args = Storage.Plugin.Types.Plugin.Query.In.make args.Args.Query.Query.dbg in
    let args = Storage.Plugin.Types.Plugin.Query.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    fork_exec_rpc root_dir (script root_dir name `Volume "Plugin.Query") args Storage.Plugin.Types.Plugin.Query.Out.t_of_rpc
    >>= fun response ->
    (* Convert between the xapi-storage interface and the SMAPI *)
    let features = List.map ~f:(function
      | "VDI_DESTROY" -> "VDI_DELETE"
      | x -> x) response.Storage.Plugin.Types.features in
    (* Look for executable scripts and automatically add capabilities *)
    let rec loop acc = function
      | [] -> return (Ok acc)
      | (s, capability) :: rest ->
        let open Deferred.Monad_infix in
        let script_name = script root_dir name `Volume s in
        ( Sys.is_file ~follow_symlinks:true script_name
          >>= function
          | `No | `Unknown ->
            return false
          | `Yes ->
            ( Unix.access script_name [ `Exec ]
              >>= function
              | Error exn ->
                return false
              | Ok () ->
                return true
            )
          ) >>= function
          | false -> loop acc rest
          | true -> loop (capability :: acc) rest in
    loop [] [
      "SR.attach",       "SR_ATTACH";
      "SR.create",       "SR_CREATE";
      "SR.destroy",      "SR_DELETE";
      "SR.detach",       "SR_DETACH";
      "SR.ls",           "SR_SCAN";
      "SR.stat",         "SR_UPDATE";
      "Volume.create",   "VDI_CREATE";
      "Volume.clone",    "VDI_CLONE";
      "Volume.snapshot", "VDI_SNAPSHOT";
      "Volume.resize",   "VDI_RESIZE";
      "Volume.destroy",  "VDI_DELETE";
      "Volume.stat",     "VDI_UPDATE";
    ]
    >>= fun x ->
    let features = features @ x in
    (* Add the features we always have *)
    let features = features @ [
      "VDI_ATTACH"; "VDI_DETACH"; "VDI_ACTIVATE"; "VDI_DEACTIVATE";
      "VDI_INTRODUCE"
    ] in
    let response = {
      driver = response.Storage.Plugin.Types.plugin;
      name = response.Storage.Plugin.Types.name;
      description = response.Storage.Plugin.Types.description;
      vendor = response.Storage.Plugin.Types.vendor;
      copyright = response.Storage.Plugin.Types.copyright;
      version = response.Storage.Plugin.Types.version;
      required_api_version = response.Storage.Plugin.Types.required_api_version;
      features;
      configuration =
       ("uri", "URI of the storage medium") ::
       response.Storage.Plugin.Types.configuration} in
    Deferred.Result.return (R.success (Args.Query.Query.rpc_of_response response))
  | { R.name = "Query.diagnostics"; R.params = [ args ] } ->
    let args = Args.Query.Diagnostics.request_of_rpc args in
    let args = Storage.Plugin.Types.Plugin.Diagnostics.In.make args.Args.Query.Diagnostics.dbg in
    let args = Storage.Plugin.Types.Plugin.Diagnostics.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    fork_exec_rpc root_dir (script root_dir name `Volume "Plugin.diagnostics") args Storage.Plugin.Types.Plugin.Diagnostics.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.Query.Diagnostics.rpc_of_response response))
  | { R.name = "SR.attach"; R.params = [ args ] } ->
    let args = Args.SR.Attach.request_of_rpc args in
    let device_config = args.Args.SR.Attach.device_config in
    begin match List.find device_config ~f:(fun (k, _) -> k = "uri") with
    | None ->
      Deferred.Result.return (R.failure (missing_uri ()))
    | Some (_, uri) ->
      let args' = Storage.Volume.Types.SR.Attach.In.make args.Args.SR.Attach.dbg uri in
      let args' = Storage.Volume.Types.SR.Attach.In.rpc_of_t args' in
      let open Deferred.Result.Monad_infix in
      fork_exec_rpc root_dir (script root_dir name `Volume "SR.attach") args' Storage.Volume.Types.SR.Attach.Out.t_of_rpc
      >>= fun response ->
      (* associate the 'sr' from the plugin with the SR reference passed in *)
      Attached_SRs.add args.Args.SR.Attach.sr response
      >>= fun () ->
      Deferred.Result.return (R.success (Args.SR.Attach.rpc_of_response response))
    end
  | { R.name = "SR.detach"; R.params = [ args ] } ->
    let args = Args.SR.Detach.request_of_rpc args in
    begin Attached_SRs.find args.Args.SR.Detach.sr
    >>= function
    | Error _ ->
      (* ensure SR.detach is idempotent *)
      Deferred.Result.return (R.success (Args.SR.Detach.rpc_of_response ()))
    | Ok sr ->
      let open Deferred.Result.Monad_infix in
      let args' = Storage.Volume.Types.SR.Detach.In.make
        args.Args.SR.Detach.dbg
        sr in
      let args' = Storage.Volume.Types.SR.Detach.In.rpc_of_t args' in
      fork_exec_rpc root_dir (script root_dir name `Volume "SR.detach") args' Storage.Volume.Types.SR.Detach.Out.t_of_rpc
      >>= fun response ->
      Attached_SRs.remove args.Args.SR.Detach.sr
      >>= fun () ->
      Deferred.Result.return (R.success (Args.SR.Detach.rpc_of_response response))
    end
  | { R.name = "SR.create"; R.params = [ args ] } ->
    let args = Args.SR.Create.request_of_rpc args in
    let device_config = args.Args.SR.Create.device_config in
    begin match List.find device_config ~f:(fun (k, _) -> k = "uri") with
    | None ->
      Deferred.Result.return (R.failure (missing_uri ()))
    | Some (_, uri) ->
      let args = Storage.Volume.Types.SR.Create.In.make
        args.Args.SR.Create.dbg
        uri
        device_config in
      let args = Storage.Volume.Types.SR.Create.In.rpc_of_t args in
      let open Deferred.Result.Monad_infix in
      fork_exec_rpc root_dir (script root_dir name `Volume "SR.create") args Storage.Volume.Types.SR.Create.Out.t_of_rpc
      >>= fun response ->
      Deferred.Result.return (R.success (Args.SR.Create.rpc_of_response response))
    end
  | { R.name = "SR.scan"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.SR.Scan.request_of_rpc args in
    Attached_SRs.find args.Args.SR.Scan.sr
    >>= fun sr ->
    let args = Storage.Volume.Types.SR.Ls.In.make
      args.Args.SR.Scan.dbg
      sr in
    let args = Storage.Volume.Types.SR.Ls.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "SR.ls") args Storage.Volume.Types.SR.Ls.Out.t_of_rpc
    >>= fun response ->
    let response = List.map ~f:vdi_of_volume response in
    Deferred.Result.return (R.success (Args.SR.Scan.rpc_of_response response))
  | { R.name = "VDI.create"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Create.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Create.sr
    >>= fun sr ->
    let vdi_info = args.Args.VDI.Create.vdi_info in
    let args = Storage.Volume.Types.Volume.Create.In.make
      args.Args.VDI.Create.dbg
      sr
      vdi_info.name_label
      vdi_info.name_description
      vdi_info.virtual_size in
    let args = Storage.Volume.Types.Volume.Create.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "Volume.create") args Storage.Volume.Types.Volume.Create.Out.t_of_rpc
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Create.rpc_of_response response))
  | { R.name = "VDI.destroy"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Destroy.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Destroy.sr
    >>= fun sr ->
    let args = Storage.Volume.Types.Volume.Destroy.In.make
      args.Args.VDI.Destroy.dbg
      sr
      args.Args.VDI.Destroy.vdi in
    let args = Storage.Volume.Types.Volume.Destroy.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "Volume.destroy") args Storage.Volume.Types.Volume.Destroy.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Destroy.rpc_of_response response))
  | { R.name = "VDI.snapshot"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Snapshot.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Snapshot.sr
    >>= fun sr ->
    let vdi_info = args.Args.VDI.Snapshot.vdi_info in
    let args = Storage.Volume.Types.Volume.Snapshot.In.make
      args.Args.VDI.Snapshot.dbg
      sr
      vdi_info.vdi in
    let args = Storage.Volume.Types.Volume.Snapshot.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "Volume.snapshot") args Storage.Volume.Types.Volume.Snapshot.Out.t_of_rpc
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Snapshot.rpc_of_response response))
  | { R.name = "VDI.clone"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Clone.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Clone.sr
    >>= fun sr ->
    let vdi_info = args.Args.VDI.Clone.vdi_info in
    let args = Storage.Volume.Types.Volume.Clone.In.make
      args.Args.VDI.Clone.dbg
      sr
      vdi_info.vdi in
    let args = Storage.Volume.Types.Volume.Clone.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "Volume.clone") args Storage.Volume.Types.Volume.Clone.Out.t_of_rpc
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Clone.rpc_of_response response))
  | { R.name = "VDI.resize"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Resize.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Resize.sr
    >>= fun sr ->
    let vdi = args.Args.VDI.Resize.vdi in
    let new_size = args.Args.VDI.Resize.new_size in
    let dbg = args.Args.VDI.Resize.dbg in
    let args = Storage.Volume.Types.Volume.Resize.In.make dbg sr vdi new_size in
    let args = Storage.Volume.Types.Volume.Resize.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "Volume.resize") args Storage.Volume.Types.Volume.Resize.Out.t_of_rpc
    >>= fun () ->
    (* Now call Volume.stat to discover the size *)
    let args = Storage.Volume.Types.Volume.Stat.In.make dbg sr vdi in
    let args = Storage.Volume.Types.Volume.Stat.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "Volume.stat") args Storage.Volume.Types.Volume.Stat.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Resize.rpc_of_response response.Storage.Volume.Types.virtual_size))
  | { R.name = "VDI.stat"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Stat.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Stat.sr
    >>= fun sr ->
    let vdi = args.Args.VDI.Stat.vdi in
    let args = Storage.Volume.Types.Volume.Stat.In.make
      args.Args.VDI.Stat.dbg
      sr
      vdi in
    let args = Storage.Volume.Types.Volume.Stat.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "Volume.stat") args Storage.Volume.Types.Volume.Stat.Out.t_of_rpc
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Stat.rpc_of_response response))
  | { R.name = "VDI.attach"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Attach.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Attach.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat root_dir name
      args.Args.VDI.Attach.dbg
      sr
      args.Args.VDI.Attach.vdi
    >>= fun (datapath, uri, domain) ->
    let args' = Storage.Datapath.Types.Datapath.Attach.In.make
      args.Args.VDI.Attach.dbg
      uri domain in
    let args' = Storage.Datapath.Types.Datapath.Attach.In.rpc_of_t args' in
    fork_exec_rpc root_dir (script root_dir name (`Datapath datapath) "Datapath.attach") args' Storage.Datapath.Types.Datapath.Attach.Out.t_of_rpc
    >>= fun response ->
    let backend, params = match response.Storage.Datapath.Types.implementation with
    | Storage.Datapath.Types.Blkback p -> "vbd", p
    | Storage.Datapath.Types.Qdisk p -> "qdisk", p
    | Storage.Datapath.Types.Tapdisk3 p -> "vbd3", p in
    let attach_info = {
      params;
      xenstore_data = [ "backend-kind", backend ];
      o_direct = true;
      o_direct_reason = "";
    } in
    Deferred.Result.return (R.success (Args.VDI.Attach.rpc_of_response attach_info))
  | { R.name = "VDI.activate"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Activate.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Activate.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat root_dir name
      args.Args.VDI.Activate.dbg
      sr
      args.Args.VDI.Activate.vdi
    >>= fun (datapath, uri, domain) ->
    let args' = Storage.Datapath.Types.Datapath.Activate.In.make
      args.Args.VDI.Activate.dbg
      uri domain in
    let args' = Storage.Datapath.Types.Datapath.Activate.In.rpc_of_t args' in
    fork_exec_rpc root_dir (script root_dir name (`Datapath datapath) "Datapath.activate") args' Storage.Datapath.Types.Datapath.Activate.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Activate.rpc_of_response ()))
  | { R.name = "VDI.deactivate"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Deactivate.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Deactivate.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat root_dir name
      args.Args.VDI.Deactivate.dbg
      sr
      args.Args.VDI.Deactivate.vdi
    >>= fun (datapath, uri, domain) ->
    let args' = Storage.Datapath.Types.Datapath.Deactivate.In.make
      args.Args.VDI.Deactivate.dbg
      uri domain in
    let args' = Storage.Datapath.Types.Datapath.Deactivate.In.rpc_of_t args' in
    fork_exec_rpc root_dir (script root_dir name (`Datapath datapath) "Datapath.deactivate") args' Storage.Datapath.Types.Datapath.Deactivate.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Deactivate.rpc_of_response ()))
  | { R.name = "VDI.detach"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Detach.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Detach.sr
    >>= fun sr ->
    (* Discover the URIs using Volume.stat *)
    stat root_dir name
      args.Args.VDI.Detach.dbg
      sr
      args.Args.VDI.Detach.vdi
    >>= fun (datapath, uri, domain) ->
    let args' = Storage.Datapath.Types.Datapath.Detach.In.make
      args.Args.VDI.Detach.dbg
      uri domain in
    let args' = Storage.Datapath.Types.Datapath.Detach.In.rpc_of_t args' in
    fork_exec_rpc root_dir (script root_dir name (`Datapath datapath) "Datapath.detach") args' Storage.Datapath.Types.Datapath.Detach.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Detach.rpc_of_response ()))
  | { R.name = "SR.stat"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.SR.Stat.request_of_rpc args in
    Attached_SRs.find args.Args.SR.Stat.sr
    >>= fun sr ->
    let args = Storage.Volume.Types.SR.Stat.In.make
      args.Args.SR.Stat.dbg
      sr in
    let args = Storage.Volume.Types.SR.Stat.In.rpc_of_t args in
    fork_exec_rpc root_dir (script root_dir name `Volume "SR.stat") args Storage.Volume.Types.SR.Stat.Out.t_of_rpc
    >>= fun response ->
    let response = {
      total_space = response.Storage.Volume.Types.total_space;
      free_space = response.Storage.Volume.Types.free_space;
    } in
    Deferred.Result.return (R.success (Args.SR.Stat.rpc_of_response response))
  | { R.name = "VDI.epoch_begin"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Epoch_begin.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Epoch_begin.sr
    >>= fun sr ->
    (* FIXME: will some backends do this in special ways?
       See [djs55/xapi-storage#19] *)
    Deferred.Result.return (R.success (Args.VDI.Epoch_begin.rpc_of_response ()))
  | { R.name = "VDI.epoch_end"; R.params = [ args ] } ->
    let open Deferred.Result.Monad_infix in
    let args = Args.VDI.Epoch_end.request_of_rpc args in
    Attached_SRs.find args.Args.VDI.Epoch_end.sr
    >>= fun sr ->
    (* FIXME: will some backends do this in special ways?
       See [djs55/xapi-storage#19] *)
    Deferred.Result.return (R.success (Args.VDI.Epoch_end.rpc_of_response ()))

  | { R.name = name } ->
    Deferred.return (Error (backend_error "UNIMPLEMENTED" [ name ])))
  >>= function
  | Result.Error error ->
    info "returning error %s" (Jsonrpc.string_of_response (R.failure error))
    >>= fun () ->
    return (Jsonrpc.string_of_response (R.failure error))
  | Result.Ok rpc ->
    return (Jsonrpc.string_of_response rpc)

(* Active servers, one per sub-directory of the root_dir *)
let servers = String.Table.create () ~size:4

(* XXX: need a better error-handling strategy *)
let get_ok = function
  | `Ok x -> x
  | `Error e ->
    let b = Buffer.create 16 in
    let fmt = Format.formatter_of_buffer b in
    Protocol_unix.Server.pp_error fmt e;
    Format.pp_print_flush fmt ();
    failwith (Buffer.contents b)

let create switch_path root_dir name =
  if Hashtbl.mem servers name
  then return ()
  else begin
    info "Adding %s" name
    >>= fun () ->
    Protocol_async.Server.listen ~process:(process root_dir name) ~switch:switch_path ~queue:(Filename.basename name) ()
    >>= fun result ->
    let server = get_ok result in
    Hashtbl.add_exn servers name server;
    return ()
  end

let destroy switch_path name =
  info "Removing %s" name
  >>= fun () ->
  if Hashtbl.mem servers name then begin
    let t = Hashtbl.find_exn servers name in
    Protocol_async.Server.shutdown ~t () >>= fun () ->
    Hashtbl.remove servers name;
    return ()
  end else return ()

let rec diff a b = match a with
  | [] -> []
  | a :: aa ->
    if List.mem b a then diff aa b else a :: (diff aa b)

(* Ensure the right servers are started *)
let sync ~root_dir ~switch_path =
  Sys.readdir root_dir
  >>= fun names ->
  let needed : string list = Array.to_list names in
  let got_already : string list = Hashtbl.keys servers in
  Deferred.all_ignore (List.map ~f:(create switch_path root_dir) (diff needed got_already))
  >>= fun () ->
  Deferred.all_ignore (List.map ~f:(destroy switch_path) (diff got_already needed))

let main ~root_dir ~state_path ~switch_path =
  Attached_SRs.reload state_path
  >>= fun () ->
  (* We watch and create queues for the Volume plugins only *)
  let root_dir = Filename.concat root_dir "volume" in
  Async_inotify.create ~recursive:false ~watch_new_dirs:false root_dir
  >>= fun (watch, _) ->
  sync ~root_dir ~switch_path
  >>= fun () ->
  let pipe = Async_inotify.pipe watch in
  let open Async_inotify.Event in
  let rec loop () =
    ( Pipe.read pipe >>= function
    | `Eof ->
      info "Received EOF from inotify event pipe"
      >>= fun () ->
      Shutdown.exit 1
    | `Ok (Created path)
    | `Ok (Moved (Into path)) ->
      create switch_path root_dir (Filename.basename path)
    | `Ok (Unlinked path)
    | `Ok (Moved (Away path)) ->
      destroy switch_path (Filename.basename path)
    | `Ok (Modified _) ->
      return ()
    | `Ok (Moved (Move (path_a, path_b))) ->
      destroy switch_path (Filename.basename path_a)
      >>= fun () ->
      create switch_path root_dir (Filename.basename path_b)
    | `Ok Queue_overflow ->
      sync ~root_dir ~switch_path
    ) >>= fun () ->
    loop () in
  loop ()

let main ~root_dir ~state_path ~switch_path =
  let (_: unit Deferred.t) = main ~root_dir ~state_path ~switch_path in
  never_returns (Scheduler.go ())

open Xcp_service

let description = String.concat ~sep:" " [
  "Allow xapi storage adapters to be written as individual scripts.";
  "To add a storage adapter, create a sub-directory in the --root directory";
  "with the name of the adapter (e.g. org.xen.xcp.storage.mylvm) and place";
  "the scripts inside.";
]

let _ =
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

  (match configure2
    ~name:"xapi-script-storage"
    ~version:Version.version
    ~doc:description
    ~resources
    () with
  | `Ok () -> ()
  | `Error x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    Pervasives.exit 1);

  if !Xcp_service.daemon then begin
    Xcp_service.maybe_daemonize ();
    use_syslog := true;
    Core.Syslog.openlog ~id:"xapi-storage-script" ~facility:Core.Syslog.Facility.DAEMON ();
  end;
  main ~root_dir:!root_dir ~state_path:!state_path ~switch_path:!Xcp_client.switch_path

