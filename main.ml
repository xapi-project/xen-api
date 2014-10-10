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

open Core.Std
open Async.Std

let backend_error name args =
  Printf.fprintf stderr "backend_error %s [ %s ]\n%!" name (String.concat ~sep:"; " args);
  let open Storage_interface in
  let exnty = Exception.Backend_error (name, args) in
  Exception.rpc_of_exnty exnty

let missing_uri () =
  backend_error "MISSING_URI" [ "Please include a URI in the device-config" ]

(* If we get a general Error.t then perform some diagnosis to report
   the most 'actionable' error we can. *)
let diagnose script_name e =
  Sys.is_file ~follow_symlinks:true script_name
  >>= function
  | `No | `Unknown ->
    return (backend_error "SCRIPT_MISSING" [ script_name; "Check whether the file exists and has correct permissions" ])
  | `Yes ->
    begin Unix.access script_name [ `Exec ]
    >>= function
    | Error exn ->
      return (backend_error "SCRIPT_NOT_EXECUTABLE" [ script_name; Exn.to_string exn ])
    | Ok () ->
      return (backend_error "SCRIPT_FAILED" [ script_name; Error.to_string_hum e ])
    end

let fork_exec_rpc root_dir script_name args response_of_rpc =
  Process.create ~prog:script_name ~args:["--json"] ~working_dir:root_dir ()
  >>= function
  | Error e ->
    diagnose script_name e
    >>= fun response ->
    return (Error response)
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
      return (Error (backend_error "SCRIPT_FAILED" [ script_name; "non-zero exit"; string_of_int code ]))
    | Error (`Signal signal) ->
      return (Error (backend_error "SCRIPT_FAILED" [ script_name; "signalled"; Signal.to_string signal ]))
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

let vdi_of_volume x =
  let open Storage_interface in {
  vdi = x.Storage.V.Types.key;
  content_id = "";
  name_label = x.Storage.V.Types.name;
  name_description = x.Storage.V.Types.description;
  ty = "";
  metadata_of_pool = "";
  is_a_snapshot = false;
  snapshot_time = "";
  snapshot_of = "";
  read_only = not x.Storage.V.Types.read_write;
  virtual_size = x.Storage.V.Types.virtual_size;
  physical_utilisation = 0L;
  sm_config = [];
  persistent = true;
}

(* Process a message *)
let process root_dir name x =
  let open Storage_interface in
  let call = Jsonrpc.call_of_string x in
  let script script = Filename.(concat (concat root_dir name) script) in
  (match call with
  | { R.name = "Query.query"; R.params = [ args ] } ->
    let args = Args.Query.Query.request_of_rpc args in
    (* convert to new storage interface *)
    let args = Storage.P.Types.Plugin.Query.In.make args.Args.Query.Query.dbg in
    let args = Storage.P.Types.Plugin.Query.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    fork_exec_rpc root_dir (script "Plugin.Query") args Storage.P.Types.Plugin.Query.Out.t_of_rpc
    >>= fun response ->
    (* Convert between the xapi-storage interface and the SMAPI *)
    let response = {
      driver = response.Storage.P.Types.plugin;
      name = response.Storage.P.Types.name;
      description = response.Storage.P.Types.description;
      vendor = response.Storage.P.Types.vendor;
      copyright = response.Storage.P.Types.copyright;
      version = response.Storage.P.Types.version;
      required_api_version = response.Storage.P.Types.required_api_version;
      features = response.Storage.P.Types.features;
      configuration =
       ("uri", "URI of the storage medium") ::
       response.Storage.P.Types.configuration} in
    Deferred.Result.return (R.success (Args.Query.Query.rpc_of_response response))
  | { R.name = "SR.attach"; R.params = [ args ] } ->
    let args = Args.SR.Attach.request_of_rpc args in
    let device_config = args.Args.SR.Attach.device_config in
    begin match List.find device_config ~f:(fun (k, _) -> k = "uri") with
    | None ->
      Deferred.Result.return (R.failure (missing_uri ()))
    | Some (_, uri) ->
      let args = Storage.V.Types.SR.Attach.In.make args.Args.SR.Attach.dbg uri in
      let args = Storage.V.Types.SR.Attach.In.rpc_of_t args in
      let open Deferred.Result.Monad_infix in
      fork_exec_rpc root_dir (script "SR.attach") args Storage.V.Types.SR.Attach.Out.t_of_rpc
      >>= fun response ->
      Deferred.Result.return (R.success (Args.SR.Attach.rpc_of_response response))
    end
  | { R.name = "SR.detach"; R.params = [ args ] } ->
    let args = Args.SR.Detach.request_of_rpc args in
    let args = Storage.V.Types.SR.Detach.In.make
      args.Args.SR.Detach.dbg
      args.Args.SR.Detach.sr in
    let args = Storage.V.Types.SR.Detach.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    fork_exec_rpc root_dir (script "SR.detach") args Storage.V.Types.SR.Detach.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.SR.Detach.rpc_of_response response))
  | { R.name = "SR.create"; R.params = [ args ] } ->
    let args = Args.SR.Create.request_of_rpc args in
    let device_config = args.Args.SR.Create.device_config in
    begin match List.find device_config ~f:(fun (k, _) -> k = "uri") with
    | None ->
      Deferred.Result.return (R.failure (missing_uri ()))
    | Some (_, uri) ->
      let args = Storage.V.Types.SR.Create.In.make
        args.Args.SR.Create.dbg
        uri
        device_config in
      let args = Storage.V.Types.SR.Create.In.rpc_of_t args in
      let open Deferred.Result.Monad_infix in
      fork_exec_rpc root_dir (script "SR.create") args Storage.V.Types.SR.Create.Out.t_of_rpc
      >>= fun response ->
      Deferred.Result.return (R.success (Args.SR.Create.rpc_of_response response))
    end
  | { R.name = "SR.scan"; R.params = [ args ] } ->
    let args = Args.SR.Scan.request_of_rpc args in
    let args = Storage.V.Types.SR.Ls.In.make
      args.Args.SR.Scan.dbg
      args.Args.SR.Scan.sr in
    let args = Storage.V.Types.SR.Ls.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    fork_exec_rpc root_dir (script "SR.ls") args Storage.V.Types.SR.Ls.Out.t_of_rpc
    >>= fun response ->
    let response = List.map ~f:vdi_of_volume response in
    Deferred.Result.return (R.success (Args.SR.Scan.rpc_of_response response))
  | { R.name = "VDI.create"; R.params = [ args ] } ->
    let args = Args.VDI.Create.request_of_rpc args in
    let vdi_info = args.Args.VDI.Create.vdi_info in
    let args = Storage.V.Types.Volume.Create.In.make
      args.Args.VDI.Create.dbg
      args.Args.VDI.Create.sr
      vdi_info.name_label
      vdi_info.name_description
      vdi_info.virtual_size in
    let args = Storage.V.Types.Volume.Create.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    fork_exec_rpc root_dir (script "Volume.create") args Storage.V.Types.Volume.Create.Out.t_of_rpc
    >>= fun response ->
    let response = vdi_of_volume response in
    Deferred.Result.return (R.success (Args.VDI.Create.rpc_of_response response))
  | { R.name = "VDI.destroy"; R.params = [ args ] } ->
    let args = Args.VDI.Destroy.request_of_rpc args in
    let args = Storage.V.Types.Volume.Destroy.In.make
      args.Args.VDI.Destroy.dbg
      args.Args.VDI.Destroy.sr
      args.Args.VDI.Destroy.vdi in
    let args = Storage.V.Types.Volume.Destroy.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    fork_exec_rpc root_dir (script "Volume.destroy") args Storage.V.Types.Volume.Destroy.Out.t_of_rpc
    >>= fun response ->
    Deferred.Result.return (R.success (Args.VDI.Destroy.rpc_of_response response))
  | _ ->
    Deferred.Result.return (R.failure (R.String "hello")))
  >>= function
  | Result.Error error ->
    Printf.fprintf stderr "returning %s\n%!" (Jsonrpc.string_of_response (R.failure error));
    return (Jsonrpc.string_of_response (R.failure error))
  | Result.Ok rpc ->
    return (Jsonrpc.string_of_response rpc)

(* Active servers, one per sub-directory of the root_dir *)
let servers = String.Table.create () ~size:4

let create switch_port root_dir name =
  if Hashtbl.mem servers name
  then return ()
  else begin
    Printf.fprintf stderr "Adding %s\n%!" name;
    Protocol_async.M.connect switch_port >>= fun c ->
    let server = Protocol_async.Server.listen (process root_dir name) c (Filename.basename name) in
    Hashtbl.add_exn servers name server;
    return ()
  end

let destroy switch_port name =
  Printf.fprintf stderr "Removing %s\n%!" name;
  Protocol_async.M.connect switch_port >>= fun c ->
  Hashtbl.remove servers name;
  return ()

let rec diff a b = match a with
  | [] -> []
  | a :: aa ->
    if List.mem b a then diff aa b else a :: (diff aa b)

(* Ensure the right servers are started *)
let sync ~root_dir ~switch_port =
  Sys.readdir root_dir
  >>= fun names ->
  let needed : string list = Array.to_list names in
  let got_already : string list = Hashtbl.keys servers in
  Deferred.all_ignore (List.map ~f:(create switch_port root_dir) (diff needed got_already))
  >>= fun () ->
  Deferred.all_ignore (List.map ~f:(destroy switch_port) (diff got_already needed))

let main ~root_dir ~switch_port =
  Async_inotify.create ~recursive:false ~watch_new_dirs:false root_dir
  >>= fun (watch, _) ->
  sync ~root_dir ~switch_port
  >>= fun () ->
  let pipe = Async_inotify.pipe watch in
  let open Async_inotify.Event in
  let rec loop () =
    ( Pipe.read pipe >>= function
    | `Eof ->
      Printf.fprintf stderr "Received EOF from inotify event pipe\n%!";
      Shutdown.exit 1
    | `Ok (Created name)
    | `Ok (Moved (Into name)) ->
      create switch_port root_dir name
    | `Ok (Unlinked name)
    | `Ok (Moved (Away name)) ->
      destroy switch_port name
    | `Ok (Modified _) ->
      return ()
    | `Ok (Moved (Move (a, b))) ->
      destroy switch_port a
      >>= fun () ->
      create switch_port root_dir b
    | `Ok Queue_overflow ->
      sync ~root_dir ~switch_port
    ) >>= fun () ->
    loop () in
  loop ()

let main ~root_dir ~switch_port =
  let (_: unit Deferred.t) = main ~root_dir ~switch_port in
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

  let resources = [
    { Xcp_service.name = "root";
      description = "directory whose sub-directories contain sets of per-operation scripts, one sub-directory per queue name";
      essential = true;
      path = root_dir;
      perms = [ U.X_OK ];
    }
  ] in

  match configure2
    ~name:"xapi-script-storage"
    ~version:Version.version
    ~doc:description
    ~resources
    () with
  | `Ok () -> main ~root_dir:!root_dir ~switch_port:!Xcp_client.switch_port
  | `Error x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    Pervasives.exit 1
