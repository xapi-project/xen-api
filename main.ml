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

(* If we get a general Error.t then perform some diagnosis to report
   the most 'actionable' error we can. *)
let backend_error script_name e =
  let marshal name args =
    let open Storage_interface in
    let exnty = Exception.Backend_error (name, args) in
    let rpc = Exception.rpc_of_exnty exnty in
    return (Jsonrpc.string_of_response (R.failure rpc)) in
  Sys.is_file ~follow_symlinks:true script_name
  >>= function
  | `No | `Unknown ->
    marshal "SCRIPT_MISSING" [ script_name; "Check whether the file exists and has correct permissions" ]
  | `Yes ->
    begin Unix.access script_name [ `Exec ]
    >>= function
    | Error exn ->
      marshal "SCRIPT_NOT_EXECUTABLE" [ script_name; Exn.to_string exn ]
    | Ok () ->
      marshal "SCRIPT_FAILED" [ script_name; Error.to_string_hum e ]
    end

(* Process a message *)
let process root_dir name x =
  let open Storage_interface in
  let call = Jsonrpc.call_of_string x in
  let script_name = Filename.(concat (concat root_dir name) call.R.name) in
  (match call with
  | { R.name = "Query.query"; R.params = [ args ] } ->
    let args = Args.Query.Query.request_of_rpc args in
    (* convert to new storage interface *)
    let args = Storage.P.Types.Plugin.Query.In.make args.Args.Query.Query.dbg in
    let args = Storage.P.Types.Plugin.Query.In.rpc_of_t args in
    let open Deferred.Result.Monad_infix in
    Process.create ~prog:script_name ~args:["--json"] ~working_dir:root_dir ()
    >>= fun p ->
    (* Send the request as json on stdin *)
    let w = Process.stdin p in
    Writer.write w (Jsonrpc.to_string args);
    let open Deferred.Monad_infix in
    Writer.close w
    >>= fun () ->
    Process.wait p
    >>= fun output ->
    let open Deferred.Result.Monad_infix in
    (* Check we got a zero exit code *)
    Deferred.return (Unix.Exit_or_signal.or_error output.Process.Output.exit_status)
    >>= fun () ->
    (* Parse the json on stdout *)
    (fun () -> Jsonrpc.of_string output.Process.Output.stdout)
    |> Or_error.try_with
    |> Deferred.return
    >>= fun response ->
    (* Parse the json on stdin *)
    (fun () -> Storage.P.Types.Plugin.Query.Out.t_of_rpc response)
    |> Or_error.try_with
    |> Deferred.return
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
      configuration = response.Storage.P.Types.configuration} in
    Deferred.Result.return (R.success (Args.Query.Query.rpc_of_response response))
  | _ ->
    (* NB we don't call backend_error to perform diagnosis because we don't
       want to look up paths with user-supplied elements. *)
    Deferred.Result.return (R.failure (R.String "hello")))
  >>= function
  | Result.Error e ->
    backend_error script_name e
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
