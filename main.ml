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
open Core.Std
open Async.Std

(* Process a message *)
let process x = return x

(* Active servers, one per sub-directory of the root_dir *)
let servers = String.Table.create () ~size:4

let create switch_port name =
  if Hashtbl.mem servers name
  then return ()
  else begin
    Printf.fprintf stderr "Adding %s\n%!" name;
    Protocol_async.M.connect switch_port >>= fun c ->
    let server = Protocol_async.Server.listen process c (Filename.basename name) in
    Hashtbl.add_exn servers name server;
    return ()
  end

let destroy name =
  Printf.fprintf stderr "Removing %s\n%!" name;
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
  let needed = Array.to_list names in
  let got_already = Hashtbl.keys servers in
  Deferred.all_ignore (List.map ~f:(create switch_port) (diff needed got_already))
  >>= fun () ->
  Deferred.all_ignore (List.map ~f:destroy (diff got_already needed))

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
      create switch_port name
    | `Ok (Unlinked name)
    | `Ok (Moved (Away name)) ->
      destroy name
    | `Ok (Modified _) ->
      return ()
    | `Ok (Moved (Move (a, b))) ->
      destroy a
      >>= fun () ->
      create switch_port b
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
