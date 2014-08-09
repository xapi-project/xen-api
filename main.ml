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

open Xcp_service

let description = String.concat ~sep:" " [
  "Allow xapi storage adapters to be written as individual scripts.";
  "To add a storage adapter, create a sub-directory in the --root directory";
  "with the name of the adapter (e.g. org.xen.xcp.storage.mylvm) and place";
  "the scripts inside.";
]

let root_dir = ref "/var/lib/xapi/storage-scripts"

let resources = [
  { Xcp_service.name = "root";
    description = "directory whose sub-directories contain sets of per-operation scripts, one sub-directory per queue name";
    essential = true;
    path = root_dir;
    perms = [ U.X_OK ];
  }
]

let _ =
  match configure2
    ~name:"xapi-script-storage"
    ~version:Version.version
    ~doc:description
    ~resources
    () with
  | `Ok () -> ()
  | `Error x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    Pervasives.exit 1
