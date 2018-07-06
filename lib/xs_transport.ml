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

let xenstored_socket = ref "/var/run/xenstored/socket"

(* We'll look for these paths in order: *)
let get_xenstore_paths () =
  let default = [
    !xenstored_socket;
    "/proc/xen/xenbus"; (* Linux *)
    "/dev/xen/xenstore"; (* FreeBSD *)
  ] in
  try
    Sys.getenv "XENSTORED_PATH" :: default
  with Not_found -> default

let choose_xenstore_path () =
  List.fold_left (fun acc possibility -> match acc with
      | Some x -> Some x
      | None ->
        if Sys.file_exists possibility then Some possibility else None
    ) None (get_xenstore_paths ())

exception Could_not_find_xenstore
