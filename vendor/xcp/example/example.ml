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

open Xcp_service

let ls = ref "/bin/ls"
let sh = ref "/bin/sh"

let resources = [
  { Xcp_service.name = "ls";
    description = "program used to list things";
    essential = true;
    path = ls;
    perms = [ Unix.X_OK ];
  }; {
    Xcp_service.name = "sh";
    description = "interpreter for arcane programming language";
    essential = false;
    path = sh;
    perms = [ Unix.X_OK ];
  }
]

let socket_path = ref "/var/xapi/socket"

let comma = Re.Str.regexp_string ","
let csv = Re.Str.split_delim comma

let queues : string list ref = ref [
  "org.xen.xapi.ffs";
]

let set_default_format _ = ()
let get_default_format () = "vhd"

let mount_path = ref "/mnt"

let options = [
  "socket-path", Arg.Set_string socket_path, (fun () -> !socket_path), "Path of listening socket";
  "queue-name", Arg.String (fun x -> queues := csv x), (fun () -> String.concat "," !queues), "Comma-separated list of queue names to listen on";
  "default-format", Arg.String set_default_format, get_default_format, "Default format for disk files";
  "sr-mount-path", Arg.Set_string mount_path, (fun () -> !mount_path), "Default mountpoint for mounting remote filesystems";
]

let _ =
  Debug.log_to_stdout ();
  match configure2
    ~name:"Example-service"
    ~version:"1.0"
    ~doc:"This is an example service which demonstrates the configuration mechanism."
    ~options
    ~resources
    () with
  | `Ok () -> exit 0
  | `Error m ->
    Printf.fprintf stderr "Error: %s\n%!" m;
    exit 1
