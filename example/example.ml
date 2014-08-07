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
module D = Debug.Make(struct let name = "example" end)
open D

let ls = ref "/bin/ls"
let sh = ref "/bin/sh"

let resources = [
  { Xcp_service.name = "ls";
    description = "used to list things";
    essential = true;
    path = ls;
    perms = [ Unix.X_OK ];
  }; {
    Xcp_service.name = "sh";
    description = "type commands in here and see what they do";
    essential = false;
    path = sh;
    perms = [ Unix.X_OK ];
  }
]

let socket_path = ref "/var/xapi/socket"

let comma = Re_str.regexp_string ","
let csv = Re_str.split_delim comma

let queues : string list ref = ref [
  "org.xen.xcp.ffs";
]

let set_default_format _ = ()
let get_default_format () = "vhd"

let mount_path = ref "/mnt"

let options = [
  "use-switch", Arg.Set Xcp_client.use_switch, (fun () -> string_of_bool !Xcp_client.use_switch), "true if we want to use the message switch";
  "socket-path", Arg.Set_string socket_path, (fun () -> !socket_path), "Path of listening socket";
  "queue-name", Arg.String (fun x -> queues := csv x), (fun () -> String.concat "," !queues), "Comma-separated list of queue names to listen on";
  "default-format", Arg.String set_default_format, get_default_format, "Default format for disk files";
  "sr-mount-path", Arg.Set_string mount_path, (fun () -> !mount_path), "Default mountpoint for mounting remote filesystems";
]

let _ =
  configure ~options ~resources ();
