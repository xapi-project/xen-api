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

open Varstore_privileged_interface

module Cmds = RPC_API (Cmdlinergen.Gen ())

let doc =
  String.concat " "
    [
      "A CLI for the deprivileged socket spawning API."
    ; "This allows scripting of the varstored deprivileging daemon"
    ; "for testing and debugging. This tool is not intended to be used"
    ; "as an end user tool"
    ]

let cmdline_gen () =
  List.map (fun t -> t Varstore_privileged_client.rpc) (Cmds.implementation ())

let cli =
  Xcp_service.cli ~name:"varstore_cli" ~doc ~version:Cmds.description.version
    ~cmdline_gen

let () = Xcp_service.eval_cmdline cli
