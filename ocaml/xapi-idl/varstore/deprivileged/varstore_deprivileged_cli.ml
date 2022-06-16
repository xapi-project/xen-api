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

module Cmds = Varstore_deprivileged_interface.RPC_API (Cmdlinergen.Gen ())

open! Cmdliner

let cli () =
  let uri = ref "" in
  let rpc call =
    Xcp_client.xml_http_rpc
      ~srcstr:(Xcp_client.get_user_agent ())
      ~dststr:"varstored"
      (fun () -> !uri)
      call
  in
  let wrapper file next =
    uri := "file://" ^ file ;
    next
  in
  let path =
    let doc = "Path to deprivileged socket in /var/run/xen" in
    Arg.(required & opt (some file) None & info ["socket"] ~doc ~docv:"SOCKET")
  in
  let cmdline_gen () =
    List.map
      (fun t ->
        let term, info = t rpc in
        (Term.(const wrapper $ path $ term $ const ()), info)
      )
      (Cmds.implementation ())
  in
  Xcp_service.cli ~name:"varstored_cli" ~doc:"debug CLI"
    ~version:Cmds.description.version ~cmdline_gen

let () = exit (Cmd.eval @@ cli ())
