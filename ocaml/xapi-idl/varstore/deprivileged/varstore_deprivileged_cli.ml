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

let version_str description =
  let maj, min, mic = description.Idl.Interface.version in
  Printf.sprintf "%d.%d.%d" maj min mic

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
  let doc = "Path to deprivileged socket in /var/run/xen" in
  let path =
    Arg.(required & opt (some file) None & info ["socket"] ~doc ~docv:"SOCKET")
  in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info =
    Cmd.info "varstored_cli"
      ~version:(version_str Cmds.description)
      ~doc:"debug CLI"
  in
  let cmds =
    List.map
      (fun t ->
        let term, info = t rpc in
        Cmd.v info Term.(const wrapper $ path $ term $ const ())
      )
      (Cmds.implementation ())
  in
  Cmd.eval (Cmd.group ~default info cmds)

let () = exit (cli ())
