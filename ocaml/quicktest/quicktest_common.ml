(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Xapi_stdext_std.Xstringext
open Xapi_stdext_pervasives.Pervasiveext
open Forkhelpers
open Client

let rpc = Quicktest_args.rpc

let init_session username password =
  Client.Session.login_with_password ~rpc:!rpc ~uname:username ~pwd:password ~version:Datamodel_common.api_version_string ~originator:"quick_test"

let get_pool session_id =
  let pool = Client.Pool.get_all !rpc session_id in
  if List.length pool <> 1 then (failwith "Number of pools isn't zero!");
  List.hd pool

let http request f =
  let open Xmlrpc_client in
  let transport =
    if !Quicktest_args.using_unix_domain_socket
    then Unix Xapi_globs.unix_domain_socket
    else SSL(SSL.make ~use_fork_exec_helper:false (), !Quicktest_args.host, 443) in
  with_transport transport (with_http request f)

let vm_template = "Demo Linux VM"
let other = "Other install media"

exception Unable_to_find_suitable_vm_template

let find_template session_id startswith =
  let vms = Client.VM.get_all !rpc session_id in
  match List.filter (fun self ->
      (String.startswith startswith (Client.VM.get_name_label !rpc session_id self))
      && (Client.VM.get_is_a_template !rpc session_id self)
    ) vms with
  | [] -> raise Unable_to_find_suitable_vm_template
  | x :: _ ->
    (* Printf.printf "Choosing template with name: %s\n" (Client.VM.get_name_label !rpc session_id x); *)
    x

let cli_cmd args =
  print_endline (String.concat " " ("$ xe" :: args));
  try
    let output = String.rtrim (fst(Forkhelpers.execute_command_get_output !Quicktest_args.xe_path args)) in
    print_endline output;
    output
  with
  | Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED n) ->
    Alcotest.fail (Printf.sprintf "CLI failed: exit code=%d output=[%s] log=[%s]" n output log)
  | Forkhelpers.Spawn_internal_error(log, output, _) ->
    Alcotest.fail (Printf.sprintf "CLI failed: exit code unkown; output=[%s] log=[%s]" output log)
  | e ->
    Alcotest.fail ("CLI failed" ^ (Printexc.to_string e))

let vm_install session_id template name =
  let newvm_uuid = cli_cmd [ "vm-install"; "template-uuid=" ^ template; "new-name-label=" ^ name ] in
  Client.VM.get_by_uuid !rpc session_id newvm_uuid

let template_uninstall test session_id vm =
  let uuid = Client.VM.get_uuid !rpc session_id vm in
  ignore(cli_cmd [ "template-uninstall"; "template-uuid=" ^ uuid; "--force" ])

let install_vm session_id =
  let t = find_template session_id vm_template in
  let uuid = Client.VM.get_uuid !rpc session_id t in
  print_endline (Printf.sprintf "Template has uuid: %s%!" uuid);
  let vm = vm_install session_id uuid "quicktest" in
  (* Prevent the guest startup blocking at the enter password stage *)
  Client.VM.set_PV_args !rpc session_id vm "noninteractive";
  vm

let find_guest_installer_network session_id =
  let all = Client.Network.get_all_records !rpc session_id in
  match List.filter (fun (_, r) -> List.mem_assoc Xapi_globs.is_guest_installer_network r.API.network_other_config) all with
  | (rf, _) :: _ -> rf
  | _ -> failwith "Could not find guest installer network"

(** Return a host's domain zero *)
let dom0_of_host session_id host =
  Client.Host.get_control_domain !rpc session_id host

let get_default_sr session_id =
  Client.Pool.get_default_SR ~session_id ~rpc:!rpc ~self:(get_pool session_id)

let assert_raises_match exception_match fn =
  try
    fn ();
    Alcotest.fail "assert_raises_match: failure expected"
  with failure ->
    if not (exception_match failure)
    then raise failure
    else ()
