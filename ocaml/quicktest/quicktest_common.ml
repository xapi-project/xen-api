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
open Stdext
open Xstringext
open Pervasiveext
open Forkhelpers
open Client

let ( +* ) = Int64.add
let ( ** ) = Int64.mul
let mib = 1024L ** 1024L
let gib = mib ** 1024L

(***********************************************************************)
(* Nice output stuff                                                   *)

let total_started = ref 0
let total_passed = ref 0

type status = Pending | Success | Failed
type vt100 = Control of string | Data of string
let length_of_vt100 sequence =
  let length = function
    | Control _ -> 0 | Data x -> String.length x in
  List.fold_left (+) 0 (List.map length sequence)
let flatten_vt100 sequence = List.fold_left (^) "" (List.map (function Control x -> x | Data x -> x) sequence)

let escape = String.make 1 (char_of_int 0x1b)
let set_attribute attrs = Control(Printf.sprintf "%s[%sm" escape (String.concat ";" (List.map string_of_int attrs)))
let reset  = 0
let bright = 1
let dim    = 2
let red    = 31
let green  = 32
let blue   = 34
let yellow = 33
let basic_string_of_status = function
  | Pending -> [ Data "           " ]
  | Success -> [ Data "[ Success ]" ]
  | Failed  -> [ Data "[ Failed  ]" ]
let coloured_string_of_status = function
  | Pending -> [ Data "           " ]
  | Success ->
    [ Data "[ ";
      set_attribute [ bright; green ];
      Data "Success";
      set_attribute [ reset ];
      Data " ]" ]
  | Failed ->
    [ Data "[ ";
      set_attribute [ bright; red ];
      Data "Failed ";
      set_attribute [ reset ];
      Data " ]" ]

let xe_path = ref "/opt/xensource/bin/xe"
let use_colour = ref true

let cols = 80
let nice_status_output indent name status =
  let vt100 = (if !use_colour then coloured_string_of_status else basic_string_of_status) status in
  let flattened = flatten_vt100 vt100 in
  let invisible_length = String.length flattened in
  let visible_length = length_of_vt100 vt100 in

  (* Need a bigger string to cope with control characters *)
  let line = String.make (cols + (invisible_length - visible_length)) ' ' in
  (* Stick the test name towards the left, with indent *)
  String.blit name 0 line indent (min (cols - indent) (String.length name));

  (* Stick the coloured bit towards the right *)
  (* NB we need to use the 'visible length' for positioning but copy all chars, even invis ones *)
  String.blit (flatten_vt100 vt100) 0 line (cols - visible_length) (String.length flattened);
  line

type test_description = {
  name: string;
  indent: int;
  mutable status: status;
}

let all_tests = Hashtbl.create 10

let make_test name indent = { name = name; indent = indent; status = Pending }

let rec failed (test: test_description) msg =
  if not (Hashtbl.mem all_tests test.name)
  then failwith (Printf.sprintf "Test not started: %s" test.name);
  if Hashtbl.mem all_tests test.name then Hashtbl.remove all_tests test.name;
  test.status <- Failed;
  debug test msg;
  Printf.printf "%s\n" (nice_status_output test.indent "" Failed);
  flush stdout

and start (test: test_description) =
  incr total_started;
  Hashtbl.add all_tests test.name test;
  Printf.printf "%s\n" (nice_status_output test.indent test.name Pending);
  flush stdout

and debug (test: test_description) msg =
  (* Might need to divide into multiple lines *)
  let max_length = cols - length_of_vt100 (coloured_string_of_status test.status) - test.indent - 1 in
  let rec loop start_offset =
    if start_offset < String.length msg then begin
      let length = min (String.length msg - start_offset) max_length in
      let submsg = String.sub msg start_offset length in
      Printf.printf "%s\n" (nice_status_output (test.indent + 1) submsg test.status);
      loop (start_offset + length)
    end in
  loop 0;
  flush stdout

let success (test: test_description) =
  if not (Hashtbl.mem all_tests test.name)
  then failwith (Printf.sprintf "Test not started: %s" test.name);

  if Hashtbl.mem all_tests test.name then Hashtbl.remove all_tests test.name;
  if test.status = Pending then begin
    incr total_passed;
    test.status <- Success
  end;
  Printf.printf "%s\n" (nice_status_output test.indent "" test.status);
  flush stdout

let summarise () =
  Printf.printf "\n\nTotal tests started: %d; total passed: %d (pass rate %.2f%c)\n" !total_started !total_passed
    (float_of_int !total_passed /. (float_of_int !total_started) *. 100.) '%';
  flush stdout;
  Hashtbl.iter (fun name test -> Printf.printf "Test neither succeeded nor failed: %s\n" name) all_tests;
  if !total_passed <> !total_started then begin
    Printf.printf "*** Some tests failed ***\n";
    flush stdout;
    exit 1;
  end


let host = ref ""

open Xmlrpc_client
let http = xmlrpc ~version:"1.1" "/"
let rpc_remote xml = XMLRPC_protocol.rpc ~srcstr:"quicktest" ~dststr:"xapi" ~transport:(SSL(SSL.make(), !host, 443)) ~http xml
let rpc_unix_domain xml = XMLRPC_protocol.rpc ~srcstr:"quicktest" ~dststr:"xapi" ~transport:(Unix Xapi_globs.unix_domain_socket) ~http xml

let rpc = ref rpc_unix_domain

let using_unix_domain_socket = ref true

let init_session username password =
  Client.Session.login_with_password ~rpc:!rpc ~uname:username ~pwd:password ~version:Datamodel_common.api_version_string ~originator:"quick_test"

let get_pool session_id =
  let pool = Client.Pool.get_all !rpc session_id in
  if List.length pool <> 1 then (failwith "Number of pools isn't zero!");
  List.hd pool

let http request f =
  let open Xmlrpc_client in
  let transport =
    if !using_unix_domain_socket
    then Unix Xapi_globs.unix_domain_socket
    else SSL(SSL.make ~use_fork_exec_helper:false (), !host, 443) in
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

let cli_cmd test args =
  debug test (String.concat " " ("$ xe" :: args));
  try
    let output = String.rtrim (fst(Forkhelpers.execute_command_get_output !xe_path args)) in
    debug test output;
    output
  with
  | Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED n) ->
    failed test (Printf.sprintf "Exit code: %d" n);
    failed test (Printf.sprintf "output=[%s] log=[%s]" output log);
    failwith "CLI failed"
  | Forkhelpers.Spawn_internal_error(log, output, _) ->
    failed test "Exit code unknown";
    failed test (Printf.sprintf "output=[%s] log=[%s]" output log);
    failwith "CLI failed"
  | e ->
    failed test (Printexc.to_string e);
    failwith "CLI failed"

let vm_install test session_id template name =
  let newvm_uuid = cli_cmd test [ "vm-install"; "template-uuid=" ^ template; "new-name-label=" ^ name ] in
  Client.VM.get_by_uuid !rpc session_id newvm_uuid

let template_uninstall test session_id vm =
  let uuid = Client.VM.get_uuid !rpc session_id vm in
  ignore(cli_cmd test [ "template-uninstall"; "template-uuid=" ^ uuid; "--force" ])

let vm_uninstall test session_id vm =
  let uuid = Client.VM.get_uuid !rpc session_id vm in
  ignore(cli_cmd test [ "vm-uninstall"; "uuid=" ^ uuid; "--force" ])

let vm_export ?(metadata_only=false) test session_id vm filename =
  let uuid = Client.VM.get_uuid !rpc session_id vm in
  let args = [ "vm-export"; "vm=" ^ uuid; "filename=" ^ filename ] in
  let args = if metadata_only then args @ [ "metadata=true" ] else args in
  ignore(cli_cmd test args)

let vm_import ?(metadata_only=false) ?(preserve=false) ?sr test session_id filename =
  let sr_uuid = Opt.map (fun sr -> Client.SR.get_uuid !rpc session_id sr) sr in
  let args = [ "vm-import"; "filename=" ^ filename ] in
  let args = args @ (Opt.default [] (Opt.map (fun x -> [ "sr-uuid=" ^ x ]) sr_uuid)) in
  let args = if metadata_only then args @ [ "metadata=true" ] else args in
  let args = if preserve then args @ [ "preserve=true" ] else args in
  let newvm_uuids = String.split ',' (cli_cmd test args) in
  List.map (fun uuid -> Client.VM.get_by_uuid !rpc session_id uuid) newvm_uuids

let install_vm test session_id =
  let t = find_template session_id vm_template in
  let uuid = Client.VM.get_uuid !rpc session_id t in
  debug test (Printf.sprintf "Template has uuid: %s%!" uuid);
  let vm = vm_install test session_id uuid "quicktest" in
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
