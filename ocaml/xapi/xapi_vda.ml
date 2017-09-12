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

module D = Debug.Make(struct let name="xapi_vda" end)
open D

let find_vda ~__context ~vm =
  match Db.VDA.get_refs_where ~__context
    ~expr:(Db_filter_types.(Eq (Field "vm", Literal (Ref.string_of vm)))) with
  | ref::[] -> Some ref
  | ref::_ -> (* should never happen; this occurance indicates a bug *)
    let msg = "Multiple VDAs found for VM" in
    error "%s %s" msg (Db.VM.get_uuid ~__context ~self:vm);
    raise Api_errors.(Server_error(internal_error, [msg; (Ref.string_of vm)]))
  | _ -> None

let assert_vm_is_valid ~__context ~vm =
  if vm = Ref.null
  then
    let msg = "VM reference is null" in
    error "%s" msg;
    raise Api_errors.(Server_error(internal_error, [msg]))

let assert_vm_has_no_vda ~__context ~vm =
  match Db.VDA.get_records_where ~__context
    ~expr:(Db_filter_types.(Eq (Field "vm", Literal (Ref.string_of vm)))) with
  | (_,record)::_ ->
    let msg = "VDA already exists" in
    error "%s (%s)" msg record.API.vDA_uuid;
    raise Api_errors.(Server_error(internal_error, [msg; record.API.vDA_uuid]))
  | _ -> ()

let assert_vda_version_is_valid ~__context ~version =
  if version = ""
  then
    let msg = "VDA version cannot be empty" in
    error "%s" msg;
    raise Api_errors.(Server_error(internal_error, [msg]))

(** Assert that the VDA's VM is in a certain power state *)
let assert_vm_power_state_is ~__context ~self ~expected =
  let vm = Db.VDA.get_vm ~__context ~self in
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vm ~expected

let create ~__context ~vm ~version =
  assert_vm_is_valid ~__context ~vm;
  assert_vm_has_no_vda ~__context ~vm;
  assert_vda_version_is_valid ~__context ~version;
  let vda = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.VDA.create ~__context ~ref:vda ~uuid ~vm ~version;
  vda

let destroy ~__context ~self =
  Db.VDA.destroy ~__context ~self

let copy ~__context ~vm vda =
  let record = Db.VDA.get_record ~__context ~self:vda in
  create ~__context ~vm ~version:record.API.vDA_version

let make_command cmd =
    Rpc.call cmd []
    |> Jsonrpc.string_of_call

exception ResponseError

let parse_response out =
    let open Rpc in
    let response = Jsonrpc.response_of_string out in
    if response.success then
      string_of_rpc response.contents
    else begin
      error "Error %s while parsing VDA response" (Rpc.string_of_rpc response.contents);
      raise ResponseError
    end
  
let call_guestcommands vda tty command =
  let output, _ =
    try
      Forkhelpers.execute_command_get_output ~timeout:!Xapi_globs.vda_communication_timeout
        Xapi_globs.vda_communication_script ["--tty"; tty; make_command command]
    with
    | Forkhelpers.Subprocess_timeout ->
      error "VDA.%s communication timeout" command;
      raise Api_errors.(Server_error(vda_communication_timeout, [Ref.string_of vda]))
    | Forkhelpers.Spawn_internal_error(log, _output, ps) ->
      let ps_kind =
        match ps with
        | Unix.WEXITED i -> Printf.sprintf "Exit %d" i
        | Unix.WSIGNALED i -> Printf.sprintf "Killed by signal %d" i
        | Unix.WSTOPPED i -> Printf.sprintf "Stopped by signal %d" i
      in
      error "VDA.%s exited with '%s'. Log: '%s'" command ps_kind log;
      let msg = Printf.sprintf "VDA.%s exited with '%s'" command ps_kind in
      raise Api_errors.(Server_error(internal_error, [msg]))
    | err ->
      let msg = Printf.sprintf "VDA.%s failed with '%s'" command (Printexc.to_string err) in
      error "%s" msg;
      raise Api_errors.(Server_error(internal_error, [msg]))
  in
  try
    parse_response output
  with
  | ResponseError ->
    (* This is already logged by parse_response *)
    raise Api_errors.(Server_error(vda_response_error, [Ref.string_of vda; output]))
  | err ->
    let msg = Printf.sprintf "VDA.%s failed with %s" command (Printexc.to_string err) in
    error "%s" msg;
    raise Api_errors.(Server_error(internal_error, [msg]))

let get_console ~__context ~vm =
  let open Xenstore in
  let domid = Db.VM.get_domid ~__context ~self:vm |> Int64.to_string in
  try
    with_xs (fun xs ->
      xs.read (Printf.sprintf "/local/domain/%s/console/tty" domid))
  with err ->
    let msg = Printf.sprintf "Unable to find tty for VM with domid %s" domid in
    error "%s" msg;
    raise Api_errors.(Server_error(internal_error, [msg]))

let get_status ~__context ~self =
  assert_vm_power_state_is ~__context ~self ~expected:`Running;
  let vm = Db.VDA.get_vm ~__context ~self in
  let console = get_console ~__context ~vm in
  debug "Calling VDA.get_status on console '%s' for VM '%s'" console (Ref.string_of vm);
  call_guestcommands self console "get_status"

let get_log_report ~__context ~self =
  assert_vm_power_state_is ~__context ~self ~expected:`Running;
  let vm = Db.VDA.get_vm ~__context ~self in
  let console = get_console ~__context ~vm in
  debug "Calling VDA.get_log_report on console '%s' for VM '%s'" console (Ref.string_of vm);
  call_guestcommands self console "get_log_report"
