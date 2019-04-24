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
(* String-based interface to the API *)

open Stdext
open Pervasiveext
open Client
open Db_cache (* eek! *)
open Xstringext
open Threadext

let nullref = Ref.string_of (Ref.null)
let nid = "<not in database>"
let unknown_time = "<unknown time>"

let string_of_float f = Printf.sprintf "%.3f" f

(* Splitting an empty string gives a list containing the empty string, which
 * is usually not what we want. *)
let get_words separator = function
  | "" -> []
  | str -> String.split separator str

type field  = { name: string;
                get: (unit -> string);
                set: (string -> unit) option;
                get_set : (unit -> string list) option; (* gets the string list that is a representation of a set *)
                add_to_set: (string -> unit) option;
                remove_from_set: (string -> unit) option;
                get_map : (unit -> (string * string) list) option;
                add_to_map: (string -> string -> unit) option;
                remove_from_map: (string -> unit) option;
                set_in_map: (string -> string -> unit) option; (* Change the value of an existing map field, without using add/remove *)
                set_map: ((string * string) list -> unit) option; (* Set the (key, value) pairs to an existing map field *)
                expensive: bool; (* Simply means an extra API call is required to get it *)
                hidden: bool; (* Meaning we don't show it unless it's *explicitly* asked for (i.e. hidden from *-list and *-param-list *)
                deprecated: bool;
                case_insensitive: bool; (* Use case-insensitive matching when selecting *)
              }

type ('a,'b) record = { getref : unit -> 'a Ref.t;
                        record : unit -> 'b;
                        setref : 'a Ref.t -> unit;
                        setrefrec : 'a Ref.t * 'b -> unit;
                        fields : field list; }

let make_field ?add_to_set ?remove_from_set ?add_to_map ?remove_from_map ?set_in_map ?set_map ?set ?get_set ?get_map ?(expensive=false) ?(hidden=false) ?(deprecated=false) ?(case_insensitive=false) ~name ~get () =
  { name; get; set; add_to_set; remove_from_set; add_to_map; remove_from_map; set_in_map; set_map; get_set; get_map; expensive; hidden; case_insensitive; deprecated }

let makeexpensivefield field =
  { field with get=(fun () -> "<expensive field>") }

let safe_i64_of_string field str =
  try Int64.of_string str with _ -> raise (Record_util.Record_failure ("Failed to parse parameter '"^field^"': expecting an integer"))

let safe_bool_of_string field str =
  try bool_of_string str with _ -> raise (Record_util.Record_failure ("Failed to parse parameter '"^field^"': expecting a boolean (true or false)"))

(* local lazy caches of objects *)
type 'a lzy = Got of 'a | ToGet of (unit -> 'a)

let lzy_get a =
  match !a with
  | Got x -> x
  | ToGet f -> let x = f () in a := Got x; x

(* End of cache code *)

exception CLI_failed_to_find_param of string
let field_lookup recs name = match List.filter (fun x -> x.name = name) recs with
  | [ x ] -> x
  | _ -> raise (CLI_failed_to_find_param name)

let safe_get_field x =
  try x.get ()
  with
  | Api_errors.Server_error(s,_) as e-> if s=Api_errors.handle_invalid then "<invalid reference>" else raise e
  | e -> raise e

let get_uuid_from_ref r =
  try
    match Ref_index.lookup (Ref.string_of r) with
    | None -> raise (CLI_failed_to_find_param "uuid")
    | Some x -> x.Ref_index.uuid
  with _ -> nid

let get_name_from_ref r =
  try
    match Ref_index.lookup (Ref.string_of r) with
    | None -> raise (CLI_failed_to_find_param "name")
    | Some x ->
      begin
        match x.Ref_index.name_label with
        | None -> raise (CLI_failed_to_find_param "name")
        | Some y -> y
      end
  with _ -> nid


(** If the given list is of length 1, get a ref for the PBD's host,
    otherwise return Ref.null *)
let get_pbds_host rpc session_id pbds =
  match pbds with
    [pbd] ->
    Client.PBD.get_host rpc session_id pbd
  | _ ->
    Ref.null

(** Get a ref for the single host to which the given SR is attached, or
    Ref.null if it's attached to 0 or >1 hosts. *)
let get_sr_host rpc session_id record =
  get_pbds_host rpc session_id (record.API.sR_PBDs)


let bond_record rpc session_id bond =
  let _ref = ref bond in
  let empty_record = ToGet (fun () -> Client.Bond.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record);
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields=
      [
        make_field ~name:"uuid"         ~get:(fun () -> (x ()).API.bond_uuid) ();
        make_field ~name:"master"       ~get:(fun () -> get_uuid_from_ref (x ()).API.bond_master) ();
        make_field ~name:"slaves"       ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.bond_slaves)) ();
        make_field ~name:"mode" ~get:(fun () -> Record_util.bond_mode_to_string (x ()).API.bond_mode) ();
        make_field ~name:"properties"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.bond_properties)
          ~get_map:(fun () -> (x ()).API.bond_properties)
          ~set_in_map:(fun k v -> Client.Bond.set_property rpc session_id bond k v) ();
        make_field ~name:"primary-slave" ~get:(fun () -> get_uuid_from_ref (x ()).API.bond_primary_slave) ();
        make_field ~name:"links-up" ~get:(fun () -> Int64.to_string (x ()).API.bond_links_up) ();
      ]
  }

let vlan_record rpc session_id vlan =
  let _ref = ref vlan in
  let empty_record = ToGet (fun () -> Client.VLAN.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields=
      [
        make_field ~name:"uuid"         ~get:(fun () -> (x ()).API.vLAN_uuid) ();
        make_field ~name:"tagged-PIF"   ~get:(fun () -> get_uuid_from_ref (x ()).API.vLAN_tagged_PIF) ();
        make_field ~name:"untagged-PIF" ~get:(fun () -> get_uuid_from_ref (x ()).API.vLAN_untagged_PIF) ();
        make_field ~name:"tag"          ~get:(fun () -> Int64.to_string (x ()).API.vLAN_tag) ();
      ]
  }

let tunnel_record rpc session_id tunnel =
  let _ref = ref tunnel in
  let empty_record = ToGet (fun () -> Client.Tunnel.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields=
      [
        make_field ~name:"uuid"         ~get:(fun () -> (x ()).API.tunnel_uuid) ();
        make_field ~name:"access-PIF"   ~get:(fun () -> get_uuid_from_ref (x ()).API.tunnel_access_PIF) ();
        make_field ~name:"transport-PIF" ~get:(fun () -> get_uuid_from_ref (x ()).API.tunnel_transport_PIF) ();
        make_field ~name:"status"        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.tunnel_status) ();
        make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.tunnel_other_config)
          ~add_to_map:(fun k v -> Client.Tunnel.add_to_other_config rpc session_id tunnel k v)
          ~remove_from_map:(fun k -> Client.Tunnel.remove_from_other_config rpc session_id tunnel k)
          ~get_map:(fun () -> (x ()).API.tunnel_other_config) ();

      ]
  }

let message_record rpc session_id message =
  let _ref = ref message in
  let empty_record = ToGet (fun () -> Client.Message.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields=
      [
        make_field ~name:"uuid"         ~get:(fun () -> (x ()).API.message_uuid) ();
        make_field ~name:"name"         ~get:(fun () -> (x ()).API.message_name) ();
        make_field ~name:"priority"     ~get:(fun () -> Int64.to_string (x ()).API.message_priority) ();
        make_field ~name:"class"        ~get:(fun () -> Xapi_message.class_to_string (x ()).API.message_cls) ();
        make_field ~name:"obj-uuid"     ~get:(fun () -> (x ()).API.message_obj_uuid) ();
        make_field ~name:"timestamp"    ~get:(fun () -> Date.to_string (x ()).API.message_timestamp) ();
        make_field ~name:"body"         ~get:(fun () -> (x ()).API.message_body) ();
      ]
  }

let network_sriov_record rpc session_id network_sriov =
  let _ref = ref network_sriov in
  let empty_record = ToGet (fun () -> Client.Network_sriov.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"                     ~get:(fun () -> (x ()).API.network_sriov_uuid) ();
        make_field ~name:"physical-PIF"             ~get:(fun () -> get_uuid_from_ref (x ()).API.network_sriov_physical_PIF) ();
        make_field ~name:"logical-PIF"              ~get:(fun () -> get_uuid_from_ref (x ()).API.network_sriov_logical_PIF) ();
        make_field ~name:"requires-reboot"          ~get:(fun () -> string_of_bool (x ()).API.network_sriov_requires_reboot) ();
        make_field ~name:"remaining-capacity"       ~get:(fun () -> try Int64.to_string (Client.Network_sriov.get_remaining_capacity rpc session_id network_sriov) with _ -> "<unknown>") ~expensive:true ();
      ]}

let pif_record rpc session_id pif =
  let _ref = ref pif in
  let empty_record = ToGet (fun () -> Client.PIF.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  let metrics = ref (ToGet (fun () -> try Some (Client.PIF_metrics.get_record rpc session_id (x ()).API.pIF_metrics) with _ -> None)) in
  let xm () = lzy_get metrics in
  { setref=(fun r -> _ref := r; record := empty_record);
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields=
      [
        make_field ~name:"uuid"         ~get:(fun () -> (x ()).API.pIF_uuid) ();
        make_field ~name:"device"       ~get:(fun () -> (x ()).API.pIF_device) ();
        make_field ~name:"MAC"          ~get:(fun () -> (x ()).API.pIF_MAC)
          ~case_insensitive:true ();
        make_field ~name:"physical"     ~get:(fun () -> string_of_bool ((x ()).API.pIF_physical)) ();
        make_field ~name:"managed"      ~get:(fun () -> string_of_bool ((x ()).API.pIF_managed)) ();
        make_field ~name:"currently-attached"     ~get:(fun () -> string_of_bool ((x ()).API.pIF_currently_attached)) ();
        make_field ~name:"MTU"          ~get:(fun () -> (Int64.to_string (x ()).API.pIF_MTU)) ();
        make_field ~name:"VLAN"         ~get:(fun () -> (Int64.to_string (x ()).API.pIF_VLAN)) ();
        make_field ~name:"bond-master-of" ~get:(fun () -> String.concat "; " (List.map (fun pif -> get_uuid_from_ref pif) (x ()).API.pIF_bond_master_of)) ();
        make_field ~name:"bond-slave-of"  ~get:(fun () -> get_uuid_from_ref (x ()).API.pIF_bond_slave_of) ();
        make_field ~name:"sriov-physical-PIF-of" ~get:(fun () -> String.concat ";" (List.map get_uuid_from_ref (x ()).API.pIF_sriov_physical_PIF_of)) ();
        make_field ~name:"sriov-logical-PIF-of" ~get:(fun () -> String.concat ";" (List.map get_uuid_from_ref (x ()).API.pIF_sriov_logical_PIF_of)) ();
        make_field ~name:"tunnel-access-PIF-of" ~get:(fun () -> String.concat "; " (List.map (fun pif -> get_uuid_from_ref pif) (x ()).API.pIF_tunnel_access_PIF_of)) ();
        make_field ~name:"tunnel-transport-PIF-of"  ~get:(fun () -> String.concat "; " (List.map (fun pif -> get_uuid_from_ref pif) (x ()).API.pIF_tunnel_transport_PIF_of)) ();
        make_field ~name:"management"   ~get:(fun () -> string_of_bool ((x ()).API.pIF_management)) ();
        make_field ~name:"network-uuid" ~get:(fun () -> try get_uuid_from_ref (x ()).API.pIF_network with _ -> nid) ();
        make_field ~name:"network-name-label" ~get:(fun () -> try get_name_from_ref (x ()).API.pIF_network with _ -> nid) ();
        make_field ~name:"host-uuid"    ~get:(fun () -> try get_uuid_from_ref (x ()).API.pIF_host with _ -> nid) ();
        make_field ~name:"host-name-label"    ~get:(fun () -> try get_name_from_ref (x ()).API.pIF_host with _ -> nid) ();
        make_field ~name:"IP-configuration-mode" ~get:(fun () -> Record_util.ip_configuration_mode_to_string (x ()).API.pIF_ip_configuration_mode) ();
        make_field ~name:"IP"           ~get:(fun () -> (x ()).API.pIF_IP) ();
        make_field ~name:"netmask"      ~get:(fun () -> (x ()).API.pIF_netmask) ();
        make_field ~name:"gateway"      ~get:(fun () -> (x ()).API.pIF_gateway) ();
        make_field ~name:"IPv6-configuration-mode" ~get:(fun () -> Record_util.ipv6_configuration_mode_to_string (x ()).API.pIF_ipv6_configuration_mode) ();
        make_field ~name:"IPv6"           ~get:(fun () -> String.concat "; " (x ()).API.pIF_IPv6) ();
        make_field ~name:"IPv6-gateway"      ~get:(fun () -> (x ()).API.pIF_ipv6_gateway) ();
        make_field ~name:"primary-address-type" ~get:(fun () -> Record_util.primary_address_type_to_string (x ()).API.pIF_primary_address_type) ();
        make_field ~name:"DNS"          ~get:(fun () -> (x ()).API.pIF_DNS) ();
        make_field ~name:"properties"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pIF_properties)
          ~get_map:(fun () -> (x ()).API.pIF_properties)
          ~set_in_map:(fun k v -> Client.PIF.set_property rpc session_id pif k v) ();
        make_field ~name:"capabilities" ~get:(fun () -> String.concat "; " (x ()).API.pIF_capabilities)
          ~get_set:(fun () -> (x ()).API.pIF_capabilities) ();
        make_field ~name:"io_read_kbs" ~get:(fun () ->
            try
              let host = (x ()).API.pIF_host in
              let name = Printf.sprintf "pif_%s_rx" (x ()).API.pIF_device in
              let value = Client.Host.query_data_source rpc session_id host name in
              string_of_float (value /. 1024.0)
            with _ -> "<unknown>") ~expensive:true ();
        make_field ~name:"io_write_kbs" ~get:(fun () ->
            try
              let host = (x ()).API.pIF_host in
              let name = Printf.sprintf "pif_%s_tx" (x ()).API.pIF_device in
              let value = Client.Host.query_data_source rpc session_id host name in
              string_of_float (value /. 1024.0)
            with _ -> "<unknown>") ~expensive:true ();
        make_field ~name:"carrier" ~get:(fun () -> default nid (may (fun m -> string_of_bool m.API.pIF_metrics_carrier) (xm ()))) ();
        make_field ~name:"vendor-id" ~get:(fun () -> default nid (may (fun m -> m.API.pIF_metrics_vendor_id) (xm ()))) ();
        make_field ~name:"vendor-name" ~get:(fun () -> default nid (may (fun m -> m.API.pIF_metrics_vendor_name) (xm ()))) ();
        make_field ~name:"device-id" ~get:(fun () -> default nid (may (fun m -> m.API.pIF_metrics_device_id) (xm ()))) ();
        make_field ~name:"device-name" ~get:(fun () -> default nid (may (fun m -> m.API.pIF_metrics_device_name) (xm ()))) ();
        make_field ~name:"speed" ~get:(fun () -> default nid (may (fun m -> (Int64.to_string m.API.pIF_metrics_speed) ^ " Mbit/s") (xm ()))) ();
        make_field ~name:"duplex" ~get:(fun () -> default nid (may (fun m ->
            if m.API.pIF_metrics_duplex then
              "full"
            else if m.API.pIF_metrics_carrier then
              "half"
            else
              "unknown"
          ) (xm ()))) ();
        make_field ~name:"disallow-unplug" ~get:(fun () -> string_of_bool ((x ()).API.pIF_disallow_unplug))
          ~set:(fun disallow_unplug -> Client.PIF.set_disallow_unplug rpc session_id pif (safe_bool_of_string "disallow-unplug" disallow_unplug)) ();
        make_field ~name:"pci-bus-path" ~get:(fun () -> default nid (may (fun m -> m.API.pIF_metrics_pci_bus_path) (xm ()))) ();
        make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pIF_other_config)
          ~add_to_map:(fun k v -> Client.PIF.add_to_other_config rpc session_id pif k v)
          ~remove_from_map:(fun k -> Client.PIF.remove_from_other_config rpc session_id pif k)
          ~get_map:(fun () -> (x ()).API.pIF_other_config) ();
        make_field ~name:"igmp-snooping-status" ~get:(fun () -> Record_util.pif_igmp_status_to_string ((x ()).API.pIF_igmp_snooping_status)) ();
      ] }

let task_record rpc session_id task =
  let _ref = ref task in
  let empty_record = ToGet (fun () -> Client.Task.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record);
    record=x;
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    getref=(fun () -> !_ref);
    fields = [
      make_field ~name:"uuid"         ~get:(fun () -> (x ()).API.task_uuid) ();
      make_field ~name:"name-label"   ~get:(fun () -> (x ()).API.task_name_label) ();
      make_field ~name:"name-description"  ~get:(fun () -> (x ()).API.task_name_description) ();
      make_field ~name:"subtask_of" ~get:(fun () -> get_uuid_from_ref (x ()).API.task_subtask_of) ();
      make_field ~name:"subtasks" ~get:(fun () -> String.concat ";" (List.map get_uuid_from_ref (x ()).API.task_subtasks)) ();
      make_field ~name:"resident-on"  ~get:(fun () -> get_uuid_from_ref (x ()).API.task_resident_on) ();
      make_field ~name:"status"       ~get:(fun () -> (Record_util.task_status_type_to_string (x ()).API.task_status)) ();
      make_field ~name:"progress"     ~get:(fun () -> (string_of_float (x ()).API.task_progress)) ();
      make_field ~name:"type"         ~get:(fun () -> (x ()).API.task_type) ();
      make_field ~name:"result"         ~get:(fun () -> (x ()).API.task_result) ();
      make_field ~name:"created"      ~get:(fun () -> Date.to_string (x ()).API.task_created) ();
      make_field ~name:"finished"    ~get:(fun () -> Date.to_string (x ()).API.task_finished) ();
      make_field ~name:"error_info"         ~get:(fun () -> String.concat "; " (x ()).API.task_error_info) ();
      make_field ~name:"allowed_operations" ~get:(fun () -> String.concat "; " (List.map Record_util.task_allowed_operations_to_string (x ()).API.task_allowed_operations)) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.task_other_config)
        ~add_to_map:(fun k v -> Client.Task.add_to_other_config rpc session_id task k v)
        ~remove_from_map:(fun k -> Client.Task.remove_from_other_config rpc session_id task k)
        ~get_map:(fun () -> (x ()).API.task_other_config) ();
    ]}


let vif_record rpc session_id vif =
  let _ref = ref vif in
  let empty_record = ToGet (fun () -> Client.VIF.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields=
      [
        make_field ~name:"uuid"         ~get:(fun () -> (x ()).API.vIF_uuid) ();
        make_field ~name:"vm-uuid"      ~get:(fun () -> get_uuid_from_ref (x ()).API.vIF_VM) ();
        make_field ~name:"vm-name-label"      ~get:(fun () -> get_name_from_ref (x ()).API.vIF_VM) ();
        make_field ~name:"allowed-operations"
          ~get:(fun () -> String.concat "; " (List.map Record_util.vif_operation_to_string (x ()).API.vIF_allowed_operations))
          ~get_set:(fun () -> List.map Record_util.vif_operation_to_string (x ()).API.vIF_allowed_operations) ();
        make_field ~name:"current-operations"
          ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.vif_operation_to_string b) (x ()).API.vIF_current_operations))
          ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.vif_operation_to_string b) (x ()).API.vIF_current_operations) ();
        make_field ~name:"device"       ~get:(fun () -> (x ()).API.vIF_device) ();
        make_field ~name:"MAC"          ~get:(fun () -> (x ()).API.vIF_MAC)
          ~case_insensitive:true ();
        make_field ~name:"MAC-autogenerated" ~get:(fun () -> string_of_bool (x ()).API.vIF_MAC_autogenerated) ();
        make_field ~name:"MTU"          ~get:(fun () -> Int64.to_string (x ()).API.vIF_MTU) ();
        make_field ~name:"currently-attached" ~get:(fun () -> string_of_bool (x ()).API.vIF_currently_attached) ();
        make_field ~name:"qos_algorithm_type" ~get:(fun () -> (x ()).API.vIF_qos_algorithm_type)
          ~set:(fun qat -> Client.VIF.set_qos_algorithm_type rpc session_id vif qat) ();
        make_field ~name:"qos_algorithm_params" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vIF_qos_algorithm_params)
          ~add_to_map:(fun k v -> Client.VIF.add_to_qos_algorithm_params rpc session_id vif k v)
          ~remove_from_map:(fun k -> Client.VIF.remove_from_qos_algorithm_params rpc session_id vif k)
          ~get_map:(fun () -> (x ()).API.vIF_qos_algorithm_params) ();
        make_field ~name:"qos_supported_algorithms" ~get:(fun () -> String.concat "; " (x ()).API.vIF_qos_supported_algorithms)
          ~get_set:(fun () -> (x ()).API.vIF_qos_supported_algorithms) ();
        make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vIF_other_config)
          ~add_to_map:(fun k v -> Client.VIF.add_to_other_config rpc session_id vif k v)
          ~remove_from_map:(fun k -> Client.VIF.remove_from_other_config rpc session_id vif k)
          ~get_map:(fun () -> (x ()).API.vIF_other_config) ();
        make_field ~name:"network-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.vIF_network) ();
        make_field ~name:"network-name-label" ~get:(fun () -> get_name_from_ref (x ()).API.vIF_network) ();
        make_field ~name:"io_read_kbs" ~get:(fun () ->
            try
              let name = Printf.sprintf "vif_%s_rx" (x ()).API.vIF_device in
              string_of_float ((Client.VM.query_data_source rpc session_id (x ()).API.vIF_VM name) /. 1024.0)
            with _ -> "<unknown>") ~expensive:true ();
        make_field ~name:"io_write_kbs" ~get:(fun () ->
            try
              let name = Printf.sprintf "vif_%s_tx" (x ()).API.vIF_device in
              string_of_float ((Client.VM.query_data_source rpc session_id (x ()).API.vIF_VM name) /. 1024.0)
            with _ -> "<unknown>") ~expensive:true ();
        make_field ~name:"locking-mode"
          ~get:(fun () -> Record_util.vif_locking_mode_to_string (x ()).API.vIF_locking_mode)
          ~set:(fun value -> Client.VIF.set_locking_mode rpc session_id vif (Record_util.string_to_vif_locking_mode value)) ();
        make_field ~name:"ipv4-allowed"
          ~get:(fun () -> String.concat "; " (x ()).API.vIF_ipv4_allowed)
          ~get_set:(fun () -> (x ()).API.vIF_ipv4_allowed)
          ~add_to_set:(fun value -> Client.VIF.add_ipv4_allowed rpc session_id vif value)
          ~remove_from_set:(fun value -> Client.VIF.remove_ipv4_allowed rpc session_id vif value)
          ~set:(fun value -> Client.VIF.set_ipv4_allowed rpc session_id vif (String.split ',' value)) ();
        make_field ~name:"ipv6-allowed"
          ~get:(fun () -> String.concat "; " (x ()).API.vIF_ipv6_allowed)
          ~get_set:(fun () -> (x ()).API.vIF_ipv6_allowed)
          ~add_to_set:(fun value -> Client.VIF.add_ipv6_allowed rpc session_id vif value)
          ~remove_from_set:(fun value -> Client.VIF.remove_ipv6_allowed rpc session_id vif value)
          ~set:(fun value -> Client.VIF.set_ipv6_allowed rpc session_id vif (String.split ',' value)) ();
        make_field ~name:"ipv4-configuration-mode" ~get:(fun () -> Record_util.vif_ipv4_configuration_mode_to_string (x ()).API.vIF_ipv4_configuration_mode) ();
        make_field ~name:"ipv4-addresses" ~get:(fun () -> String.concat "; " (x ()).API.vIF_ipv4_addresses) ();
        make_field ~name:"ipv4-gateway" ~get:(fun () -> (x ()).API.vIF_ipv4_gateway) ();
        make_field ~name:"ipv6-configuration-mode" ~get:(fun () -> Record_util.vif_ipv6_configuration_mode_to_string (x ()).API.vIF_ipv6_configuration_mode) ();
        make_field ~name:"ipv6-addresses" ~get:(fun () -> String.concat "; " (x ()).API.vIF_ipv6_addresses) ();
        make_field ~name:"ipv6-gateway" ~get:(fun () -> (x ()).API.vIF_ipv6_gateway) ();
      ]}


let net_record rpc session_id net =
  let _ref = ref net in
  let empty_record = ToGet (fun () -> Client.Network.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.network_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.network_name_label)
        ~set:(fun x -> Client.Network.set_name_label rpc session_id net x) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.network_name_description)
        ~set:(fun x -> Client.Network.set_name_description rpc session_id net x) ();
      make_field ~name:"VIF-uuids" ~get:(fun () -> String.concat "; " (List.map (fun vif -> get_uuid_from_ref vif) (x ()).API.network_VIFs))
        ~get_set:(fun () -> (List.map (fun vif -> get_uuid_from_ref vif) (x ()).API.network_VIFs)) ();
      make_field ~name:"PIF-uuids" ~get:(fun () -> String.concat "; " (List.map (fun pif -> get_uuid_from_ref pif) (x ()).API.network_PIFs))
        ~get_set:(fun () -> (List.map (fun pif -> get_uuid_from_ref pif) (x ()).API.network_PIFs)) ();
      make_field ~name:"MTU" ~get:(fun () -> (Int64.to_string (x ()).API.network_MTU)) ~set:(fun x -> Client.Network.set_MTU rpc session_id net (Int64.of_string x)) ();
      make_field ~name:"bridge" ~get:(fun () -> (x ()).API.network_bridge) ();
      make_field ~name:"managed" ~get:(fun () -> string_of_bool (x ()).API.network_managed) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.network_other_config)
        ~add_to_map:(fun k v -> Client.Network.add_to_other_config rpc session_id net k v)
        ~remove_from_map:(fun k -> Client.Network.remove_from_other_config rpc session_id net k)
        ~get_map:(fun () -> (x ()).API.network_other_config) ();
      make_field ~name:"blobs" ~get:(fun () -> Record_util.s2brm_to_string get_uuid_from_ref "; " (x ()).API.network_blobs) ();
      make_field ~name:"tags"
        ~get:(fun () -> String.concat ", " (x ()).API.network_tags)
        ~get_set:(fun () -> (x ()).API.network_tags)
        ~add_to_set:(fun tag -> Client.Network.add_tags rpc session_id net tag)
        ~remove_from_set:(fun tag -> Client.Network.remove_tags rpc session_id net tag) ();
      make_field ~name:"default-locking-mode"
        ~get:(fun () -> Record_util.network_default_locking_mode_to_string (x ()).API.network_default_locking_mode)
        ~set:(fun value -> Client.Network.set_default_locking_mode rpc session_id net
                 (Record_util.string_to_network_default_locking_mode value)) ();

      make_field ~name:"purpose"
        ~get:(fun () -> (x ()).API.network_purpose |> List.map Record_util.network_purpose_to_string |> (String.concat ", "))
        ~get_set:(fun () -> (x ()).API.network_purpose |> List.map Record_util.network_purpose_to_string)
        ~add_to_set:(fun s -> Client.Network.add_purpose rpc session_id net
                        (Record_util.string_to_network_purpose s))
        ~remove_from_set:(fun s -> Client.Network.remove_purpose rpc session_id net
                             (Record_util.string_to_network_purpose s)) ();

    ]}


let pool_record rpc session_id pool =
  let _ref = ref pool in
  let empty_record = ToGet (fun () -> Client.Pool.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.pool_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.pool_name_label)
        ~set:(fun x -> Client.Pool.set_name_label rpc session_id pool x) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.pool_name_description)
        ~set:(fun x -> Client.Pool.set_name_description rpc session_id pool x) ();
      make_field ~name:"master"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.pool_master) ();
      make_field ~name:"default-SR"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.pool_default_SR)
        ~set:(fun x ->
            let sr_ref = if x="" then Ref.null else Client.SR.get_by_uuid rpc session_id x in
            Client.Pool.set_default_SR rpc session_id pool sr_ref) ();
      make_field ~name:"crash-dump-SR"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.pool_crash_dump_SR)
        ~set:(fun x ->
            let sr_ref = if x="" then Ref.null else Client.SR.get_by_uuid rpc session_id x in
            Client.Pool.set_crash_dump_SR rpc session_id pool sr_ref) ();
      make_field ~name:"suspend-image-SR"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.pool_suspend_image_SR)
        ~set:(fun x ->
            let sr_ref = if x="" then Ref.null else Client.SR.get_by_uuid rpc session_id x in
            Client.Pool.set_suspend_image_SR rpc session_id pool sr_ref) ();
      make_field ~name:"supported-sr-types" ~get:(fun () -> String.concat "; " (Client.SR.get_supported_types rpc session_id)) ~expensive:true ();
      make_field ~name:"other-config"     ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_other_config)
        ~add_to_map:(fun k v -> Client.Pool.add_to_other_config rpc session_id pool k v)
        ~remove_from_map:(fun k -> Client.Pool.remove_from_other_config rpc session_id pool k)
        ~get_map:(fun () -> (x ()).API.pool_other_config) ();
      make_field ~name:"allowed-operations"
        ~get:(fun () -> String.concat "; " (List.map Record_util.pool_operation_to_string (x ()).API.pool_allowed_operations))
        ~get_set:(fun () -> List.map Record_util.pool_operation_to_string (x ()).API.pool_allowed_operations) ();
      make_field ~name:"current-operations"
        ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.pool_operation_to_string b) (x ()).API.pool_current_operations))
        ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.pool_operation_to_string b) (x ()).API.pool_current_operations) ();
      make_field ~name:"ha-enabled" ~get:(fun () -> string_of_bool (x ()).API.pool_ha_enabled) ();
      make_field ~name:"ha-configuration"     ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_ha_configuration) ();
      make_field ~name:"ha-statefiles" ~get:(fun () -> String.concat "; " (List.map (fun x -> get_uuid_from_ref (Ref.of_string x)) (x ()).API.pool_ha_statefiles)) ();
      make_field ~name:"ha-host-failures-to-tolerate" ~get:(fun () -> Int64.to_string (x ()).API.pool_ha_host_failures_to_tolerate) ~set:(fun x -> Client.Pool.set_ha_host_failures_to_tolerate rpc session_id pool (Int64.of_string x)) ();
      make_field ~name:"ha-plan-exists-for" ~get:(fun () -> Int64.to_string (x ()).API.pool_ha_plan_exists_for) ();
      make_field ~name:"ha-allow-overcommit" ~get:(fun () -> string_of_bool (x ()).API.pool_ha_allow_overcommit) ~set:(fun x -> Client.Pool.set_ha_allow_overcommit rpc session_id pool (bool_of_string x)) ();
      make_field ~name:"ha-overcommitted" ~get:(fun () -> string_of_bool (x ()).API.pool_ha_overcommitted) ();
      make_field ~name:"blobs" ~get:(fun () -> Record_util.s2brm_to_string get_uuid_from_ref "; " (x ()).API.pool_blobs) ();
      make_field ~name:"wlb-url" ~get:(fun () -> (x ()).API.pool_wlb_url) ();
      make_field ~name:"wlb-username" ~get:(fun () -> (x ()).API.pool_wlb_username) ();
      make_field ~name:"wlb-enabled" ~get:(fun () -> string_of_bool (x ()).API.pool_wlb_enabled) ~set:(fun x -> Client.Pool.set_wlb_enabled rpc session_id pool (bool_of_string x)) ();
      make_field ~name:"wlb-verify-cert" ~get:(fun () -> string_of_bool (x ()).API.pool_wlb_verify_cert) ~set:(fun x -> Client.Pool.set_wlb_verify_cert rpc session_id pool (bool_of_string x)) ();
      make_field ~name:"igmp-snooping-enabled" ~get:(fun () -> string_of_bool (x ()).API.pool_igmp_snooping_enabled) ~set:(fun x -> Client.Pool.set_igmp_snooping_enabled rpc session_id pool (bool_of_string x)) ();
      make_field ~name:"gui-config"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_gui_config)
        ~add_to_map:(fun k v -> Client.Pool.add_to_gui_config rpc session_id pool k v)
        ~remove_from_map:(fun k -> Client.Pool.remove_from_gui_config rpc session_id pool k)
        ~get_map:(fun () -> (x ()).API.pool_gui_config) ();
      make_field ~name:"health-check-config"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_health_check_config)
        ~add_to_map:(fun k v -> Client.Pool.add_to_health_check_config rpc session_id pool k v)
        ~remove_from_map:(fun k -> Client.Pool.remove_from_health_check_config rpc session_id pool k)
        ~get_map:(fun () -> (x ()).API.pool_health_check_config) ();
      make_field ~name:"vswitch-controller" ~hidden:true ~get:(fun () -> let r = (x ()).API.pool_vswitch_controller in if r = "" then "<not set>" else r) ();
      make_field ~name:"restrictions" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_restrictions) ();
      make_field ~name:"tags"
        ~get:(fun () -> String.concat ", " (x ()).API.pool_tags)
        ~get_set:(fun () -> (x ()).API.pool_tags)
        ~add_to_set:(fun tag -> Client.Pool.add_tags rpc session_id pool tag)
        ~remove_from_set:(fun tag -> Client.Pool.remove_tags rpc session_id pool tag) ();
      make_field ~name:"license-state"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (Client.Pool.get_license_state rpc session_id pool)) ();
      make_field ~name:"ha-cluster-stack" ~get:(fun () -> (x ()).API.pool_ha_cluster_stack) ();
      make_field ~name:"guest-agent-config"
        ~get:(fun () ->
            Record_util.s2sm_to_string "; " (x ()).API.pool_guest_agent_config)
        ~add_to_map:(fun k v ->
            Client.Pool.add_to_guest_agent_config rpc session_id pool k v)
        ~remove_from_map:(fun k ->
            Client.Pool.remove_from_guest_agent_config rpc session_id pool k)
        ~get_map:(fun () -> (x ()).API.pool_guest_agent_config)
        ();
      make_field ~name:"cpu_info" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_cpu_info) ~get_map:(fun () -> (x ()).API.pool_cpu_info) ();
      make_field ~name:"policy-no-vendor-device" ~get:(fun () -> string_of_bool (x ()).API.pool_policy_no_vendor_device) ~set:(fun s -> Client.Pool.set_policy_no_vendor_device rpc session_id pool (safe_bool_of_string "policy-no-vendor-device" s)) ();
      make_field ~name:"live-patching-disabled"  ~get:(fun () -> string_of_bool (x ()).API.pool_live_patching_disabled) ~set:(fun s -> Client.Pool.set_live_patching_disabled rpc session_id pool (safe_bool_of_string "live-patching-disabled" s)) ();
    ]}

let vmss_record rpc session_id vmss =
  let _ref = ref vmss in
  let empty_record = ToGet (fun () -> Client.VMSS.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid" ~get:(fun () -> (x ()).API.vMSS_uuid) ();
        make_field ~name:"name-label"
          ~get:(fun () -> (x ()).API.vMSS_name_label)
          ~set:(fun x -> Client.VMSS.set_name_label rpc session_id vmss x) ();
        make_field ~name:"name-description"
          ~get:(fun () -> (x ()).API.vMSS_name_description)
          ~set:(fun x -> Client.VMSS.set_name_description rpc session_id vmss x) ();
        make_field ~name:"enabled"
          ~get:(fun () -> string_of_bool (x ()).API.vMSS_enabled)
          ~set:(fun x -> Client.VMSS.set_enabled rpc session_id vmss (safe_bool_of_string "enabled" x)) ();
        make_field ~name:"type"
          ~get:(fun () -> (Record_util.vmss_type_to_string (x ()).API.vMSS_type))
          ~set:(fun x -> Client.VMSS.set_type rpc session_id vmss (Record_util.string_to_vmss_type x)) ();
        make_field ~name:"retained-snapshots"
          ~get:(fun () -> string_of_int (Int64.to_int (x ()).API.vMSS_retained_snapshots))
          ~set:(fun x -> Client.VMSS.set_retained_snapshots rpc session_id vmss (safe_i64_of_string "retained-snapshots" x)) ();
        make_field ~name:"frequency"
          ~get:(fun () -> (Record_util.vmss_frequency_to_string (x ()).API.vMSS_frequency))
          ~set:(fun x -> Client.VMSS.set_frequency rpc session_id vmss (Record_util.string_to_vmss_frequency x)) ();
        make_field ~name:"schedule"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vMSS_schedule)
          ~get_map:(fun () -> (x ()).API.vMSS_schedule)
          ~add_to_map:(fun k v -> Client.VMSS.add_to_schedule rpc session_id vmss k v)
          ~remove_from_map:(fun k -> Client.VMSS.remove_from_schedule rpc session_id vmss k) ();
        make_field ~name:"last-run-time"
          ~get:(fun () -> Date.to_string (x ()).API.vMSS_last_run_time) ();
        make_field ~name:"VMs"
          ~get:(fun () -> String.concat "; "
                   (try
                      List.map
                        (fun self -> try Client.VM.get_uuid rpc session_id self with _ -> nid)
                        (Client.VMSS.get_VMs rpc session_id vmss)
                    with _ -> []
                   )
               )
          ~expensive:false
          ~get_set:(fun () -> try List.map
                                    (fun self -> try Client.VM.get_uuid rpc session_id self with _ -> nid)
                                    (Client.VMSS.get_VMs rpc session_id vmss) with _ -> [])
          ();
      ]}


let subject_record rpc session_id subject =
  let _ref = ref subject in
  let empty_record = ToGet (fun () -> Client.Subject.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"             ~get:(fun () -> (x ()).API.subject_uuid) ();
        make_field ~name:"subject-identifier"       ~get:(fun () -> (x ()).API.subject_subject_identifier) ();
        make_field ~name:"other-config"     ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.subject_other_config)
          ~get_map:(fun () -> (x ()).API.subject_other_config) ();
        make_field ~name:"roles"
          ~get:(fun () -> String.concat "; "
                   (try
                      List.map
                        (fun self -> try Client.Role.get_name_label rpc session_id self with _ -> nid)
                        (Client.Subject.get_roles rpc session_id subject)
                    with _ -> []
                   )
               )
          ~expensive:false
          ~get_set:(fun () -> try List.map
                                    (fun self -> try Client.Role.get_name_label rpc session_id self with _ -> nid)
                                    (Client.Subject.get_roles rpc session_id subject) with _ -> [])
          ();
      ]}

let role_record rpc session_id role =
  let _ref = ref role in
  let empty_record = ToGet (fun () -> Client.Role.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"             ~get:(fun () -> (x ()).API.role_uuid) ();
        make_field ~name:"name"             ~get:(fun () -> (x ()).API.role_name_label) ();
        make_field ~name:"description"             ~get:(fun () -> (x ()).API.role_name_description) ();
        (*make_field ~name:"subroles"
           ~get:(fun () -> String.concat "; "
             (try (Client.Role.get_permissions_name_label ~rpc ~session_id ~self:(!_ref)) with _ -> [])
            )
           ~expensive:true
           ~get_set:(fun () -> try (Client.Role.get_permissions_name_label ~rpc ~session_id ~self:(!_ref))
             with _ -> [])
          ();*)
        (*make_field ~name:"is_complete"             ~get:(fun () -> string_of_bool (x ()).API.role_is_complete) ();*)
        (*make_field ~name:"is_basic"             ~get:(fun () -> string_of_bool (x ()).API.role_is_basic) ();*)
      ]}

(*
let alert_record rpc session_id pool =
  let _ref = ref pool in
  let record = ref None in
  let x () = match !record with
    | Some x -> x
    | None ->
	let x = Client.Alert.get_record rpc session_id !_ref in
	record := Some x;
	x
  in
  { setref=(fun r -> _ref := r; record := None );
    setrefrec=(fun (a,b) -> _ref := a; record := Some b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
[
  make_field ~name:"uuid"    ~get:(fun () -> (x ()).API.alert_uuid) ();
  make_field ~name:"message" ~get:(fun () -> (x ()).API.alert_message) ();
  make_field ~name:"level"   ~get:(fun () -> Record_util.alert_level_to_string (x ()).API.alert_level) ();
  make_field ~name:"timestamp" ~get:(fun () -> Date.to_string (x ()).API.alert_timestamp) ();
  make_field ~name:"system"  ~get:(fun () -> string_of_bool (x ()).API.alert_system) ();
  make_field ~name:"task" ~get:(fun () -> get_uuid_from_ref (x ()).API.alert_task) ();
]}
*)


let console_record rpc session_id console =
  let _ref = ref console in
  let empty_record = ToGet (fun () -> Client.Console.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"                ~get:(fun () -> (x ()).API.console_uuid) ();
        make_field ~name:"vm-uuid"
          ~get:(fun () -> (get_uuid_from_ref (x ()).API.console_VM)) ();
        make_field ~name:"vm-name-label"
          ~get:(fun () -> (get_name_from_ref (x ()).API.console_VM)) ();
        make_field ~name:"protocol"            ~get:(fun () -> Record_util.protocol_to_string (x ()).API.console_protocol) ();
        make_field ~name:"location"            ~get:(fun () -> (x ()).API.console_location) ();
        make_field ~name:"other-config"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.console_other_config)
          ~add_to_map:(fun k v -> Client.Console.add_to_other_config rpc session_id console k v)
          ~remove_from_map:(fun k -> Client.Console.remove_from_other_config rpc session_id console k)
          ~get_map:(fun () -> (x ()).API.console_other_config) ();
      ]}

let vm_record rpc session_id vm =
  let _ref = ref vm in
  let empty_record = ToGet (fun () -> Client.VM.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  let empty_metrics = ToGet (fun () -> try Some (Client.VM_metrics.get_record rpc session_id (x ()).API.vM_metrics) with _ -> None) in
  let metrics = ref empty_metrics in
  let xm () = lzy_get metrics in
  let empty_guest_metrics = ToGet (fun () -> try Some (Client.VM_guest_metrics.get_record rpc session_id (x ()).API.vM_guest_metrics) with _ -> None) in
  let guest_metrics = ref empty_guest_metrics in
  let get_vcpus_utilisation () =
    let nvcpus = default 0 (may (fun m -> Int64.to_int m.API.vM_metrics_VCPUs_number) (xm ())) in
    let rec inner n =
      if n=nvcpus then [] else
        (string_of_int n,string_of_float (Client.VM.query_data_source rpc session_id !_ref (Printf.sprintf "cpu%d" n)))::(inner (n+1))
    in
    inner 0
  in
  let get_memory_target () =
    try
      Int64.to_string (
        try
          Int64.of_float (
            Client.VM.query_data_source
              rpc session_id !_ref "memory_target"
          )
        with Api_errors.Server_error (code, _)
          when code = Api_errors.vm_bad_power_state -> 0L
      )
    with _ -> "<unknown>"
  in
  let xgm () = lzy_get guest_metrics in
  {
    setref = (fun r -> _ref := r; record := empty_record );
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid"
        ~get:(fun () -> (x ()).API.vM_uuid) ();
      make_field ~name:"name-label"
        ~get:(fun () -> (x ()).API.vM_name_label)
        ~set:(fun x -> Client.VM.set_name_label rpc session_id vm x) ();
      make_field ~name:"name-description"
        ~get:(fun () -> (x ()).API.vM_name_description)
        ~set:(fun x -> Client.VM.set_name_description rpc session_id vm x) ();
      make_field ~name:"user-version"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_user_version)
        ~set:(fun x -> Client.VM.set_user_version rpc session_id vm (safe_i64_of_string "user-version" x)) ();
      make_field ~name:"is-a-template"
        ~get:(fun () -> string_of_bool (x ()).API.vM_is_a_template)
        ~set:(fun x -> Client.VM.set_is_a_template rpc session_id vm (safe_bool_of_string "is-a-template" x)) ();
      make_field ~name:"is-default-template"
        ~get:(fun () -> string_of_bool (x ()).API.vM_is_default_template)
        ~set:(fun x -> Client.VM.set_is_default_template rpc session_id vm (safe_bool_of_string "is-default-template" x)) ();
      make_field ~name:"is-a-snapshot"
        ~get:(fun () -> string_of_bool (x ()).API.vM_is_a_snapshot) ();
      make_field ~name:"snapshot-of"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vM_snapshot_of) ();
      make_field ~name:"snapshots"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vM_snapshots)) ();
      make_field ~name:"snapshot-time"
        ~get:(fun () -> Date.to_string (x ()).API.vM_snapshot_time) ();
      make_field ~name:"transportable-snapshot-id" ~hidden:true
        ~get:(fun () -> (x()).API.vM_transportable_snapshot_id) ();
      make_field ~name:"snapshot-info"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_snapshot_info) ();
      make_field ~name:"parent"
        ~get:(fun () -> get_uuid_from_ref (x()).API.vM_parent) ();
      make_field ~name:"children"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vM_children)) ();
      make_field ~name:"is-control-domain"
        ~get:(fun () -> string_of_bool (x ()).API.vM_is_control_domain) ();
      make_field ~name:"power-state"
        ~get:(fun () -> Record_util.power_to_string (x ()).API.vM_power_state) ();
      make_field ~name:"memory-actual"
        ~get:(fun () -> default nid (may (fun m -> Int64.to_string m.API.vM_metrics_memory_actual) (xm ()) )) ();
      make_field ~name:"memory-target" ~expensive:true
        ~get:(fun () -> get_memory_target ()) ();
      make_field ~name:"memory-overhead"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_memory_overhead) ();
      make_field ~name:"memory-static-max"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_memory_static_max)
        ~set:(fun x -> Client.VM.set_memory_static_max rpc session_id vm (Record_util.bytes_of_string "memory-static-max" x)) ();
      make_field ~name:"memory-dynamic-max"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_memory_dynamic_max)
        ~set:(fun x -> Client.VM.set_memory_dynamic_max rpc session_id vm (Record_util.bytes_of_string "memory-dynamic-max" x)) ();
      make_field ~name:"memory-dynamic-min"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_memory_dynamic_min)
        ~set:(fun x -> Client.VM.set_memory_dynamic_min rpc session_id vm (Record_util.bytes_of_string "memory-dynamic-min" x)) ();
      make_field ~name:"memory-static-min"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_memory_static_min)
        ~set:(fun x -> Client.VM.set_memory_static_min rpc session_id vm (Record_util.bytes_of_string "memory-static-min" x)) ();
      make_field ~name:"suspend-VDI-uuid"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vM_suspend_VDI)
        ~set:(fun x -> Client.VM.set_suspend_VDI rpc session_id vm (Client.VDI.get_by_uuid rpc session_id x)) ();
      make_field ~name:"suspend-SR-uuid"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vM_suspend_SR)
        ~set:(fun x -> Client.VM.set_suspend_SR rpc session_id vm (Client.SR.get_by_uuid rpc session_id x)) ();
      make_field ~name:"VCPUs-params"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_VCPUs_params)
        ~add_to_map:(fun k v -> match k with
            | "weight" | "cap" | "mask" -> Client.VM.add_to_VCPUs_params rpc session_id vm k v
            | _ -> raise (Record_util.Record_failure ("Failed to add parameter '"^k^"': expecting 'weight','cap' or 'mask'")))
        ~remove_from_map:(fun k -> Client.VM.remove_from_VCPUs_params rpc session_id vm k)
        ~get_map:(fun () -> (x ()).API.vM_VCPUs_params) ();
      make_field ~name:"VCPUs-max"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_VCPUs_max)
        ~set:(fun x -> Client.VM.set_VCPUs_max rpc session_id vm (safe_i64_of_string "VCPUs-max" x)) ();
      make_field ~name:"VCPUs-at-startup"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_VCPUs_at_startup)
        ~set:(fun x -> Client.VM.set_VCPUs_at_startup rpc session_id vm (safe_i64_of_string "VCPUs-at-startup" x)) ();
      make_field ~name:"actions-after-shutdown"
        ~get:(fun () -> Record_util.on_normal_exit_to_string (x ()).API.vM_actions_after_shutdown)
        ~set:(fun x -> Client.VM.set_actions_after_shutdown rpc session_id vm (Record_util.string_to_on_normal_exit x)) ();
      make_field ~name:"actions-after-reboot"
        ~get:(fun () -> Record_util.on_normal_exit_to_string (x ()).API.vM_actions_after_reboot)
        ~set:(fun x -> Client.VM.set_actions_after_reboot rpc session_id vm (Record_util.string_to_on_normal_exit x)) ();
      make_field ~name:"actions-after-crash"
        ~get:(fun () -> Record_util.on_crash_behaviour_to_string (x ()).API.vM_actions_after_crash)
        ~set:(fun x -> Client.VM.set_actions_after_crash rpc session_id vm (Record_util.string_to_on_crash_behaviour x)) ();
      make_field ~name:"console-uuids"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vM_consoles))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.vM_consoles) ();
      make_field ~name:"hvm"
        ~get:(fun () -> default "false" (may (fun m ->
            string_of_bool m.API.vM_metrics_hvm) (xm ()) )) ();
      make_field ~name:"nomigrate" ~hidden:true
        ~get:(fun () -> default "false" (may (fun m ->
            string_of_bool m.API.vM_metrics_nomigrate) (xm ()) )) ();
      make_field ~name:"nested-virt" ~hidden:true
        ~get:(fun () -> default "false" (may (fun m ->
            string_of_bool m.API.vM_metrics_nested_virt) (xm ()) )) ();
      make_field ~name:"platform"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_platform)
        ~add_to_map:(fun k v -> Client.VM.add_to_platform rpc session_id vm k v)
        ~remove_from_map:(fun k -> Client.VM.remove_from_platform rpc session_id vm k)
        ~get_map:(fun () -> (x ()).API.vM_platform) ();
      make_field ~name:"allowed-operations"
        ~get:(fun () -> String.concat "; " (List.map Record_util.vm_operation_to_string (x ()).API.vM_allowed_operations))
        ~get_set:(fun () -> List.map Record_util.vm_operation_to_string (x ()).API.vM_allowed_operations) ();
      make_field ~name:"current-operations"
        ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.vm_operation_to_string b) (x ()).API.vM_current_operations))
        ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.vm_operation_to_string b) (x ()).API.vM_current_operations) ();
      make_field ~name:"blocked-operations"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (List.map (fun (k, v) -> Record_util.vm_operation_to_string k, v) ((x ()).API.vM_blocked_operations)))
        ~add_to_map:(fun k v -> Client.VM.add_to_blocked_operations rpc session_id vm (Record_util.string_to_vm_operation k) v)
        ~remove_from_map:(fun k -> Client.VM.remove_from_blocked_operations rpc session_id vm (Record_util.string_to_vm_operation k))
        ~get_map:(fun () -> List.map (fun (k, v) -> Record_util.vm_operation_to_string k, v) ((x ()).API.vM_blocked_operations)) ();
      (* These two don't work on Dom-0 at the moment, so catch the exception *)
      make_field ~name:"allowed-VBD-devices"
        ~get:(fun () -> String.concat "; " (try Client.VM.get_allowed_VBD_devices rpc session_id vm with _ -> [])) ~expensive:true
        ~get_set:(fun () -> try Client.VM.get_allowed_VBD_devices rpc session_id vm with _ -> []) ();
      make_field ~name:"allowed-VIF-devices"
        ~get:(fun () -> String.concat "; " (try Client.VM.get_allowed_VIF_devices rpc session_id vm with _ -> [])) ~expensive:true
        ~get_set:(fun () -> try Client.VM.get_allowed_VIF_devices rpc session_id vm with _ -> []) ();
      make_field ~name:"possible-hosts"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (Client.VM.get_possible_hosts rpc session_id vm))) ~expensive:true ();
      make_field ~name:"domain-type"
        ~get:(fun () -> Record_util.domain_type_to_string (x ()).API.vM_domain_type)
        ~set:(fun x -> Client.VM.set_domain_type rpc session_id vm (Record_util.domain_type_of_string x)) ();
      make_field ~name:"current-domain-type"
        ~get:(fun () -> default nid (may (fun m -> Record_util.domain_type_to_string (m.API.vM_metrics_current_domain_type)) (xm ()) )) ();
      make_field ~name:"HVM-boot-policy"
        ~get:(fun () -> (x ()).API.vM_HVM_boot_policy)
        ~set:(fun x -> Client.VM.set_HVM_boot_policy rpc session_id vm x) ();
      make_field ~name:"HVM-boot-params"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_HVM_boot_params)
        ~add_to_map:(fun k v -> Client.VM.add_to_HVM_boot_params rpc session_id vm k v)
        ~remove_from_map:(fun k -> Client.VM.remove_from_HVM_boot_params rpc session_id vm k)
        ~get_map:(fun () -> (x ()).API.vM_HVM_boot_params) ();
      make_field ~name:"HVM-shadow-multiplier"
        ~get:(fun () -> string_of_float (x ()).API.vM_HVM_shadow_multiplier)
        ~set:(fun x -> Client.VM.set_HVM_shadow_multiplier rpc session_id vm (float_of_string x)) ();
      make_field ~name:"NVRAM" ~hidden:true
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_NVRAM)
        ~add_to_map:(fun k v -> Client.VM.add_to_NVRAM rpc session_id vm k v)
        ~remove_from_map:(fun k -> Client.VM.remove_from_NVRAM rpc session_id vm k)
        ~get_map:(fun () -> (x ()).API.vM_NVRAM) ();
      make_field ~name:"PV-kernel"
        ~get:(fun () -> (x ()).API.vM_PV_kernel)
        ~set:(fun x -> Client.VM.set_PV_kernel rpc session_id vm x) ();
      make_field ~name:"PV-ramdisk"
        ~get:(fun () -> (x ()).API.vM_PV_ramdisk)
        ~set:(fun x -> Client.VM.set_PV_ramdisk rpc session_id vm x) ();
      make_field ~name:"PV-args"
        ~get:(fun () -> (x ()).API.vM_PV_args)
        ~set:(fun x -> Client.VM.set_PV_args rpc session_id vm x) ();
      make_field ~name:"PV-legacy-args"
        ~get:(fun () -> (x ()).API.vM_PV_legacy_args)
        ~set:(fun x -> Client.VM.set_PV_legacy_args rpc session_id vm x) ();
      make_field ~name:"PV-bootloader"
        ~get:(fun () -> (x ()).API.vM_PV_bootloader)
        ~set:(fun x -> Client.VM.set_PV_bootloader rpc session_id vm x) ();
      make_field ~name:"PV-bootloader-args"
        ~get:(fun () -> (x ()).API.vM_PV_bootloader_args)
        ~set:(fun x -> Client.VM.set_PV_bootloader_args rpc session_id vm x) ();
      make_field ~name:"last-boot-CPU-flags"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_last_boot_CPU_flags) ();
      make_field ~name:"last-boot-record" ~expensive:true
        ~get:(fun () -> "'" ^ ((x ()).API.vM_last_booted_record) ^ "'") ();
      make_field ~name:"resident-on"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vM_resident_on) ();
      make_field ~name:"affinity"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vM_affinity)
        ~set:(fun x -> if x="" then Client.VM.set_affinity rpc session_id vm Ref.null else Client.VM.set_affinity rpc session_id vm (Client.Host.get_by_uuid rpc session_id x)) ();
      make_field ~name:"other-config"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_other_config)
        ~add_to_map:(fun k v -> Client.VM.add_to_other_config rpc session_id vm k v)
        ~remove_from_map:(fun k -> Client.VM.remove_from_other_config rpc session_id vm k)
        ~get_map:(fun () -> (x ()).API.vM_other_config) ();
      make_field ~name:"dom-id"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_domid) ();
      make_field ~name:"recommendations"
        ~get:(fun () -> (x ()).API.vM_recommendations) ();
      make_field ~name:"xenstore-data"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_xenstore_data)
        ~add_to_map:(fun k v -> Client.VM.add_to_xenstore_data rpc session_id vm k v)
        ~remove_from_map:(fun k -> Client.VM.remove_from_xenstore_data rpc session_id vm k)
        ~get_map:(fun () -> (x ()).API.vM_xenstore_data) ();
      make_field ~name:"ha-always-run" ~deprecated:true
        ~get:(fun () -> string_of_bool ((x ()).API.vM_ha_always_run))
        ~set:(fun x -> Client.VM.set_ha_always_run rpc session_id vm (bool_of_string x)) ();
      make_field ~name:"ha-restart-priority"
        ~get:(fun () -> (x ()).API.vM_ha_restart_priority)
        ~set:(fun x -> Client.VM.set_ha_restart_priority rpc session_id vm x) ();
      make_field ~name:"blobs"
        ~get:(fun () -> Record_util.s2brm_to_string get_uuid_from_ref "; " (x ()).API.vM_blobs) ();
      make_field ~name:"start-time"
        ~get:(fun () -> default unknown_time (may (fun m -> Date.to_string m.API.vM_metrics_start_time) (xm ()) )) ();
      make_field ~name:"install-time"
        ~get:(fun () -> default unknown_time (may (fun m -> Date.to_string m.API.vM_metrics_install_time) (xm ()) )) ();
      make_field ~name:"VCPUs-number"
        ~get:(fun () -> default nid (may (fun m -> Int64.to_string m.API.vM_metrics_VCPUs_number) (xm ()) )) ();
      make_field ~name:"VCPUs-utilisation"
        ~get:(fun () -> try let info = get_vcpus_utilisation () in String.concat "; " (List.map (fun (a,b) -> Printf.sprintf "%s: %s" a b) info) with _ -> "")
        ~get_map:(fun () -> try get_vcpus_utilisation () with _ -> []) ~expensive:true ();
      make_field ~name:"os-version"
        ~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_os_version) (xgm ())))
        ~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_os_version) (xgm ()))) ();
      make_field ~name:"PV-drivers-version"
        ~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_PV_drivers_version) (xgm ()) ))
        ~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_PV_drivers_version) (xgm ()))) ();
      make_field ~name:"PV-drivers-up-to-date"
        ~deprecated: true
        ~get:(fun () -> default nid (may (fun m -> string_of_bool m.API.vM_guest_metrics_PV_drivers_up_to_date) (xgm ()) )) ();
      make_field ~name:"memory"
        ~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_memory) (xgm ())))
        ~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_memory) (xgm ()))) ();
      make_field ~name:"disks"
        ~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_disks) (xgm ()) ))
        ~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_disks) (xgm ()))) ();
      make_field ~name:"VBDs"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vM_VBDs))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.vM_VBDs) ();
      make_field ~name:"networks"
        ~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_networks) (xgm ()) ))
        ~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_networks) (xgm ()))) ();
      make_field ~name:"PV-drivers-detected"
        ~get:(fun () -> default nid (may (fun m -> string_of_bool m.API.vM_guest_metrics_PV_drivers_detected) (xgm ()) )) ();
      make_field ~name:"other"
        ~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_other) (xgm ()) ))
        ~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_other) (xgm()))) ();
      make_field ~name:"live"
        ~get:(fun () -> default nid (may (fun m -> string_of_bool m.API.vM_guest_metrics_live) (xgm ()) )) ();
      make_field ~name:"guest-metrics-last-updated"
        ~get:(fun () -> default nid (may (fun m -> Date.to_string m.API.vM_guest_metrics_last_updated) (xgm ()) )) ();
      make_field ~name:"can-use-hotplug-vbd"
        ~get:(fun () -> default nid (may (fun m -> Record_util.tristate_to_string m.API.vM_guest_metrics_can_use_hotplug_vbd) (xgm ()) )) ();
      make_field ~name:"can-use-hotplug-vif"
        ~get:(fun () -> default nid (may (fun m -> Record_util.tristate_to_string m.API.vM_guest_metrics_can_use_hotplug_vif) (xgm ()) )) ();
      make_field ~name:"cooperative"
        (* NB this can receive VM_IS_SNAPSHOT *)
        ~get:(fun () -> string_of_bool (try Client.VM.get_cooperative rpc session_id vm with _ -> true))
        ~expensive:true ~deprecated:true ();
      make_field ~name:"tags"
        ~get:(fun () -> String.concat ", " (x ()).API.vM_tags)
        ~get_set:(fun () -> (x ()).API.vM_tags)
        ~add_to_set:(fun tag -> Client.VM.add_tags rpc session_id vm tag)
        ~remove_from_set:(fun tag -> Client.VM.remove_tags rpc session_id vm tag) ();
      make_field ~name:"appliance"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vM_appliance)
        ~set:(fun x -> if x="" then Client.VM.set_appliance rpc session_id vm Ref.null else Client.VM.set_appliance rpc session_id vm (Client.VM_appliance.get_by_uuid rpc session_id x)) ();
      make_field ~name:"snapshot-schedule"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vM_snapshot_schedule)
        ~set:(fun x -> if x="" then Client.VM.set_snapshot_schedule rpc session_id vm Ref.null else Client.VM.set_snapshot_schedule rpc session_id vm (Client.VMSS.get_by_uuid rpc session_id x)) ();
      make_field ~name:"is-vmss-snapshot"
        ~get:(fun () -> string_of_bool (x ()).API.vM_is_vmss_snapshot) ();
      make_field ~name:"start-delay"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_start_delay)
        ~set:(fun x -> Client.VM.set_start_delay rpc session_id vm (safe_i64_of_string "start-delay" x)) ();
      make_field ~name:"shutdown-delay"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_shutdown_delay)
        ~set:(fun x -> Client.VM.set_shutdown_delay rpc session_id vm (safe_i64_of_string "shutdown-delay" x)) ();
      make_field ~name:"order"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_order)
        ~set:(fun x -> Client.VM.set_order rpc session_id vm (safe_i64_of_string "order" x)) ();
      make_field ~name:"version"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_version) ();
      make_field ~name:"generation-id"
        ~get:(fun () -> (x ()).API.vM_generation_id) ();
      make_field ~name:"hardware-platform-version"
        ~get:(fun () -> Int64.to_string (x ()).API.vM_hardware_platform_version) ();
      make_field ~name:"has-vendor-device"
        ~get:(fun () -> string_of_bool (x ()).API.vM_has_vendor_device)
        ~set:(fun x -> Client.VM.set_has_vendor_device rpc session_id vm (safe_bool_of_string "has-vendor-device" x)) ();
      make_field ~name:"requires-reboot"
        ~get:(fun () -> string_of_bool (x ()).API.vM_requires_reboot) ();
      make_field ~name:"reference-label"
        ~get:(fun () -> (x ()).API.vM_reference_label) ();
      make_field ~name:"bios-strings"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vM_bios_strings)
        ~get_map:(fun () -> (x ()).API.vM_bios_strings)
        ~set_map:(fun x ->
          List.iter (fun (k, v) -> if not (List.mem k Xapi_globs.settable_vm_bios_string_keys) then
            raise (Record_util.Record_failure ("Unknown key '"^k^"': expecting " ^ (String.concat ", " Xapi_globs.settable_vm_bios_string_keys)))) x;
          Client.VM.set_bios_strings rpc session_id vm x) ();
    ]}

let host_crashdump_record rpc session_id host =
  let _ref = ref host in
  let empty_record = ToGet (fun () -> Client.Host_crashdump.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"                ~get:(fun () -> (x ()).API.host_crashdump_uuid) ();
        make_field ~name:"host-uuid"           ~get:(fun () -> get_uuid_from_ref (x ()).API.host_crashdump_host) ();
        make_field ~name:"timestamp"           ~get:(fun () -> Date.to_string (x ()).API.host_crashdump_timestamp) ();
        make_field ~name:"size"                ~get:(fun () -> Int64.to_string (x ()).API.host_crashdump_size) ();
      ]}

let pool_patch_record rpc session_id patch =
  let _ref = ref patch in
  let empty_record = ToGet (fun () -> Client.Pool_patch.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  let get_hosts () =
    let host_patch_refs = (x ()).API.pool_patch_host_patches in
    let host_refs = List.map (fun x -> Client.Host_patch.get_host ~rpc ~session_id ~self:x) host_patch_refs in
    let host_uuids = List.map (fun x -> Client.Host.get_uuid ~rpc ~session_id ~self:x) host_refs in
    host_uuids
  in
  let after_apply_guidance_to_string = function
    | `restartHVM -> "restartHVM"
    | `restartPV -> "restartPV"
    | `restartHost -> "restartHost"
    | `restartXAPI -> "restartXAPI"
  in
  let after_apply_guidance_to_string_set =
    List.map after_apply_guidance_to_string
  in
  let after_apply_guidance () =
    after_apply_guidance_to_string_set (x ()).API.pool_patch_after_apply_guidance
  in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"                ~get:(fun () -> (x ()).API.pool_patch_uuid) ();
        make_field ~name:"name-label"          ~get:(fun () -> (x ()).API.pool_patch_name_label) ();
        make_field ~name:"name-description"    ~get:(fun () -> (x ()).API.pool_patch_name_description) ();
        make_field ~name:"size"                ~get:(fun () -> Int64.to_string (x ()).API.pool_patch_size) ();
        make_field ~name:"hosts"               ~get:(fun () -> String.concat ", " (get_hosts ())) ~get_set:get_hosts ();
        make_field ~name:"after-apply-guidance" ~get:(fun () -> String.concat ", " (after_apply_guidance ())) ~get_set:after_apply_guidance ();
        make_field ~name:"update"              ~get:(fun () -> get_uuid_from_ref (x ()).API.pool_patch_pool_update) ();
      ]}

let pool_update_record rpc session_id update =
  let _ref = ref update in
  let empty_record = ToGet (fun () -> Client.Pool_update.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  let get_hosts () =
    let host_refs = (x ()).API.pool_update_hosts in
    let host_uuids = List.map (fun x -> Client.Host.get_uuid ~rpc ~session_id ~self:x) host_refs in
    host_uuids
  in
  let after_apply_guidance_to_string = function
    | `restartHVM -> "restartHVM"
    | `restartPV -> "restartPV"
    | `restartHost -> "restartHost"
    | `restartXAPI -> "restartXAPI"
  in
  let after_apply_guidance_to_string_set =
    List.map after_apply_guidance_to_string
  in
  let after_apply_guidance () =
    after_apply_guidance_to_string_set (x ()).API.pool_update_after_apply_guidance
  in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"                ~get:(fun () -> (x ()).API.pool_update_uuid) ();
        make_field ~name:"name-label"          ~get:(fun () -> (x ()).API.pool_update_name_label) ();
        make_field ~name:"name-description"    ~get:(fun () -> (x ()).API.pool_update_name_description) ();
        make_field ~name:"version"             ~get:(fun () -> (x ()).API.pool_update_version) ();
        make_field ~name:"installation-size"   ~get:(fun () -> Int64.to_string (x ()).API.pool_update_installation_size) ();
        make_field ~name:"hosts"               ~get:(fun () -> String.concat ", " (get_hosts ())) ~get_set:get_hosts ();
        make_field ~name:"after-apply-guidance" ~get:(fun () -> String.concat ", " (after_apply_guidance ())) ~get_set:after_apply_guidance ();
        make_field ~name:"enforce-homogeneity" ~get:(fun () -> string_of_bool (x ()).API.pool_update_enforce_homogeneity) ();
      ]}

let host_cpu_record rpc session_id host_cpu =
  let _ref = ref host_cpu in
  let empty_record = ToGet (fun () -> Client.Host_cpu.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"            ~get:(fun () -> (x ()).API.host_cpu_uuid) ();
        make_field ~name:"host-uuid"       ~get:(fun () -> get_uuid_from_ref (x ()).API.host_cpu_host) ();
        make_field ~name:"number"          ~get:(fun () -> Int64.to_string ((x ()).API.host_cpu_number)) ();
        make_field ~name:"vendor"          ~get:(fun () -> (x ()).API.host_cpu_vendor) ();
        make_field ~name:"speed"           ~get:(fun () -> Int64.to_string ((x ()).API.host_cpu_speed)) ();
        make_field ~name:"model"           ~get:(fun () -> Int64.to_string ((x ()).API.host_cpu_model)) ();
        make_field ~name:"family"          ~get:(fun () -> Int64.to_string ((x ()).API.host_cpu_family)) ();
        make_field ~name:"modelname"       ~get:(fun () -> (x ()).API.host_cpu_modelname) ();
        make_field ~name:"stepping"        ~get:(fun () -> (x ()).API.host_cpu_stepping) ();
        make_field ~name:"flags"           ~get:(fun () -> (x ()).API.host_cpu_flags) ();
        make_field ~name:"utilisation"     ~get:(fun () ->
            try
              string_of_float (Client.Host.query_data_source rpc session_id (x ()).API.host_cpu_host (Printf.sprintf "cpu%Ld" (x ()).API.host_cpu_number))
            with _ -> "<unknown>") ~expensive:true ();
      ]}

let host_record rpc session_id host =
  let _ref = ref host in
  let empty_record = ToGet (fun () -> Client.Host.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  let metrics = ref (ToGet (fun () -> try Some (Client.Host_metrics.get_record rpc session_id (x ()).API.host_metrics) with _ -> None)) in
  let xm () = lzy_get metrics in
  let get_patches () =
    let host_patch_refs = (x ()).API.host_patches in
    let patch_refs = List.map (fun x -> Client.Host_patch.get_pool_patch ~rpc ~session_id ~self:x) host_patch_refs in
    let patch_uuids = List.map (fun x -> Client.Pool_patch.get_uuid ~rpc ~session_id ~self:x) patch_refs in
    patch_uuids
  in
  let get_updates () =
    let pool_update_refs = (x ()).API.host_updates in
    let update_uuids = List.map (fun x -> Client.Pool_update.get_uuid ~rpc ~session_id ~self:x) pool_update_refs in
    update_uuids
  in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.host_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.host_name_label) ~set:(fun s -> Client.Host.set_name_label rpc session_id host s) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.host_name_description) ~set:(fun s -> Client.Host.set_name_description rpc session_id host s) ();
      make_field ~name:"allowed-operations"
        ~get:(fun () -> String.concat "; " (List.map Record_util.host_operation_to_string (x ()).API.host_allowed_operations))
        ~get_set:(fun () -> List.map Record_util.host_operation_to_string (x ()).API.host_allowed_operations) ();
      make_field ~name:"current-operations"
        ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.host_operation_to_string b) (x ()).API.host_current_operations))
        ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.host_operation_to_string b) (x ()).API.host_current_operations) ();
      make_field ~name:"enabled" ~get:(fun () -> string_of_bool (x ()).API.host_enabled) ();
      make_field ~name:"display" ~get:(fun () -> Record_util.host_display_to_string (x ()).API.host_display) ();
      make_field ~name:"API-version-major" ~get:(fun () -> Int64.to_string (x ()).API.host_API_version_major) ();
      make_field ~name:"API-version-minor" ~get:(fun () -> Int64.to_string (x ()).API.host_API_version_minor) ();
      make_field ~name:"API-version-vendor" ~get:(fun () -> (x ()).API.host_API_version_vendor) ();
      make_field ~name:"API-version-vendor-implementation"
        ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_API_version_vendor_implementation)
        ~get_map:(fun () -> (x ()).API.host_API_version_vendor_implementation) ();
      make_field ~name:"logging"						 ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_logging)
        ~add_to_map:(fun k v -> Client.Host.add_to_logging rpc session_id host k v)
        ~remove_from_map:(fun k -> Client.Host.remove_from_logging rpc session_id host k)
        ~get_map:(fun () -> (x ()).API.host_logging) ();
      make_field ~name:"suspend-image-sr-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.host_suspend_image_sr)
        ~set:(fun s -> Client.Host.set_suspend_image_sr rpc session_id host (if s="" then Ref.null else Client.SR.get_by_uuid rpc session_id s)) ();
      make_field ~name:"crash-dump-sr-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.host_crash_dump_sr)
        ~set:(fun s -> Client.Host.set_crash_dump_sr rpc session_id host (if s="" then Ref.null else Client.SR.get_by_uuid rpc session_id s)) ();
      make_field ~name:"software-version" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_software_version)
        ~get_map:(fun () -> (x ()).API.host_software_version) ();
      make_field ~name:"capabilities" ~get:(fun () -> String.concat "; " (x ()).API.host_capabilities)
        ~get_set:(fun () -> (x ()).API.host_capabilities) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_other_config)
        ~add_to_map:(fun k v -> Client.Host.add_to_other_config rpc session_id host k v)
        ~remove_from_map:(fun k -> Client.Host.remove_from_other_config rpc session_id host k)
        ~get_map:(fun () -> (x ()).API.host_other_config) ();
      make_field ~name:"cpu_info" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_cpu_info) ~get_map:(fun () -> (x ()).API.host_cpu_info) ();
      make_field ~name:"chipset-info" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_chipset_info) ~get_map:(fun () -> (x ()).API.host_chipset_info) ();
      make_field ~name:"hostname" ~get:(fun () -> (x ()).API.host_hostname) ();
      make_field ~name:"address" ~get:(fun () -> (x ()).API.host_address) ();
      make_field ~name:"supported-bootloaders" ~get:(fun () -> String.concat "; " (x ()).API.host_supported_bootloaders)
        ~get_set:(fun () -> (x ()).API.host_supported_bootloaders) ();
      make_field ~name:"blobs" ~get:(fun () -> Record_util.s2brm_to_string get_uuid_from_ref "; " (x ()).API.host_blobs) ();
      make_field ~name:"memory-overhead" ~get:(fun () -> Int64.to_string (x ()).API.host_memory_overhead) ();
      make_field ~name:"memory-total" ~get:(fun () -> default nid (may (fun m -> Int64.to_string m.API.host_metrics_memory_total) (xm ()) )) ();
      make_field ~name:"memory-free" ~get:(fun () -> default nid (may (fun m -> Int64.to_string m.API.host_metrics_memory_free) (xm ()) )) ();
      make_field ~name:"memory-free-computed" ~expensive:true ~get:(fun () -> Int64.to_string (Client.Host.compute_free_memory rpc session_id host)) ();
      make_field ~name:"host-metrics-live" ~get:(fun () -> default nid (may (fun m -> string_of_bool m.API.host_metrics_live) (xm ()) )) ();
      make_field ~name:"patches" ~deprecated:true ~get:(fun () -> String.concat ", " (get_patches ())) ~get_set:get_patches ();
      make_field ~name:"updates" ~get:(fun () -> String.concat ", " (get_updates ())) ~get_set:get_updates ();
      make_field ~name:"ha-statefiles" ~get:(fun () -> String.concat "; " (List.map (fun x -> get_uuid_from_ref (Ref.of_string x)) (x ()).API.host_ha_statefiles)) ();
      make_field ~name:"ha-network-peers" ~get:(fun () -> String.concat "; " (x ()).API.host_ha_network_peers) ();
      make_field ~name:"external-auth-type" ~get:(fun () -> (x ()).API.host_external_auth_type) ();
      make_field ~name:"external-auth-service-name" ~get:(fun () -> (x ()).API.host_external_auth_service_name) ();
      make_field ~name:"external-auth-configuration" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_external_auth_configuration)
        ~get_map:(fun () -> (x ()).API.host_external_auth_configuration) ();
      make_field ~name:"edition" ~get:(fun () -> (x ()).API.host_edition) ();
      make_field ~name:"license-server" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_license_server) ~get_map:(fun () -> (x ()).API.host_license_server) ();
      make_field ~name:"power-on-mode" ~get:(fun () -> (x ()).API.host_power_on_mode) ();
      make_field ~name:"power-on-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_power_on_config)
        ~get_map:(fun () -> (x ()).API.host_power_on_config) ();
      make_field ~name:"local-cache-sr" ~get:(fun () -> get_uuid_from_ref (x ()).API.host_local_cache_sr) ();
      make_field ~name:"tags"
        ~get:(fun () -> String.concat ", " (x ()).API.host_tags)
        ~get_set:(fun () -> (x ()).API.host_tags)
        ~add_to_set:(fun tag -> Client.Host.add_tags rpc session_id host tag)
        ~remove_from_set:(fun tag -> Client.Host.remove_tags rpc session_id host tag) ();
      make_field ~name:"ssl-legacy"
        ~get:(fun () -> string_of_bool (x ()).API.host_ssl_legacy)
        ~set:(fun s -> Client.Host.set_ssl_legacy rpc session_id host (safe_bool_of_string "ssl-legacy" s)) ();
      make_field ~name:"guest_VCPUs_params" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_guest_VCPUs_params)
        ~get_map:(fun () -> (x ()).API.host_guest_VCPUs_params)
        ~add_to_map:(fun k v -> Client.Host.add_to_guest_VCPUs_params rpc session_id host k v)
        ~remove_from_map:(fun k -> Client.Host.remove_from_guest_VCPUs_params rpc session_id host k) ();
      make_field ~name:"virtual-hardware-platform-versions"
        ~get:(fun () -> String.concat "; " (List.map Int64.to_string (x ()).API.host_virtual_hardware_platform_versions))
        ~get_set:(fun () -> List.map Int64.to_string (x ()).API.host_virtual_hardware_platform_versions) ();
      make_field ~name:"control-domain-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.host_control_domain) ();
      make_field ~name:"resident-vms"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.host_resident_VMs))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.host_resident_VMs) ();
      make_field ~name:"updates-requiring-reboot"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.host_updates_requiring_reboot))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.host_updates_requiring_reboot) ();
      make_field ~name:"features"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.host_features))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.host_features) ();
      make_field ~name:"iscsi_iqn"
        ~get:(fun () -> (x ()).API.host_iscsi_iqn) ~set:(fun s -> Client.Host.set_iscsi_iqn rpc session_id host s) ();
      make_field ~name:"multipathing"
        ~get:(fun () -> string_of_bool (x ()).API.host_multipathing) ~set:(fun s -> Client.Host.set_multipathing rpc session_id host (safe_bool_of_string "multipathing" s)) ();
    ]}

let vdi_record rpc session_id vdi =
  let _ref = ref vdi in
  let empty_record = ToGet (fun () -> Client.VDI.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.vDI_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.vDI_name_label)
        ~set:(fun label -> Client.VDI.set_name_label rpc session_id vdi label) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.vDI_name_description)
        ~set:(fun desc -> Client.VDI.set_name_description rpc session_id vdi desc) ();
      make_field ~name:"is-a-snapshot" ~get:(fun () -> string_of_bool (x ()).API.vDI_is_a_snapshot) ();
      make_field ~name:"snapshot-of" ~get:(fun () -> get_uuid_from_ref (x ()).API.vDI_snapshot_of) ();
      make_field ~name:"snapshots" ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vDI_snapshots)) ();
      make_field ~name:"snapshot-time" ~get:(fun () -> Date.to_string (x ()).API.vDI_snapshot_time) ();
      make_field ~name:"allowed-operations"
        ~get:(fun () -> String.concat "; " (List.map Record_util.vdi_operation_to_string (x ()).API.vDI_allowed_operations))
        ~get_set:(fun () -> List.map Record_util.vdi_operation_to_string (x ()).API.vDI_allowed_operations) ();
      make_field ~name:"current-operations"
        ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.vdi_operation_to_string b) (x ()).API.vDI_current_operations))
        ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.vdi_operation_to_string b) (x ()).API.vDI_current_operations) ();
      make_field ~name:"sr-uuid"
        ~get:(fun () -> get_uuid_from_ref (x ()).API.vDI_SR) ();
      make_field ~name:"sr-name-label"
        ~get:(fun () -> get_name_from_ref (x ()).API.vDI_SR) ();
      make_field ~name:"vbd-uuids"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vDI_VBDs))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.vDI_VBDs) ();
      make_field ~name:"crashdump-uuids"
        ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vDI_crash_dumps))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.vDI_crash_dumps) ();
      make_field ~name:"virtual-size" ~get:(fun () -> Int64.to_string (x ()).API.vDI_virtual_size) ();
      make_field ~name:"physical-utilisation" ~get:(fun () -> Int64.to_string (x ()).API.vDI_physical_utilisation) ();
      make_field ~name:"location" ~get:(fun () -> (x ()).API.vDI_location) ();
      make_field ~name:"type" ~get:(fun () -> Record_util.vdi_type_to_string (x ()).API.vDI_type) ();
      make_field ~name:"sharable" ~get:(fun () -> string_of_bool (x ()).API.vDI_sharable) ();
      make_field ~name:"read-only" ~get:(fun () -> string_of_bool (x ()).API.vDI_read_only) ();
      make_field ~name:"storage-lock" ~get:(fun () -> string_of_bool (x ()).API.vDI_storage_lock) ();
      make_field ~name:"managed" ~get:(fun () -> string_of_bool (x ()).API.vDI_managed) ();
      make_field ~name:"parent" ~deprecated:true ~get:(fun () -> get_uuid_from_ref (x ()).API.vDI_parent) ();
      make_field ~name:"missing" ~get:(fun () -> string_of_bool (x ()).API.vDI_missing) ();
      make_field ~name:"is-tools-iso" ~get:(fun () -> string_of_bool (x ()).API.vDI_is_tools_iso) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vDI_other_config)
        ~add_to_map:(fun k v -> Client.VDI.add_to_other_config rpc session_id vdi k v)
        ~remove_from_map:(fun k -> Client.VDI.remove_from_other_config rpc session_id vdi k)
        ~get_map:(fun () -> (x ()).API.vDI_other_config) ();
      make_field ~name:"xenstore-data" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vDI_xenstore_data)
        ~get_map:(fun () -> (x ()).API.vDI_xenstore_data) ();
      make_field ~name:"sm-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vDI_sm_config)
        ~get_map:(fun () -> (x ()).API.vDI_sm_config) ();
      make_field ~name:"on-boot" ~get:(fun () -> Record_util.on_boot_to_string (x ()).API.vDI_on_boot)
        ~set:(fun onboot -> Client.VDI.set_on_boot rpc session_id vdi (Record_util.string_to_vdi_onboot onboot)) ();
      make_field ~name:"allow-caching" ~get:(fun () -> string_of_bool (x ()).API.vDI_allow_caching)
        ~set:(fun b -> Client.VDI.set_allow_caching rpc session_id vdi (bool_of_string b)) ();
      make_field ~name:"metadata-latest" ~get:(fun () -> string_of_bool (x ()).API.vDI_metadata_latest) ();
      make_field ~name:"metadata-of-pool"
        ~get:(fun () ->
            let local_pool = List.hd (Client.Pool.get_all ~rpc ~session_id) in
            let vdi_pool = (x ()).API.vDI_metadata_of_pool in
            if local_pool = vdi_pool then
              get_uuid_from_ref local_pool
            else begin
              match Client.VDI.read_database_pool_uuid ~rpc ~session_id ~self:vdi with
              | "" -> nid
              | pool_uuid -> pool_uuid
            end) ();
      make_field ~name:"tags"
        ~get:(fun () -> String.concat ", " (x ()).API.vDI_tags)
        ~get_set:(fun () -> (x ()).API.vDI_tags)
        ~add_to_set:(fun tag -> Client.VDI.add_tags rpc session_id vdi tag)
        ~remove_from_set:(fun tag -> Client.VDI.remove_tags rpc session_id vdi tag) ();
      make_field ~name:"cbt-enabled" ~get:(fun () -> string_of_bool (x ()).API.vDI_cbt_enabled) ();
    ]}

let vbd_record rpc session_id vbd =
  let _ref = ref vbd in
  let empty_record = ToGet (fun () -> Client.VBD.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid" ~get:(fun () -> (x ()).API.vBD_uuid) ();
        make_field ~name:"vm-uuid"
          ~get:(fun () -> get_uuid_from_ref (x ()).API.vBD_VM) ();
        make_field ~name:"vm-name-label"
          ~get:(fun () -> get_name_from_ref (x ()).API.vBD_VM) ();
        make_field ~name:"vdi-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.vBD_VDI) ();
        make_field ~name:"vdi-name-label" ~get:(fun () -> if (x ()).API.vBD_empty then "<EMPTY>" else get_name_from_ref (x ()).API.vBD_VDI) ();
        make_field ~name:"allowed-operations"
          ~get:(fun () -> String.concat "; " (List.map Record_util.vbd_operation_to_string (x ()).API.vBD_allowed_operations))
          ~get_set:(fun () -> List.map Record_util.vbd_operation_to_string (x ()).API.vBD_allowed_operations) ();
        make_field ~name:"current-operations"
          ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.vbd_operation_to_string b) (x ()).API.vBD_current_operations))
          ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.vbd_operation_to_string b) (x ()).API.vBD_current_operations) ();
        make_field ~name:"empty" ~get:(fun () -> string_of_bool (x ()).API.vBD_empty) ();
        make_field ~name:"device" ~get:(fun () -> (x ()).API.vBD_device) ();
        make_field ~name:"userdevice" ~get:(fun () -> (x ()).API.vBD_userdevice)
          ~set:(fun dev -> Client.VBD.set_userdevice rpc session_id vbd dev) ();
        make_field ~name:"bootable" ~get:(fun () -> string_of_bool (x ()).API.vBD_bootable)
          ~set:(fun boot -> Client.VBD.set_bootable rpc session_id vbd (safe_bool_of_string "bootable" boot)) ();
        make_field ~name:"mode" ~get:(fun () -> match (x ()).API.vBD_mode with `RO -> "RO" | `RW -> "RW")
          ~set:(fun mode -> Client.VBD.set_mode rpc session_id vbd (Record_util.string_to_vbd_mode mode)) ();
        make_field ~name:"type" ~get:(fun () -> match (x ()).API.vBD_type with `CD -> "CD" | `Disk -> "Disk" | `Floppy -> "Floppy")
          ~set:(fun ty -> Client.VBD.set_type rpc session_id vbd (Record_util.string_to_vbd_type ty)) ();
        make_field ~name:"unpluggable" ~get:(fun () -> string_of_bool (x ()).API.vBD_unpluggable)
          ~set:(fun unpluggable -> Client.VBD.set_unpluggable rpc session_id vbd (safe_bool_of_string "unpluggable" unpluggable)) ();
        make_field ~name:"currently-attached" ~get:(fun () -> string_of_bool (x ()).API.vBD_currently_attached) ();
        make_field ~name:"attachable" ~get:(fun () -> try Client.VBD.assert_attachable rpc session_id vbd; "true" with e -> Printf.sprintf "false (error: %s)" (Printexc.to_string e)) ~expensive:true ();
        make_field ~name:"storage-lock" ~get:(fun () -> string_of_bool (x ()).API.vBD_storage_lock) ();
        make_field ~name:"status-code" ~get:(fun () -> Int64.to_string (x ()).API.vBD_status_code) ();
        make_field ~name:"status-detail" ~get:(fun () -> (x ()).API.vBD_status_detail) ();
        make_field ~name:"qos_algorithm_type" ~get:(fun () -> (x ()).API.vBD_qos_algorithm_type)
          ~set:(fun qat -> Client.VBD.set_qos_algorithm_type rpc session_id vbd qat) ();
        make_field ~name:"qos_algorithm_params" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vBD_qos_algorithm_params)
          ~get_map:(fun () -> (x ()).API.vBD_qos_algorithm_params)
          ~add_to_map:(fun k v -> Client.VBD.add_to_qos_algorithm_params rpc session_id vbd k v)
          ~remove_from_map:(fun k -> Client.VBD.remove_from_qos_algorithm_params rpc session_id vbd k) ();
        make_field ~name:"qos_supported_algorithms" ~get:(fun () -> String.concat "; " (x ()).API.vBD_qos_supported_algorithms)
          ~get_set:(fun () -> (x ()).API.vBD_qos_supported_algorithms) ();
        make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vBD_other_config)
          ~add_to_map:(fun k v -> Client.VBD.add_to_other_config rpc session_id vbd k v)
          ~remove_from_map:(fun k -> Client.VBD.remove_from_other_config rpc session_id vbd k)
          ~get_map:(fun () -> (x ()).API.vBD_other_config) ();
        make_field ~name:"io_read_kbs" ~get:(fun () ->
            try
              let name = Printf.sprintf "vbd_%s_read" (x ()).API.vBD_device in
              string_of_float ((Client.VM.query_data_source rpc session_id (x ()).API.vBD_VM name) /. 1024.0)
            with _ -> "<unknown>") ~expensive:true ();
        make_field ~name:"io_write_kbs" ~get:(fun () ->
            try
              let name = Printf.sprintf "vbd_%s_write" (x ()).API.vBD_device in
              string_of_float ((Client.VM.query_data_source rpc session_id (x ()).API.vBD_VM name) /. 1024.0)
            with _ -> "<unknown>") ~expensive:true ();
      ]}

let crashdump_record rpc session_id crashdump =
  let _ref = ref crashdump in
  let empty_record = ToGet (fun () -> Client.Crashdump.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid" ~get:(fun () -> (x ()).API.crashdump_uuid) ();
        make_field ~name:"vm-uuid"
          ~get:(fun () -> get_uuid_from_ref (x ()).API.crashdump_VM) ();
        make_field ~name:"vm-name-label"
          ~get:(fun () -> get_name_from_ref (x ()).API.crashdump_VM) ();
        make_field ~name:"vdi-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.crashdump_VDI) ();
      ]}

let sm_record rpc session_id sm =
  let _ref = ref sm in
  let empty_record = ToGet (fun () -> Client.SM.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  let s2i64_to_string m = List.map (fun (c,v) -> (c, Int64.to_string v)) m in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid" ~get:(fun () -> (x ()).API.sM_uuid) ();
        make_field ~name:"name-label" ~get:(fun () -> (x ()).API.sM_name_label) ();
        make_field ~name:"name-description" ~get:(fun () -> (x ()).API.sM_name_description) ();
        make_field ~name:"type" ~get:(fun () -> (x ()).API.sM_type) ();
        make_field ~name:"vendor" ~get:(fun () -> (x ()).API.sM_vendor) ();
        make_field ~name:"copyright" ~get:(fun () -> (x ()).API.sM_copyright) ();
        make_field ~name:"required-api-version" ~get:(fun () -> (x ()).API.sM_required_api_version) ();
        make_field ~name:"capabilities" ~deprecated:true ~get:(fun () -> String.concat "; " (x ()).API.sM_capabilities) ();
        make_field ~name:"features"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (s2i64_to_string (x ()).API.sM_features))
          ~get_map:(fun () -> s2i64_to_string (x ()).API.sM_features) ();
        make_field ~name:"configuration" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.sM_configuration) ();
        make_field ~name:"driver-filename" ~get:(fun () -> (x ()).API.sM_driver_filename) ();
        make_field ~name:"required-cluster-stack"
          ~get:(fun () -> String.concat ", " (x ()).API.sM_required_cluster_stack) ();
      ]}


let sr_record rpc session_id sr =
  let _ref = ref sr in
  let empty_record = ToGet (fun () -> Client.SR.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.sR_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.sR_name_label)
        ~set:(fun x -> Client.SR.set_name_label rpc session_id sr x) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.sR_name_description)
        ~set:(fun x -> Client.SR.set_name_description rpc session_id sr x) ();
      make_field ~name:"host"
        ~get:(fun () ->
            let sr_rec = x() in
            let pbds = sr_rec.API.sR_PBDs in
            if List.length pbds>1 then "<shared>"
            else get_name_from_ref (get_sr_host rpc session_id sr_rec)) ();
      make_field ~name:"allowed-operations"
        ~get:(fun () -> String.concat "; " (List.map Record_util.sr_operation_to_string (x ()).API.sR_allowed_operations))
        ~get_set:(fun () -> List.map Record_util.sr_operation_to_string (x ()).API.sR_allowed_operations) ();
      make_field ~name:"current-operations"
        ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.sr_operation_to_string b) (x ()).API.sR_current_operations))
        ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.sr_operation_to_string b) (x ()).API.sR_current_operations) ();
      make_field ~name:"VDIs" ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.sR_VDIs))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.sR_VDIs) ();
      make_field ~name:"PBDs" ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.sR_PBDs))
        ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.sR_PBDs) ();
      make_field ~name:"virtual-allocation" ~get:(fun () -> Int64.to_string (x ()).API.sR_virtual_allocation) ();
      make_field ~name:"physical-utilisation" ~get:(fun () -> Int64.to_string (x ()).API.sR_physical_utilisation) ();
      make_field ~name:"physical-size" ~get:(fun () -> Int64.to_string (x ()).API.sR_physical_size) ();
      make_field ~name:"type" ~get:(fun () -> (x ()).API.sR_type) ();
      make_field ~name:"content-type" ~get:(fun () -> (x ()).API.sR_content_type) ();
      make_field ~name:"shared"
        ~get:(fun () -> string_of_bool ((x ()).API.sR_shared))
        ~set:(fun x -> Client.SR.set_shared rpc session_id sr (safe_bool_of_string "shared" x)) ();
      make_field ~name:"introduced-by"
        ~get:(fun () -> (get_uuid_from_ref (x ()).API.sR_introduced_by)) ();
      make_field ~name:"is-tools-sr" ~get:(fun () -> string_of_bool (x ()).API.sR_is_tools_sr) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.sR_other_config)
        ~add_to_map:(fun k v -> Client.SR.add_to_other_config rpc session_id sr k v)
        ~remove_from_map:(fun k -> Client.SR.remove_from_other_config rpc session_id sr k)
        ~get_map:(fun () -> (x ()).API.sR_other_config) ();
      make_field ~name:"sm-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.sR_sm_config)
        ~get_map:(fun () -> (x ()).API.sR_sm_config) ();
      make_field ~name:"blobs" ~get:(fun () -> Record_util.s2brm_to_string get_uuid_from_ref "; " (x ()).API.sR_blobs) ();
      make_field ~name:"local-cache-enabled" ~get:(fun () -> string_of_bool (x ()).API.sR_local_cache_enabled) ();
      make_field ~name:"tags"
        ~get:(fun () -> String.concat ", " (x ()).API.sR_tags)
        ~get_set:(fun () -> (x ()).API.sR_tags)
        ~add_to_set:(fun tag -> Client.SR.add_tags rpc session_id sr tag)
        ~remove_from_set:(fun tag -> Client.SR.remove_tags rpc session_id sr tag) ();
      make_field ~name:"clustered"
        ~get:(fun () -> string_of_bool ((x ()).API.sR_clustered)) ();
    ]}

let pbd_record rpc session_id pbd =
  let _ref = ref pbd in
  let empty_record = ToGet (fun () -> Client.PBD.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record )
  ; setrefrec=(fun (a,b) -> _ref := a; record := Got b)
  ; record=x
  ; getref=(fun () -> !_ref)
  ; fields =
      [ make_field ~name:"uuid" ~get:(fun () -> (x ()).API.pBD_uuid) ()
      ; make_field ~name:"host" ~get:(fun () -> get_uuid_from_ref (x ()).API.pBD_host) ~deprecated:true ()
      ; make_field ~name:"host-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.pBD_host) ()
      ; make_field ~name:"host-name-label" ~get:(fun () -> get_name_from_ref (x ()).API.pBD_host) ()
      ; make_field ~name:"sr-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.pBD_SR) ()
      ; make_field ~name:"sr-name-label" ~get:(fun () -> get_name_from_ref (x ()).API.pBD_SR) ()
      ; make_field ~name:"device-config"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pBD_device_config)
          ~get_map:(fun () -> (x ()).API.pBD_device_config) ()
      ; make_field ~name:"currently-attached" ~get:(fun () -> string_of_bool (x ()).API.pBD_currently_attached) ()
      ; make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pBD_other_config)
          ~add_to_map:(fun k v -> Client.PBD.add_to_other_config rpc session_id pbd k v)
          ~remove_from_map:(fun k -> Client.PBD.remove_from_other_config rpc session_id pbd k)
          ~get_map:(fun () -> (x ()).API.pBD_other_config) ()
      ]
  }

let secret_record rpc session_id secret =
  let _ref = ref secret in
  let empty_record = ToGet (fun () ->
      Client.Secret.get_record ~rpc ~session_id ~self:!_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref = (fun r -> _ref := r; record := empty_record )
  ; setrefrec = (fun (a, b) -> _ref := a; record := Got b )
  ; record = x
  ; getref = (fun () -> !_ref )
  ; fields =
      [ make_field ~name:"uuid" ~get:(fun () -> (x ()).API.secret_uuid) ()
      ; make_field ~name:"value" ~get:(fun () -> (x ()).API.secret_value)
          ~set:(fun x ->
              Client.Secret.set_value ~rpc ~session_id ~self:!_ref ~value:x)
          ()
      ]
  }

let vm_appliance_record rpc session_id vm_appliance =
  let _ref = ref vm_appliance in
  let empty_record = ToGet (fun () ->
      Client.VM_appliance.get_record ~rpc ~session_id ~self:!_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record);
    setrefrec = (fun (a, b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid" ~get:(fun () -> (x ()).API.vM_appliance_uuid) ();
        make_field ~name:"name-label" ~get:(fun () -> (x ()).API.vM_appliance_name_label)
          ~set:(fun x -> Client.VM_appliance.set_name_label rpc session_id !_ref x) ();
        make_field ~name:"name-description" ~get:(fun () -> (x ()).API.vM_appliance_name_description)
          ~set:(fun x -> Client.VM_appliance.set_name_description rpc session_id !_ref x) ();
        make_field ~name:"VMs" ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.vM_appliance_VMs))
          ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.vM_appliance_VMs) ();
        make_field ~name:"allowed-operations"
          ~get:(fun () -> String.concat "; " (List.map Record_util.vm_appliance_operation_to_string (x ()).API.vM_appliance_allowed_operations))
          ~get_set:(fun () -> List.map Record_util.vm_appliance_operation_to_string (x ()).API.vM_appliance_allowed_operations) ();
        make_field ~name:"current-operations"
          ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.vm_appliance_operation_to_string b) (x ()).API.vM_appliance_current_operations))
          ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.vm_appliance_operation_to_string b) (x ()).API.vM_appliance_current_operations) ();
      ]
  }

let dr_task_record rpc session_id dr_task =
  let _ref = ref dr_task in
  let empty_record = ToGet (fun () ->
      Client.DR_task.get_record ~rpc ~session_id ~self:!_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record);
    setrefrec = (fun (a, b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid" ~get:(fun () -> (x ()).API.dR_task_uuid) ();
        make_field ~name:"introduced-SRs" ~get:(fun () -> String.concat "; " (List.map get_uuid_from_ref (x ()).API.dR_task_introduced_SRs)) ();
      ]
  }

let pgpu_record rpc session_id pgpu =
  let _ref = ref pgpu in
  let empty_record = ToGet (fun () -> Client.PGPU.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  let pci_record p = ref (ToGet (fun () -> Client.PCI.get_record rpc session_id p)) in
  let xp0 p = lzy_get (pci_record p) in
  let xp () = xp0 (x ()).API.pGPU_PCI in
  {
    setref = (fun r -> _ref := r; record := empty_record );
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.pGPU_uuid) ();
      make_field ~name:"vendor-name" ~get:(fun () -> try (xp ()).API.pCI_vendor_name with _ -> nid) ();
      make_field ~name:"device-name" ~get:(fun () -> try (xp ()).API.pCI_device_name with _ -> nid) ();
      make_field ~name:"dom0-access" ~get:(fun () -> Record_util.pgpu_dom0_access_to_string (x ()).API.pGPU_dom0_access ) ();
      make_field ~name:"is-system-display-device" ~get:(fun () -> string_of_bool (x ()).API.pGPU_is_system_display_device ) ();
      make_field ~name:"gpu-group-uuid"
        ~get:(fun () -> try get_uuid_from_ref (x ()).API.pGPU_GPU_group with _ -> nid)
        ~set:(fun gpu_group_uuid ->
            let gpu_group = Client.GPU_group.get_by_uuid rpc session_id gpu_group_uuid in
            Client.PGPU.set_GPU_group rpc session_id pgpu gpu_group) ();
      make_field ~name:"gpu-group-name-label" ~get:(fun () -> try get_name_from_ref (x ()).API.pGPU_GPU_group with _ -> nid) ();
      make_field ~name:"host-uuid"    ~get:(fun () -> try get_uuid_from_ref (x ()).API.pGPU_host with _ -> nid) ();
      make_field ~name:"host-name-label"    ~get:(fun () -> try get_name_from_ref (x ()).API.pGPU_host with _ -> nid) ();
      make_field ~name:"pci-id" ~get:(fun () -> try (xp ()).API.pCI_pci_id with _ -> nid) ();
      make_field ~name:"dependencies" ~get:(fun () -> String.concat "; " (List.map (fun pci -> (xp0 pci).API.pCI_pci_id) (xp ()).API.pCI_dependencies))
        ~get_set:(fun () -> (List.map (fun pci -> (xp0 pci).API.pCI_pci_id) (xp ()).API.pCI_dependencies)) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pGPU_other_config)
        ~add_to_map:(fun k v -> Client.PGPU.add_to_other_config rpc session_id pgpu k v)
        ~remove_from_map:(fun k -> Client.PGPU.remove_from_other_config rpc session_id pgpu k)
        ~get_map:(fun () -> (x ()).API.pGPU_other_config) ();
      make_field ~name:"supported-VGPU-types"
        ~get:(fun () ->
            String.concat "; "
              (List.map
                 get_uuid_from_ref
                 (x ()).API.pGPU_supported_VGPU_types)) ();
      make_field ~name:"enabled-VGPU-types"
        ~get:(fun () ->
            String.concat "; "
              (List.map
                 get_uuid_from_ref
                 (x ()).API.pGPU_enabled_VGPU_types))
        ~get_set:(fun () ->
            (List.map
               (fun vgpu_type -> get_uuid_from_ref vgpu_type)
               (x ()).API.pGPU_enabled_VGPU_types))
        ~add_to_set:(fun vgpu_type_uuid ->
            Client.PGPU.add_enabled_VGPU_types rpc session_id pgpu
              (Client.VGPU_type.get_by_uuid rpc session_id vgpu_type_uuid))
        ~remove_from_set:(fun vgpu_type_uuid ->
            Client.PGPU.remove_enabled_VGPU_types rpc session_id pgpu
              (Client.VGPU_type.get_by_uuid rpc session_id vgpu_type_uuid))
        ~set:(fun vgpu_type_uuids ->
            Client.PGPU.set_enabled_VGPU_types rpc session_id pgpu
              (List.map
                 (fun vgpu_type_uuid ->
                    Client.VGPU_type.get_by_uuid rpc session_id vgpu_type_uuid)
                 (get_words ',' vgpu_type_uuids)))
        ();
      make_field ~name:"resident-VGPUs"
        ~get:(fun () ->
            String.concat "; "
              (List.map get_uuid_from_ref
                 (x ()).API.pGPU_resident_VGPUs)) ();
    ]
  }

let gpu_group_record rpc session_id gpu_group =
  let _ref = ref gpu_group in
  let empty_record = ToGet (fun () -> Client.GPU_group.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record );
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.gPU_group_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.gPU_group_name_label)
        ~set:(fun x -> Client.GPU_group.set_name_label rpc session_id gpu_group x) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.gPU_group_name_description)
        ~set:(fun x -> Client.GPU_group.set_name_description rpc session_id gpu_group x) ();
      make_field ~name:"VGPU-uuids" ~get:(fun () -> String.concat "; " (List.map (fun vgpu -> get_uuid_from_ref vgpu) (x ()).API.gPU_group_VGPUs))
        ~get_set:(fun () -> (List.map (fun vgpu -> get_uuid_from_ref vgpu) (x ()).API.gPU_group_VGPUs)) ();
      make_field ~name:"PGPU-uuids" ~get:(fun () -> String.concat "; " (List.map (fun pgpu -> get_uuid_from_ref pgpu) (x ()).API.gPU_group_PGPUs))
        ~get_set:(fun () -> (List.map (fun pgpu -> get_uuid_from_ref pgpu) (x ()).API.gPU_group_PGPUs)) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.gPU_group_other_config)
        ~add_to_map:(fun k v -> Client.GPU_group.add_to_other_config rpc session_id gpu_group k v)
        ~remove_from_map:(fun k -> Client.GPU_group.remove_from_other_config rpc session_id gpu_group k)
        ~get_map:(fun () -> (x ()).API.gPU_group_other_config) ();
      make_field ~name:"enabled-VGPU-types"
        ~get:(fun () ->
            String.concat "; "
              (List.map
                 get_uuid_from_ref
                 (Client.GPU_group.get_enabled_VGPU_types rpc session_id gpu_group)))
        ();
      make_field ~name:"supported-VGPU-types"
        ~get:(fun () ->
            String.concat "; "
              (List.map
                 get_uuid_from_ref
                 (Client.GPU_group.get_supported_VGPU_types rpc session_id gpu_group)))
        ();
      make_field ~name:"allocation-algorithm"
        ~get:(fun () ->
            Record_util.allocation_algorithm_to_string
              (x ()).API.gPU_group_allocation_algorithm)
        ~set:(fun ty ->
            Client.GPU_group.set_allocation_algorithm rpc session_id
              gpu_group (Record_util.allocation_algorithm_of_string ty)) ();
    ]
  }

let vgpu_record rpc session_id vgpu =
  let _ref = ref vgpu in
  let empty_record = ToGet (fun () -> Client.VGPU.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record );
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.vGPU_uuid) ();
      make_field ~name:"vm-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.vGPU_VM) ();
      make_field ~name:"vm-name-label" ~get:(fun () -> get_name_from_ref (x ()).API.vGPU_VM) ();
      make_field ~name:"device" ~get:(fun () -> (x ()).API.vGPU_device) ();
      make_field ~name:"gpu-group-uuid" ~get:(fun () -> try get_uuid_from_ref (x ()).API.vGPU_GPU_group with _ -> nid) ();
      make_field ~name:"gpu-group-name-label" ~get:(fun () -> try get_name_from_ref (x ()).API.vGPU_GPU_group with _ -> nid) ();
      make_field ~name:"currently-attached" ~get:(fun () -> string_of_bool (x ()).API.vGPU_currently_attached) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vGPU_other_config)
        ~add_to_map:(fun k v -> Client.VGPU.add_to_other_config rpc session_id vgpu k v)
        ~remove_from_map:(fun k -> Client.VGPU.remove_from_other_config rpc session_id vgpu k)
        ~get_map:(fun () -> (x ()).API.vGPU_other_config) ();
      make_field ~name:"type-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.vGPU_type) ();
      make_field ~name:"type-model-name" ~get:(fun () -> try Client.VGPU_type.get_model_name rpc session_id ((x ()).API.vGPU_type) with _ -> nid) ();
      make_field ~name:"resident-on" ~get:(fun () -> try get_uuid_from_ref (x ()).API.vGPU_resident_on with _ -> nid) ();
      make_field ~name:"compatibility-metadata"
        ~get:(fun () ->
          ((x ()).API.vGPU_compatibility_metadata)
          |> List.map (fun (k,v) -> Printf.sprintf
            "%s:(%d bytes)" k (String.length v))
          |> String.concat "; "
         ) ();
    ]
  }

let vgpu_type_record rpc session_id vgpu_type =
  let _ref = ref vgpu_type in
  let empty_record = ToGet (fun () -> Client.VGPU_type.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record);
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.vGPU_type_uuid) ();
      make_field ~name:"vendor-name" ~get:(fun () -> (x ()).API.vGPU_type_vendor_name) ();
      make_field ~name:"model-name"
        ~get:(fun () -> (x ()).API.vGPU_type_model_name) ();
      make_field ~name:"framebuffer-size"
        ~get:(fun () -> Int64.to_string (x ()).API.vGPU_type_framebuffer_size) ();
      make_field ~name:"max-heads"
        ~get:(fun () -> Int64.to_string (x ()).API.vGPU_type_max_heads) ();
      make_field ~name:"max-resolution"
        ~get:(fun () -> String.concat "x" (List.map Int64.to_string [(x ()).API.vGPU_type_max_resolution_x; (x ()).API.vGPU_type_max_resolution_y])) ();
      make_field ~name:"supported-on-PGPUs"
        ~get:(fun () -> String.concat "; " (List.map (fun p -> get_uuid_from_ref p) (x ()).API.vGPU_type_supported_on_PGPUs)) ();
      make_field ~name:"enabled-on-PGPUs"
        ~get:(fun () -> String.concat "; " (List.map (fun p -> get_uuid_from_ref p) (x ()).API.vGPU_type_enabled_on_PGPUs)) ();
      make_field ~name:"supported-on-GPU-groups"
        ~get:(fun () -> String.concat "; " (List.map (fun p -> get_uuid_from_ref p) (x ()).API.vGPU_type_supported_on_GPU_groups)) ();
      make_field ~name:"enabled-on-GPU-groups"
        ~get:(fun () -> String.concat "; " (List.map (fun p -> get_uuid_from_ref p) (x ()).API.vGPU_type_enabled_on_GPU_groups)) ();
      make_field ~name:"VGPU-uuids" ~get:(fun () -> String.concat "; " (List.map (fun v -> get_uuid_from_ref v) (x ()).API.vGPU_type_VGPUs)) ();
      make_field ~name:"experimental"
        ~get:(fun () -> string_of_bool (x ()).API.vGPU_type_experimental) ();
      make_field ~name:"compatible-types-in-vm"
        ~get:(fun () -> String.concat "; " (List.map (fun p -> get_uuid_from_ref p) (x ()).API.vGPU_type_compatible_types_in_vm)) ();
    ]
  }

let pvs_site_record rpc session_id pvs_site =
  let _ref = ref pvs_site in
  let empty_record =
    ToGet (fun () -> Client.PVS_site.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref    = (fun r -> _ref := r ; record := empty_record)
  ; setrefrec = (fun (a,b) -> _ref := a; record := Got b)
  ; record    = x
  ; getref    = (fun () -> !_ref)
  ; fields=
      [ make_field ~name:"uuid"
          ~get:(fun () -> (x ()).API.pVS_site_uuid) ()
      ; make_field ~name:"name-label"
          ~get:(fun () -> (x ()).API.pVS_site_name_label)
          ~set:(fun x ->
              Client.PVS_site.set_name_label rpc session_id !_ref x) ()
      ; make_field ~name:"name-description"
          ~get:(fun () -> (x ()).API.pVS_site_name_description)
          ~set:(fun x ->
              Client.PVS_site.set_name_description rpc session_id !_ref x) ()
      ; make_field ~name:"pvs-uuid"
          ~get:(fun () -> (x ()).API.pVS_site_PVS_uuid)
          ~set:(fun x ->
              Client.PVS_site.set_PVS_uuid rpc session_id !_ref x)
          ()
      ; make_field ~name:"pvs-cache-storage-uuids"
          ~get:(fun () -> (x ()).API.pVS_site_cache_storage
                          |> List.map get_uuid_from_ref |> String.concat "; ")
          ~get_set:(fun () ->
              List.map get_uuid_from_ref (x ()).API.pVS_site_cache_storage)
          ()
      ; make_field ~name:"pvs-server-uuids"
          ~get:(fun () -> (x ()).API.pVS_site_servers
                          |> List.map get_uuid_from_ref |> String.concat "; ")
          ~get_set:(fun () -> (x ()).API.pVS_site_servers
                              |> List.map get_uuid_from_ref)
          ()
      ; make_field ~name:"pvs-proxy-uuids"
          ~get:(fun () -> (x ()).API.pVS_site_proxies
                          |> List.map get_uuid_from_ref |> String.concat "; ")
          ~get_set:(fun () -> (x ()).API.pVS_site_proxies
                              |> List.map get_uuid_from_ref)
          ()
      ]
  }

let pvs_server_record rpc session_id pvs_site =
  let _ref = ref pvs_site in
  let empty_record =
    ToGet (fun () -> Client.PVS_server.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref    = (fun r -> _ref := r ; record := empty_record)
  ; setrefrec = (fun (a,b) -> _ref := a; record := Got b)
  ; record    = x
  ; getref    = (fun () -> !_ref)
  ; fields=
      [ make_field ~name:"uuid"
          ~get:(fun () -> (x ()).API.pVS_server_uuid)
          ()
      ; make_field ~name:"addresses"
          ~get:(fun () -> String.concat "; " (x ()).API.pVS_server_addresses)
          ~get_set:(fun () -> (x ()).API.pVS_server_addresses)
          ()
      ; make_field ~name:"first-port"
          ~get:(fun () -> (x ()).API.pVS_server_first_port |> Int64.to_string)
          ()
      ; make_field ~name:"last-port"
          ~get:(fun () -> (x ()).API.pVS_server_last_port |> Int64.to_string)
          ()
      ; make_field ~name:"pvs-site-uuid"
          ~get:(fun () -> (x ()).API.pVS_server_site |> get_uuid_from_ref)
          ()
      ]
  }

let pvs_proxy_record rpc session_id pvs_site =
  let _ref = ref pvs_site in
  let empty_record =
    ToGet (fun () -> Client.PVS_proxy.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref    = (fun r -> _ref := r ; record := empty_record)
  ; setrefrec = (fun (a,b) -> _ref := a; record := Got b)
  ; record    = x
  ; getref    = (fun () -> !_ref)
  ; fields=
      [ make_field ~name:"uuid"
          ~get:(fun () -> (x ()).API.pVS_proxy_uuid)
          ()
      ; make_field ~name:"pvs-site-uuid"
          ~get:(fun () -> (x ()).API.pVS_proxy_site |> get_uuid_from_ref)
          ()
      ; make_field ~name:"vif-uuid"
          ~get:(fun () -> (x ()).API.pVS_proxy_VIF |> get_uuid_from_ref)
          ()
      ; make_field ~name:"currently-attached"
          ~get:(fun () -> (x ()).API.pVS_proxy_currently_attached
                          |> string_of_bool)
          ()
      ; make_field ~name:"status"
          ~get:(fun () -> (x ()).API.pVS_proxy_status
                          |> Record_util.pvs_proxy_status_to_string)
          ()
      ]
  }

let pvs_cache_storage_record rpc session_id pvs_site =
  let _ref = ref pvs_site in
  let empty_record =
    ToGet (fun () -> Client.PVS_cache_storage.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref    = (fun r -> _ref := r ; record := empty_record)
  ; setrefrec = (fun (a,b) -> _ref := a; record := Got b)
  ; record    = x
  ; getref    = (fun () -> !_ref)
  ; fields=
      [ make_field ~name:"uuid"
          ~get:(fun () -> (x ()).API.pVS_cache_storage_uuid)
          ()
      ; make_field ~name:"host-uuid"
          ~get:(fun () -> (x ()).API.pVS_cache_storage_host |> get_uuid_from_ref)
          ()
      ; make_field ~name:"sr-uuid"
          ~get:(fun () -> (x ()).API.pVS_cache_storage_SR |> get_uuid_from_ref)
          ()
      ; make_field ~name:"pvs-site-uuid"
          ~get:(fun () -> (x ()).API.pVS_cache_storage_site |> get_uuid_from_ref)
          ()
      ; make_field ~name:"size"
          ~get:(fun () -> (x ()).API.pVS_cache_storage_size |> Int64.to_string)
          ()
      ; make_field ~name:"vdi-uuid"
          ~get:(fun () -> (x ()).API.pVS_cache_storage_VDI |> get_uuid_from_ref)
          ()
      ]
  }

let feature_record rpc session_id feature =
  let _ref = ref feature in
  let empty_record = ToGet (fun () -> Client.Feature.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record );
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.feature_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.feature_name_label) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.feature_name_description) ();
      make_field ~name:"enabled" ~get:(fun () -> (x ()).API.feature_enabled |> string_of_bool) ();
      make_field ~name:"experimental" ~get:(fun () -> (x ()).API.feature_experimental|> string_of_bool) ();
      make_field ~name:"version" ~get:(fun () -> (x ()).API.feature_version) ();
      make_field ~name:"host-uuid" ~get:(fun () -> (x ()).API.feature_host |> get_uuid_from_ref) ();
    ]
  }

let sdn_controller_record rpc session_id sdn_controller =
  let _ref = ref sdn_controller in
  let empty_record = ToGet (fun () -> Client.SDN_controller.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid"                ~get:(fun () -> (x ()).API.sDN_controller_uuid) ();
        make_field ~name:"protocol"            ~get:(fun () -> Record_util.sdn_protocol_to_string (x ()).API.sDN_controller_protocol) ();
        make_field ~name:"address"             ~get:(fun () -> (x ()).API.sDN_controller_address) ();
        make_field ~name:"port"                ~get:(fun () -> Int64.to_string (x ()).API.sDN_controller_port) ();
      ]}

let pusb_record rpc session_id pusb =
  let _ref = ref pusb in
  let empty_record = ToGet (fun () -> Client.PUSB.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
      [
        make_field ~name:"uuid" ~get:(fun () -> (x ()).API.pUSB_uuid) ();
        make_field ~name:"usb-group-uuid"
        ~get:(fun () -> try get_uuid_from_ref (x ()).API.pUSB_USB_group with _ -> nid) ();
        make_field ~name:"host-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.pUSB_host) ();
        make_field ~name:"host-name-label" ~get:(fun () -> try get_name_from_ref (x ()).API.pUSB_host with _ -> nid) ();
        make_field ~name:"path" ~get:(fun () -> (x ()).API.pUSB_path) ();
        make_field ~name:"vendor-id" ~get:(fun () -> (x ()).API.pUSB_vendor_id) ();
        make_field ~name:"vendor-desc" ~get:(fun () -> (x ()).API.pUSB_vendor_desc) ();
        make_field ~name:"product-id" ~get:(fun () -> (x ()).API.pUSB_product_id) ();
        make_field ~name:"product-desc" ~get:(fun () -> (x ()).API.pUSB_product_desc) ();
        make_field ~name:"serial" ~get:(fun () -> (x ()).API.pUSB_serial) ();
        make_field ~name:"version" ~get:(fun () -> (x ()).API.pUSB_version) ();
        make_field ~name:"description" ~get:(fun () -> (x ()).API.pUSB_description) ();
        make_field ~name:"passthrough-enabled" ~get:(fun () -> string_of_bool ((x ()).API.pUSB_passthrough_enabled))
          ~set:(fun passthrough_enabled -> Client.PUSB.set_passthrough_enabled rpc session_id pusb (safe_bool_of_string "passthrough-enabled" passthrough_enabled)) ();
      ]
  }

let usb_group_record rpc session_id usb_group =
  let _ref = ref usb_group in
  let empty_record = ToGet (fun () -> Client.USB_group.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record );
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.uSB_group_uuid) ();
      make_field ~name:"name-label" ~get:(fun () -> (x ()).API.uSB_group_name_label)
        ~set:(fun x -> Client.USB_group.set_name_label rpc session_id usb_group x) ();
      make_field ~name:"name-description" ~get:(fun () -> (x ()).API.uSB_group_name_description)
        ~set:(fun x -> Client.USB_group.set_name_description rpc session_id usb_group x) ();
      make_field ~name:"VUSB-uuids" ~get:(fun () -> String.concat "; " (List.map (fun vusb -> get_uuid_from_ref vusb) (x ()).API.uSB_group_VUSBs))
        ~get_set:(fun () -> (List.map (fun vusb -> get_uuid_from_ref vusb) (x ()).API.uSB_group_VUSBs)) ();
      make_field ~name:"PUSB-uuids" ~get:(fun () -> String.concat "; " (List.map (fun pusb -> get_uuid_from_ref pusb) (x ()).API.uSB_group_PUSBs))
        ~get_set:(fun () -> (List.map (fun pusb -> get_uuid_from_ref pusb) (x ()).API.uSB_group_PUSBs)) ();

      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.uSB_group_other_config)
        ~add_to_map:(fun k v -> Client.USB_group.add_to_other_config rpc session_id usb_group k v)
        ~remove_from_map:(fun k -> Client.USB_group.remove_from_other_config rpc session_id usb_group k)
        ~get_map:(fun () -> (x ()).API.uSB_group_other_config) ();
    ]
  }

let vusb_record rpc session_id vusb =
  let _ref = ref vusb in
  let empty_record = ToGet (fun () -> Client.VUSB.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  {
    setref = (fun r -> _ref := r; record := empty_record );
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record = x;
    getref = (fun () -> !_ref);
    fields = [
      make_field ~name:"uuid" ~get:(fun () -> (x ()).API.vUSB_uuid) ();
      make_field ~name:"vm-uuid" ~get:(fun () -> get_uuid_from_ref (x ()).API.vUSB_VM) ();
      make_field ~name:"vm-name-label" ~get:(fun () -> get_name_from_ref (x ()).API.vUSB_VM) ();
      make_field ~name:"usb-group-uuid" ~get:(fun () -> try get_uuid_from_ref (x ()).API.vUSB_USB_group with _ -> nid) ();
      make_field ~name:"usb-group-name-label" ~get:(fun () -> try get_name_from_ref (x ()).API.vUSB_USB_group with _ -> nid) ();
      make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vUSB_other_config)
        ~add_to_map:(fun k v -> Client.VUSB.add_to_other_config rpc session_id vusb k v)
        ~remove_from_map:(fun k -> Client.VUSB.remove_from_other_config rpc session_id vusb k)
        ~get_map:(fun () -> (x ()).API.vUSB_other_config) ();
      make_field ~name:"currently-attached" ~get:(fun () -> string_of_bool ((x ()).API.vUSB_currently_attached)) ();
      make_field ~name:"allowed-operations"
        ~get:(fun () -> String.concat "; " (List.map Record_util.vusb_operation_to_string (x ()).API.vUSB_allowed_operations))
        ~get_set:(fun () -> List.map Record_util.vusb_operation_to_string (x ()).API.vUSB_allowed_operations) ();
      make_field ~name:"current-operations"
        ~get:(fun () -> String.concat "; " (List.map (fun (a,b) -> Record_util.vusb_operation_to_string b) (x ()).API.vUSB_current_operations))
        ~get_set:(fun () -> List.map (fun (a,b) -> Record_util.vusb_operation_to_string b) (x ()).API.vUSB_current_operations) ();
    ]
  }

let cluster_record rpc session_id cluster =
  let _ref = ref cluster in
  let empty_record = ToGet (fun () -> Client.Cluster.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref    = (fun r -> _ref := r; record := empty_record);
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record    = x;
    getref    = (fun () -> !_ref);
    fields =
      [ make_field ~name:"uuid"
          ~get:(fun () -> (x ()).API.cluster_uuid)
          ()
      ; make_field ~name:"cluster-hosts"
          ~get:(fun () -> String.concat "; " (List.map (fun r -> get_uuid_from_ref r) (x ()).API.cluster_cluster_hosts))
          ~get_set:(fun () -> List.map get_uuid_from_ref (x ()).API.cluster_cluster_hosts)
          ()
      ; make_field ~name:"cluster-token"
          ~get:(fun () -> (x ()).API.cluster_cluster_token)
          ()
      ; make_field ~name:"cluster-stack"
          ~get:(fun () -> (x ()).API.cluster_cluster_stack)
          ()
      ; make_field ~name:"token-timeout"
          ~get:(fun () -> string_of_float ((x ()).API.cluster_token_timeout))
          ()
      ; make_field ~name:"token-timeout-coefficient"
          ~get:(fun () -> string_of_float ((x ()).API.cluster_token_timeout_coefficient))
          ()
      ; make_field ~name:"pending-forget" ~hidden:true
          ~get:(fun () -> String.concat "; " (x ()).API.cluster_pending_forget)
          ~get_set:(fun () -> (x ()).API.cluster_pending_forget)
          ()
      ; make_field ~name:"allowed-operations"
          ~get:(fun () -> String.concat "; " (List.map Record_util.cluster_operation_to_string (x ()).API.cluster_allowed_operations))
          ~get_set:(fun () -> List.map Record_util.cluster_operation_to_string (x ()).API.cluster_allowed_operations)
          ()
      ; make_field ~name:"current-operations"
          ~get:(fun () -> String.concat "; " (List.map (fun (task,op) -> Record_util.cluster_operation_to_string op) (x ()).API.cluster_current_operations))
          ~get_set:(fun () -> List.map (fun (task,op) -> Record_util.cluster_operation_to_string op) (x ()).API.cluster_current_operations)
          ()
      ; make_field ~name:"pool-auto-join"
          ~get:(fun () -> (x ()).API.cluster_pool_auto_join |> string_of_bool)
          ()
      ; make_field ~name:"cluster-config"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.cluster_cluster_config)
          ~get_map:(fun () -> (x ()).API.cluster_cluster_config)
          ()
      ; make_field ~name:"other-config"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.cluster_other_config)
          ~get_map:(fun () -> (x ()).API.cluster_other_config)
          ~add_to_map:(fun k v -> Client.Cluster.add_to_other_config rpc session_id cluster k v)
          ~remove_from_map:(fun k -> Client.Cluster.remove_from_other_config rpc session_id cluster k)
          ()
      ]}

let cluster_host_record rpc session_id cluster_host =
  let _ref = ref cluster_host in
  let empty_record = ToGet (fun () -> Client.Cluster_host.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref    = (fun r -> _ref := r; record := empty_record);
    setrefrec = (fun (a,b) -> _ref := a; record := Got b);
    record    = x;
    getref    = (fun () -> !_ref);
    fields =
      [ make_field ~name:"uuid"
          ~get:(fun () -> (x ()).API.cluster_host_uuid)
          ()
      ; make_field ~name:"cluster"
          ~get:(fun () -> (x ()).API.cluster_host_cluster |> get_uuid_from_ref)
          ()
      ; make_field ~name:"PIF"
          ~get:(fun () -> (x ()).API.cluster_host_PIF |> get_uuid_from_ref)
          ()
      ; make_field ~name:"host"
          ~get:(fun () -> (x ()).API.cluster_host_host |> get_uuid_from_ref)
          ()
      ; make_field ~name:"enabled"
          ~get:(fun () -> (x ()).API.cluster_host_enabled |> string_of_bool)
          ()
      ; make_field ~name:"joined"
          ~get:(fun () -> (x ()).API.cluster_host_joined |> string_of_bool)
          ()
      ; make_field ~name:"allowed-operations"
          ~get:(fun () -> String.concat "; " (List.map Record_util.cluster_host_operation_to_string (x ()).API.cluster_host_allowed_operations))
          ~get_set:(fun () -> List.map Record_util.cluster_host_operation_to_string (x ()).API.cluster_host_allowed_operations)
          ()
      ; make_field ~name:"current-operations"
          ~get:(fun () -> String.concat "; " (List.map (fun (task,op) -> Record_util.cluster_host_operation_to_string op) (x ()).API.cluster_host_current_operations))
          ~get_set:(fun () -> List.map (fun (task,op) -> Record_util.cluster_host_operation_to_string op) (x ()).API.cluster_host_current_operations)
          ()
      ; make_field ~name:"other-config"
          ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.cluster_host_other_config)
          ~get_map:(fun () -> (x ()).API.cluster_host_other_config)
          ()
      ]}
