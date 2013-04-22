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

open Pervasiveext
open Client
open Db_cache (* eek! *)
open Stringext
open Threadext

let nullref = Ref.string_of (Ref.null)
let nid = "<not in database>"
let unknown_time = "<unknown time>"

let checknull f r = 
  if (Ref.string_of r)=nullref then nid else
    try f r with _ -> nid



let getparam param params = try Some (List.assoc param params) with _ -> None

let string_of_float f = Printf.sprintf "%.3f" f

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
		
let make_field ?add_to_set ?remove_from_set ?add_to_map ?remove_from_map ?set_in_map ?set ?get_set ?get_map ?(expensive=false) ?(hidden=false) ?(deprecated=false) ?(case_insensitive=false) ~name ~get () = 
  { name = name; get = get; set = set; 
    add_to_set = add_to_set; remove_from_set = remove_from_set;
    add_to_map = add_to_map; remove_from_map = remove_from_map;
    set_in_map = set_in_map;
    get_set = get_set; get_map = get_map; expensive = expensive;
    hidden = hidden; case_insensitive = case_insensitive; 
    deprecated = deprecated
  }

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
	make_field ~name:"class"        ~get:(fun () -> match (x ()).API.message_cls with `VM -> "VM" | `Host -> "Host" | `SR -> "SR" | `Pool -> "Pool" | `VMPP -> "VMPP") ();
	make_field ~name:"obj-uuid"     ~get:(fun () -> (x ()).API.message_obj_uuid) ();
	make_field ~name:"timestamp"    ~get:(fun () -> Date.to_string (x ()).API.message_timestamp) ();
	make_field ~name:"body"         ~get:(fun () -> (x ()).API.message_body) ();
      ]
  }


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
	make_field ~name:"currently-attached"     ~get:(fun () -> string_of_bool ((x ()).API.pIF_currently_attached)) ();
	make_field ~name:"MTU"          ~get:(fun () -> (Int64.to_string (x ()).API.pIF_MTU)) ();
	make_field ~name:"VLAN"         ~get:(fun () -> (Int64.to_string (x ()).API.pIF_VLAN)) ();
	make_field ~name:"bond-master-of" ~get:(fun () -> String.concat "; " (List.map (fun pif -> get_uuid_from_ref pif) (x ()).API.pIF_bond_master_of)) ();
	make_field ~name:"bond-slave-of"  ~get:(fun () -> get_uuid_from_ref (x ()).API.pIF_bond_slave_of) ();
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
			make_field ~name:"ha-enabled" ~get:(fun () -> string_of_bool (x ()).API.pool_ha_enabled) ();
			make_field ~name:"ha-configuration"     ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_ha_configuration) ();
			make_field ~name:"ha-statefiles" ~get:(fun () -> String.concat "; " (List.map (fun x -> get_uuid_from_ref (Ref.of_string x)) (x ()).API.pool_ha_statefiles)) ();
			make_field ~name:"ha-host-failures-to-tolerate" ~get:(fun () -> Int64.to_string (x ()).API.pool_ha_host_failures_to_tolerate) ~set:(fun x -> Client.Pool.set_ha_host_failures_to_tolerate rpc session_id pool (Int64.of_string x)) ();
			make_field ~name:"ha-plan-exists-for" ~get:(fun () -> Int64.to_string (x ()).API.pool_ha_plan_exists_for) ();
			make_field ~name:"ha-allow-overcommit" ~get:(fun () -> string_of_bool (x ()).API.pool_ha_allow_overcommit) ~set:(fun x -> Client.Pool.set_ha_allow_overcommit rpc session_id pool (bool_of_string x)) ();
			make_field ~name:"ha-overcommitted" ~get:(fun () -> string_of_bool (x ()).API.pool_ha_overcommitted) ();
			make_field ~name:"blobs" ~get:(fun () -> Record_util.s2brm_to_string get_uuid_from_ref "; " (x ()).API.pool_blobs) ();
			make_field ~name:"gui-config"
				~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_gui_config)
				~add_to_map:(fun k v -> Client.Pool.add_to_gui_config rpc session_id pool k v)
				~remove_from_map:(fun k -> Client.Pool.remove_from_gui_config rpc session_id pool k)
				~get_map:(fun () -> (x ()).API.pool_gui_config)
				~expensive:true ();
			make_field ~name:"vswitch-controller" ~hidden:true ~get:(fun () -> let r = (x ()).API.pool_vswitch_controller in if r = "" then "<not set>" else r) ();
			make_field ~name:"restrictions" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.pool_restrictions) ();
			make_field ~name:"tags"
				~get:(fun () -> String.concat ", " (x ()).API.pool_tags)
				~get_set:(fun () -> (x ()).API.pool_tags)
				~add_to_set:(fun tag -> Client.Pool.add_tags rpc session_id pool tag)
				~remove_from_set:(fun tag -> Client.Pool.remove_tags rpc session_id pool tag) ();
			make_field ~name:"license-state"
				~get:(fun () -> Record_util.s2sm_to_string "; " (Client.Pool.get_license_state rpc session_id pool)) ();
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
				~get:(fun () -> default nid (may (fun m -> string_of_bool m.API.vM_guest_metrics_PV_drivers_up_to_date) (xgm ()) )) ();
			make_field ~name:"memory"
				~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_memory) (xgm ())))
				~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_memory) (xgm ()))) ();
			make_field ~name:"disks"
				~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_disks) (xgm ()) ))
				~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_disks) (xgm ()))) ();
			make_field ~name:"networks"
				~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_networks) (xgm ()) ))
				~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_networks) (xgm ()))) ();
			make_field ~name:"other"
				~get:(fun () -> default nid (may (fun m -> Record_util.s2sm_to_string "; " m.API.vM_guest_metrics_other) (xgm ()) ))
				~get_map:(fun () -> default [] (may (fun m -> m.API.vM_guest_metrics_other) (xgm()))) ();
			make_field ~name:"live"
				~get:(fun () -> default nid (may (fun m -> string_of_bool m.API.vM_guest_metrics_live) (xgm ()) )) ();
			make_field ~name:"guest-metrics-last-updated"
				~get:(fun () -> default nid (may (fun m -> Date.to_string m.API.vM_guest_metrics_last_updated) (xgm ()) )) ();
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
			make_field ~name:"patches" ~get:(fun () -> String.concat ", " (get_patches ())) ~get_set:get_patches ();
			make_field ~name:"ha-statefiles" ~get:(fun () -> String.concat "; " (List.map (fun x -> get_uuid_from_ref (Ref.of_string x)) (x ()).API.host_ha_statefiles)) ();
			make_field ~name:"ha-network-peers" ~get:(fun () -> String.concat "; " (List.map (fun x -> get_uuid_from_ref (Ref.of_string x)) (x ()).API.host_ha_network_peers)) ();
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
			make_field ~name:"guest_VCPUs_params" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.host_guest_VCPUs_params) 
				~get_map:(fun () -> (x ()).API.host_guest_VCPUs_params) 
				~add_to_map:(fun k v -> Client.Host.add_to_guest_VCPUs_params rpc session_id host k v)
				~remove_from_map:(fun k -> Client.Host.remove_from_guest_VCPUs_params rpc session_id host k) ();
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
			make_field ~name:"parent" ~get:(fun () -> get_uuid_from_ref (x ()).API.vDI_parent) ();
			make_field ~name:"missing" ~get:(fun () -> string_of_bool (x ()).API.vDI_missing) ();
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
    make_field ~name:"type" ~get:(fun () -> match (x ()).API.vBD_type with `CD -> "CD" | `Disk -> "Disk")
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

let session_record rpc session_id session =
  let _ref = ref session in
  let empty_record = ToGet (fun () -> Client.Session.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields = 
  [
    make_field ~name:"uuid" ~get:(fun () -> (x ()).API.session_uuid) ();
  ]}

let blob_record rpc session_id blob =
  let _ref = ref blob in
  let empty_record = ToGet (fun () -> Client.Blob.get_record rpc session_id !_ref) in
  let record = ref empty_record in
  let x () = lzy_get record in
  { setref=(fun r -> _ref := r; record := empty_record );
    setrefrec=(fun (a,b) -> _ref := a; record := Got b);
    record=x;
    getref=(fun () -> !_ref);
    fields =
  [
    make_field ~name:"uuid" ~get:(fun () -> (x ()).API.blob_uuid) ();
    make_field ~name:"name-label" ~get:(fun () -> (x ()).API.blob_name_label) 
      ~set:(fun x -> Client.Blob.set_name_label rpc session_id !_ref x) ();
    make_field ~name:"name-description" ~get:(fun () -> (x ()).API.blob_name_description)
      ~set:(fun x -> Client.Blob.set_name_description rpc session_id !_ref x) ();
    make_field ~name:"last_updated" ~get:(fun () -> Date.to_string (x ()).API.blob_last_updated) ();
    make_field ~name:"size" ~get:(fun () -> Int64.to_string (x ()).API.blob_size) ();
    make_field ~name:"mime-type" ~get:(fun () -> (x ()).API.blob_mime_type) ();
  ]}

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


(*let record_from_ref rpc session_id ref =
  let all = [
    "PBD",(fun ref -> snd (pbd_record rpc session_id (Ref.of_string ref))); 
    "SR",(fun ref -> snd (sr_record rpc session_id (Ref.of_string ref))); 
    "VBD",(fun ref -> snd (vbd_record rpc session_id (Ref.of_string ref))); 
    "VIF",(fun ref -> snd (vif_record rpc session_id (Ref.of_string ref))); 
    "PIF",(fun ref -> snd (pif_record rpc session_id (Ref.of_string ref))); 
    "VDI",(fun ref -> snd (vdi_record rpc session_id (Ref.of_string ref))); 
    "Host",(fun ref -> snd (host_record rpc session_id (Ref.of_string ref))); 
    "Network",(fun ref -> snd (net_record rpc session_id (Ref.of_string ref))); 
    "VM",(fun ref -> snd (vm_record rpc session_id (Ref.of_string ref)));
    "Session",(fun ref -> snd (session_record rpc session_id (Ref.of_string ref)));] in
  let findfn (name,record) =
    let r = record ref in
    try
      let u = field_lookup r "uuid" in
      u.get ();
      true
    with
	_ -> false
  in
  try let (n,r) = List.find findfn all in (n,r ref) with _ -> ("Unknown",[])
	*)

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
			make_field ~name:"gpu-group-uuid" ~get:(fun () -> try get_uuid_from_ref (x ()).API.pGPU_GPU_group with _ -> nid) ();
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
			make_field ~name:"gpu-group-uuid" ~get:(fun () -> try get_uuid_from_ref (x ()).API.vGPU_GPU_group with _ -> nid) ();
			make_field ~name:"gpu-group-name-label" ~get:(fun () -> try get_name_from_ref (x ()).API.vGPU_GPU_group with _ -> nid) ();
			make_field ~name:"currently-attached" ~get:(fun () -> string_of_bool (x ()).API.vGPU_currently_attached) ();
			make_field ~name:"other-config" ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.vGPU_other_config)
				~add_to_map:(fun k v -> Client.VGPU.add_to_other_config rpc session_id vgpu k v)
				~remove_from_map:(fun k -> Client.VGPU.remove_from_other_config rpc session_id vgpu k)
				~get_map:(fun () -> (x ()).API.vGPU_other_config) ();
			]
	}
