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
open API
open Stdext
open Xstringext
open Listext

(* === Common XML operations === *)

(** Generates an XML leaf element of the form: *)
(**     <name>value</name>                     *)
let xml_leaf_element name value =
  Xml.Element (
    name, [], [Xml.PCData value]
  )

(** Returns true iff. the given element matches the given name. *)
let xml_element_has_name name element =
  match element with
  | Xml.Element (name_, _, _) -> name = name_
  | _                         -> false

(** Returns a sub-list of the given element list, containing *)
(** only those elements with the specified name.             *)
let xml_elements_with_name elements name =
  List.filter (xml_element_has_name name) elements

(** Returns the first element with the specified name from *)
(** the given element list.                                *)
let first_xml_element_with_name elements name =
  try
    Some (List.find (xml_element_has_name name) elements)
  with
    Not_found -> None

(** Parses an XML element of the form "<name>value</value>".  *)
(** Returns a (name, value) string pair, where the arguments  *)
(** are stripped of leading and trailing whitespace.          *)
let hash_table_entry_of_leaf_xml_element = function
  | Xml.Element (name, _, Xml.PCData (value) :: values) ->
    Some (
      String.strip String.isspace name,
      String.strip String.isspace value
    )
  | Xml.Element (name, _, []) -> Some (String.strip String.isspace name, "")
  | _ -> None

(** Parses a list of XML elements of the form:    *)
(**     <name0>value0</name0>                     *)
(**     <name1>value1</name1>                     *)
(**     <name2>value2</name2>                     *)
(**     ...                                       *)
(** Returns a string hash table with an entry for *)
(** each element matched:                         *)
(**     (name0 -> value0)                         *)
(**     (name1 -> value1)                         *)
(**     (name2 -> value2)                         *)
(**     ...                                       *)
let hash_table_of_leaf_xml_element_list list =
  Hashtblext.of_list (
    List.filter_map hash_table_entry_of_leaf_xml_element list
  )

(* === Daemon configuration === *)

module DaemonConfiguration = struct

  (* Taken from Marathon's spec section 4.1.4.4 *)
  let filename = Filename.concat "/etc/xensource" "xhad.conf"

  module Host = struct

    type t = {
      uuid : string;
      address : string
    }

    (** Simple type convertor. *)
    let of_host_t host_t = {
      uuid    = host_t.host_uuid   ;
      address = host_t.host_address;
    }

    (** Converts the given HA daemon host configuration *)
    (** into an XML element tree.                       *)
    let to_xml_element host =
      Xml.Element (
        "host", [], [
          (xml_leaf_element "HostID"    host.uuid   );
          (xml_leaf_element "IPaddress" host.address);
        ]
      )

  end

  type t = {
    common_generation_uuid : string;
    common_udp_port : int;
    common_hosts : Host.t list;
    local_host_uuid : string;
    local_heart_beat_interface : string;
    local_heart_beat_physical_interface : string;
    local_state_file : string;
    heart_beat_interval : int option;
    state_file_interval : int option;
    heart_beat_timeout : int option;
    state_file_timeout : int option;
    heart_beat_watchdog_timeout : int option;
    state_file_watchdog_timeout : int option;
    boot_join_timeout : int option;
    enable_join_timeout : int option;
    xapi_healthcheck_interval : int option;
    xapi_healthcheck_timeout : int option;
    xapi_restart_attempts : int option;
    xapi_restart_timeout : int option;
    xapi_licensecheck_timeout : int option;
  }

  (** See interface. *)
  let create
      ?(common_udp_port = 49154)
      ?heart_beat_interval
      ?state_file_interval
      ?heart_beat_timeout
      ?state_file_timeout
      ?heart_beat_watchdog_timeout
      ?state_file_watchdog_timeout
      ?boot_join_timeout
      ?enable_join_timeout
      ?xapi_healthcheck_interval
      ?xapi_healthcheck_timeout
      ?xapi_restart_attempts
      ?xapi_restart_timeout
      ?xapi_licensecheck_timeout
      ~common_generation_uuid
      ~local_heart_beat_interface
      ~local_heart_beat_physical_interface
      ~local_state_file
      ~__context
      () =
    let records = Db.Host.get_all_records ~__context in
    let common_hosts = List.map
        (fun (_, host) -> Host.of_host_t host)
        records in
    let local_host_uuid =
      Db.Host.get_uuid
        ~__context ~self:!Xapi_globs.localhost_ref in
    {
      common_hosts                = common_hosts;
      common_generation_uuid      = (Uuid.to_string common_generation_uuid);
      common_udp_port             = common_udp_port;
      local_host_uuid             = local_host_uuid;
      local_heart_beat_interface  = local_heart_beat_interface;
      local_heart_beat_physical_interface = local_heart_beat_physical_interface;
      local_state_file            = local_state_file;
      heart_beat_interval         = heart_beat_interval;
      state_file_interval         = state_file_interval;
      heart_beat_timeout          = heart_beat_timeout;
      state_file_timeout          = state_file_timeout;
      heart_beat_watchdog_timeout = heart_beat_watchdog_timeout;
      state_file_watchdog_timeout = state_file_watchdog_timeout;
      boot_join_timeout           = boot_join_timeout;
      enable_join_timeout         = enable_join_timeout;
      xapi_healthcheck_interval   = xapi_healthcheck_interval;
      xapi_healthcheck_timeout    = xapi_healthcheck_timeout;
      xapi_restart_attempts       = xapi_restart_attempts;
      xapi_restart_timeout        = xapi_restart_timeout;
      xapi_licensecheck_timeout   = xapi_licensecheck_timeout;
    }

  let int_parameter (name, param) =
    Opt.default [] (Opt.map (fun x -> [ xml_leaf_element name (string_of_int x) ]) param)

  (** Converts the given HA daemon configuration *)
  (** into an XML element tree.                  *)
  let to_xml_element config = Xml.Element (
      "xhad-config",
      [("version", "1.0")],
      [
        Xml.Element (
          "common-config", [],
          xml_leaf_element "GenerationUUID" (              config.common_generation_uuid) ::
          xml_leaf_element "UDPport"        (string_of_int config.common_udp_port       ) ::
          List.map Host.to_xml_element config.common_hosts @
          [
            Xml.Element ("parameters", [],
                         List.concat (List.map int_parameter
                                        [ "HeartbeatInterval",        config.heart_beat_interval;
                                          "HeartbeatTimeout",         config.heart_beat_timeout;
                                          "StateFileInterval",        config.state_file_interval;
                                          "StateFileTimeout",         config.state_file_timeout;
                                          "HeartbeatWatchdogTimeout", config.heart_beat_watchdog_timeout;
                                          "StateFileWatchdogTimeout", config.state_file_watchdog_timeout;
                                          "BootJoinTimeout",          config.boot_join_timeout;
                                          "EnableJoinTimeout",        config.enable_join_timeout;
                                          "XapiHealthCheckInterval",  config.xapi_healthcheck_interval;
                                          "XapiHealthCheckTimeout",   config.xapi_healthcheck_timeout;
                                          "XapiRestartAttempts",      config.xapi_restart_attempts;
                                          "XapiRestartTimeout",       config.xapi_restart_timeout;
                                          "XapiLicenseCheckTimeout",  config.xapi_licensecheck_timeout;
                                        ])
                        )
          ]
        );
        Xml.Element (
          "local-config", [],
          [
            Xml.Element (
              "localhost", [],
              [
                xml_leaf_element "HostID"             config.local_host_uuid           ;
                xml_leaf_element "HeartbeatInterface" config.local_heart_beat_interface;
                xml_leaf_element "HeartbeatPhysicalInterface" config.local_heart_beat_physical_interface;
                xml_leaf_element "StateFile"          config.local_state_file          ;
              ]
            )
          ]
        )
      ]
    )

  (** Converts the given HA daemon configuration *)
  (** into an XML string.                        *)
  let to_xml_string config =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ^ (
      Xml.to_string_fmt (
        to_xml_element config
      )
    )

end

(* === Live Set Information === *)

module LiveSetInformation = struct

  module Status = struct

    type t = Online | Offline | Starting

    let of_string string =
      match String.lowercase_ascii string with
      | "online" -> Some (Online)
      | "offline" -> Some (Offline)
      | "starting" -> Some Starting
      | _ -> invalid_arg "Invalid status string."

    let to_string = function
      | Online -> "online"
      | Offline -> "offline"
      | Starting -> "starting"

  end

  module Host = struct

    type t = {
      id: [`host] Uuid.t;
      liveness: bool;
      master: bool;
      state_file_access: bool;
      state_file_corrupted: bool;
      excluded: bool
    }

    (** Creates a new host record from a host XML element. *)
    (** The element must contain valid child elements for  *)
    (** each member of the host record type.               *)
    let of_xml_element = function
      | Xml.Element ("host", _, children) ->
        begin
          let table = hash_table_of_leaf_xml_element_list children in
          let find x =
            try Hashtbl.find table x
            with Not_found ->
              invalid_arg (Printf.sprintf "Missig entry '%s' within 'host' element" x) in
          let bool s =
            try bool_of_string (String.lowercase_ascii s)
            with Invalid_argument _ ->
              invalid_arg (Printf.sprintf "Invalid boolean value '%s' within 'host' element" s) in

          let uuid = Uuid.of_string in
          Some ({
              id                   = uuid (find "HostID"             );
              liveness             = bool (find "liveness"           );
              master               = bool (find "master"             );
              state_file_access    = bool (find "statefile_access"   );
              state_file_corrupted = bool (find "statefile_corrupted");
              excluded             = bool (find "excluded"           )
            })
        end
      | _ ->
        None

  end

  module HostRawData = struct
    type t = {
      id: [`host] Uuid.t;
      time_since_last_update_on_statefile: int;
      time_since_last_heartbeat: int;
      time_since_xapi_restart_first_attempted: int;
      heartbeat_active_list_on_heartbeat: [`host] Uuid.t list;
      heartbeat_active_list_on_statefile: [`host] Uuid.t list;
      (* ... *)
    }
    let of_xml_element = function
      | Xml.Element("host_raw_data", _, children) ->
        let table = hash_table_of_leaf_xml_element_list children in
        let find x =
          try Hashtbl.find table x
          with Not_found ->
            invalid_arg (Printf.sprintf "Missing entry '%s' within 'host_raw_data' element" x) in
        let int s =
          try int_of_string (String.lowercase_ascii s)
          with Invalid_argument _ ->
            invalid_arg (Printf.sprintf "Invalid integer value '%s' within 'host_raw_data' element" s) in
        let uuid = Uuid.of_string in
        let set f x = List.map f (String.split_f String.isspace x) in
        Some ({
            id = uuid (find "HostID");
            time_since_last_update_on_statefile     = int (find "time_since_last_update_on_statefile"    );
            time_since_last_heartbeat               = int (find "time_since_last_heartbeat"              );
            time_since_xapi_restart_first_attempted = int (find "time_since_xapi_restart_first_attempted");
            heartbeat_active_list_on_heartbeat      = set uuid (find "heartbeat_active_list_on_heartbeat");
            heartbeat_active_list_on_statefile      = set uuid (find "heartbeat_active_list_on_statefile");
          })
      | _ -> None

  end

  module Warning = struct
    type t = {
      statefile_lost: bool;
      heartbeat_approaching_timeout: bool;
      statefile_approaching_timeout: bool;
      xapi_healthcheck_approaching_timeout: bool;
      network_bonding_error: bool;
    }
    let of_xml_element = function
      | Xml.Element("warning_on_local_host", _, children) ->
        begin
          let table = hash_table_of_leaf_xml_element_list children in
          let find x =
            try Hashtbl.find table x
            with Not_found ->
              invalid_arg (Printf.sprintf "Missing entry '%s' within 'warning_on_local_host' element" x) in
          let bool x = find x = "TRUE" in
          Some({
              statefile_lost                        = bool "statefile_lost";
              heartbeat_approaching_timeout         = bool "heartbeat_approaching_timeout";
              statefile_approaching_timeout         = bool "statefile_approaching_timeout";
              xapi_healthcheck_approaching_timeout  = bool "Xapi_healthcheck_approaching_timeout";
              network_bonding_error                 = bool "network_bonding_error";
            })
        end
      | _ ->
        None
  end

  module RawStatus = struct
    type t = {
      statefile_latency: int;
      statefile_min: int;
      statefile_max: int;
      heartbeat_latency: int;
      heartbeat_min: int;
      heartbeat_max: int;
      xapi_healthcheck_latency: int;
      xapi_healthcheck_min: int;
      xapi_healthcheck_max: int;
      host_raw_data: ([`host] Uuid.t, HostRawData.t) Hashtbl.t;
    }
    let of_xml_element = function
      | Xml.Element("raw_status_on_local_host", _, children) ->
        begin
          let table = hash_table_of_leaf_xml_element_list children in
          let find x =
            try Hashtbl.find table x
            with Not_found ->
              invalid_arg (Printf.sprintf "Missing entry '%s' within 'raw_status_on_local_host' element" x) in
          let int s =
            try int_of_string (String.lowercase_ascii s)
            with Invalid_argument _ ->
              invalid_arg (Printf.sprintf "Invalid integer value '%s' within 'raw_status_on_local_host' element" s) in
          let host_raw_data = Hashtblext.of_list (
              List.map
                (fun host -> (host.HostRawData.id, host))
                (List.filter_map HostRawData.of_xml_element children)
            ) in

          Some({
              statefile_latency        = int (find "statefile_latency"           );
              statefile_min            = int (find "statefile_latency_min"       );
              statefile_max            = int (find "statefile_latency_max"       );
              heartbeat_latency        = int (find "heartbeat_latency"           );
              heartbeat_min            = int (find "heartbeat_latency_min"       );
              heartbeat_max            = int (find "heartbeat_latency_max"       );
              xapi_healthcheck_latency = int (find "Xapi_healthcheck_latency"    );
              xapi_healthcheck_min     = int (find "Xapi_healthcheck_latency_min");
              xapi_healthcheck_max     = int (find "Xapi_healthcheck_latency_max");
              host_raw_data            = host_raw_data;
            })
        end
      | _ ->
        None
  end



  type t = {
    status: Status.t;
    local_host_id: [`host] Uuid.t;
    hosts: ([`host] Uuid.t, Host.t) Hashtbl.t;
    raw_status_on_local_host: RawStatus.t option;
    warning_on_local_host: Warning.t option;
  }

  (** Creates a new HA live set information record *)
  (** from the given list of XML elements.         *)
  let of_xml_element_list elements = {
    hosts = Hashtblext.of_list (
        List.map
          (fun host -> (host.Host.id, host))
          (List.filter_map Host.of_xml_element elements)
      );
    local_host_id = (
      match first_xml_element_with_name elements "localhost" with
      | Some Xml.Element
          (_, _ , [Xml.Element ("HostID", _, [Xml.PCData (local_host_id)])]) ->
        Uuid.of_string local_host_id
      | _ ->
        invalid_arg "Invalid or missing 'localhost' element."
    );
    status = (
      let status_option =
        match first_xml_element_with_name elements "status" with
        | Some Xml.Element (_, _, [Xml.PCData (status_string)]) ->
          Status.of_string status_string
        | _ ->
          None in
      match status_option with
      | Some (status) -> status
      | _ -> invalid_arg "Invalid or missing 'status' element."
    );
    raw_status_on_local_host = (
      match first_xml_element_with_name elements "raw_status_on_local_host" with
      | Some x -> RawStatus.of_xml_element x
      | None -> None
    );
    warning_on_local_host = (
      match first_xml_element_with_name elements "warning_on_local_host" with
      | Some x -> Warning.of_xml_element x
      | None -> None
    );
  }

  (** Creates a new HA live set information record *)
  (** from the given root XML element.             *)
  let of_xml_element = function
    | Xml.Element ("ha_liveset_info", _, children) ->
      of_xml_element_list children
    | _ ->
      invalid_arg "Invalid or missing 'ha_liveset_info' element."

  (** See interface. *)
  let of_xml_string string =
    of_xml_element (Xml.parse_string string)

  (** See interface. *)
  let to_summary_string t =
    let status = Status.to_string t.status in
    let host h = Printf.sprintf "%s [%s%s%s%s%s%s]"
        (Uuid.string_of_uuid h.Host.id)
        (if h.Host.id = t.local_host_id then "*" else " ")
        (if h.Host.liveness             then "L" else " ")
        (if h.Host.master               then "M" else " ")
        (if h.Host.excluded             then "X" else " ")
        (if h.Host.state_file_access    then "A" else " ")
        (if h.Host.state_file_corrupted then "X" else " ") in
    status ^ " " ^ (Hashtbl.fold (fun _ h acc -> host h ^ "; " ^ acc) t.hosts "")

end

(* === Common I/O operations === *)
(*
(** Reads all lines from a input channel. *)
let string_of_channel input_channel =
	let rec string_of_channel input_channel input =
		try
			let line = input_line input_channel in
			string_of_channel input_channel (input ^ line ^ "\n")
		with End_of_file ->
			input
	in
		string_of_channel input_channel ""

(** Reads all lines from a file. *)
let string_of_file file_path=
	let input_channel = open_in file_path in
	string_of_channel input_channel
*)
(* === Daemon configuration test === *)
(*
module DaemonConfigurationTest = struct

	include DaemonConfiguration

	module HostTest = struct

		include Host

		let mock_host_0 = {
			uuid = "3a0d6864-42c6-4f82-8df9-d4cf3d747e2c";
			address = "0.0.0.0"
		}
		let mock_host_1 = {
			uuid = "96044fa9-2b43-444a-b764-f94fe10a5dec";
			address = "0.0.0.1"
		}
		let mock_host_2 = {
			uuid = "0447c77b-dc3f-4e75-8b97-eafb79a350fe";
			address = "0.0.0.2"
		}
		let mock_host_3 = {
			uuid = "7d9217cf-d59c-4b72-8116-7f860d4089c1";
			address = "0.0.0.3"
		}
		let mock_hosts = [mock_host_0; mock_host_1; mock_host_2; mock_host_3]

	end

	let ($) a b = b a

	let _ =
		{
			common_hosts               = HostTest.mock_hosts                   ;
			common_generation_uuid     = "bac1a32e-8598-4aea-ba21-e13682e436d6";
			common_udp_port            = 1234                                  ;
			local_host_uuid            ="d45bd9d2-e0db-4a91-80f8-371de132c33e" ;
			local_heart_beat_interface = "xebr0"                               ;
			local_state_file           ="/dev/xvde"                            ;
		}
		$ DaemonConfiguration.to_xml_string
		$ print_endline

end
*)
(* === Live set information test === *)
(*
module LiveSetInformationTest = struct

	include LiveSetInformation

	module HostTest = struct

		include Host

		let to_string host =
			"host {" ^
				"id = "                   ^ (Uuid.to_string host.id                  ) ^ "; " ^
				"liveness = "             ^ (string_of_bool host.liveness            ) ^ "; " ^
				"master = "               ^ (string_of_bool host.master              ) ^ "; " ^
				"state_file_access = "    ^ (string_of_bool host.state_file_access   ) ^ "; " ^
				"state_file_corrupted = " ^ (string_of_bool host.state_file_corrupted) ^ "; " ^
				"excluded = "             ^ (string_of_bool host.excluded            ) ^
			"}"

	end

	let to_string info =
		"info {" ^
			"status        = " ^ (Status.to_string info.status         ) ^ "; " ^
			"local_host_id = " ^ (Uuid.to_string   info.local_host_id  ) ^ "; " ^
			"hosts = [" ^
				String.concat "; " (
					List.map (HostTest.to_string) (Hashtbl.fold_values info.hosts)
				) ^
			"]" ^
		"}"

	let ($) f a = a f

	let _ =
		if Array.length Sys.argv != 2 then
			print_endline "usage: xha_interface <path-to-xml-file>"
		else
			Sys.argv. (1)
			$ string_of_file
			$ LiveSetInformation.of_xml_string
			$ to_string
			$ print_endline

end
*)
