open Client
open Stringext

let make_task ~__context =
	let uuid = Uuid.make_uuid () in
	let ref = Ref.make () in
	Db.DR_task.create ~__context ~ref ~uuid:(Uuid.to_string uuid);
	ref

(* A type to represent an SR record parsed from an sr_probe result. *)
type sr_probe_sr = {
	uuid: string option;
	name_label: string;
	name_description: string;
	metadata_detected: bool;
}

(* Attempt to parse a list of key/value pairs from XML. *)
let parse_kv = function 
	| Xml.Element(key, _, [ Xml.PCData v ]) -> 
		key, String.strip String.isspace v (* remove whitespace at both ends *)
	| _ -> failwith "Malformed key/value pair"

(* Parse a list of SRs from an iscsi probe result. *)
let parse_sr_probe_iscsi xml = 
	match Xml.parse_string xml with
	| Xml.Element("SRlist", _, children) ->
		let parse_sr = function
		| Xml.Element("SR", _, children) ->
			let all = List.map parse_kv children in
			{
				uuid = Some(List.assoc "UUID" all);
				name_label = List.assoc "name_label" all;
				name_description = List.assoc "name_description" all;
				metadata_detected = (List.assoc "pool_metadata_detected" all = "true");
			}
		| _ -> failwith "Malformed or missing <SR>" in
		List.map parse_sr children
	| _ -> failwith "Missing <SRlist> element"

(* Parse a list of SRs from an hba probe result. *)
let parse_sr_probe_hba xml =
	match Xml.parse_string xml with
	| Xml.Element("Devlist", _, children) ->
		let parse_sr = function
		| Xml.Element("BlockDevice", _, children) ->
			let all = List.map parse_kv children in
			{
				uuid = None;
				name_label = List.assoc "name_label" all;
				name_description = List.assoc "name_description" all;
				metadata_detected = (List.assoc "pool_metadata_detected" all = "true");
			}
		| _ -> failwith "Malformed or missing <BlockDevice>" in
		List.map parse_sr children
	| _ -> failwith "Missing <Devlist> element"

(* Add SR records to the database. *)
(* The SR records will have their introduced_by field set to the DR_task. *)
let create ~__context ~_type ~device_config ~whitelist =
	(* Check if licence allows disaster recovery. *)
	if (not (Pool_features.is_enabled ~__context Features.DR)) then
		raise (Api_errors.Server_error(Api_errors.license_restriction, []));
	let dr_task = make_task ~__context in
	(* Probe the specified device for SRs. *)
	let probe_result = Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			Client.SR.probe ~rpc ~session_id
				~host:Ref.null ~device_config
				~_type ~sm_config:[])
	in
	(* Parse the probe result. *)
	let sr_records = match _type with
	| "lvmoiscsi" -> parse_sr_probe_iscsi probe_result
	| "lvmohba" -> parse_sr_probe_hba probe_result
	| _ -> raise (Api_errors.Server_error(Api_errors.invalid_value,
		["type"; _type]))
	in
	(* If the SR record has a UUID, make sure it's in the whitelist. *)
	let sr_records = List.filter
		(fun sr_record ->
			match sr_record.uuid with
			| None -> true
			| Some uuid -> List.mem uuid whitelist)
		sr_records
	in
	(* Create the SR records and attach each SR to each host. *)
	let hosts = Db.Host.get_all ~__context in
	List.iter (fun sr_record ->
		Helpers.call_api_functions ~__context
			(fun rpc session_id ->
				let sr_uuid = match sr_record.uuid with
				| Some uuid -> uuid
				| None -> ""
				in
				(* Create the SR record. *)
				let sr = Client.SR.introduce ~rpc ~session_id
					~uuid:sr_uuid ~name_label:sr_record.name_label
					~name_description:sr_record.name_description
					~_type ~content_type:"" ~shared:true
					~sm_config:[]
				in
				Db.SR.set_introduced_by ~__context ~self:sr ~value:dr_task;
				(* Create and plug PBDs. *)
				List.iter (fun host ->
					let pbd = Client.PBD.create ~rpc ~session_id ~host ~sR:sr ~device_config ~other_config:[] in
					Client.PBD.plug ~rpc ~session_id ~self:pbd) hosts)) sr_records;
	dr_task

let destroy ~__context ~self =
	let introduced_SRs = Db.DR_task.get_introduced_SRs ~__context ~self in
	List.iter (fun sr ->
		let pbds = Db.SR.get_PBDs ~__context ~self:sr in
		(* Unplug all PBDs associated with this SR. *)
		List.iter (fun pbd ->
			Helpers.call_api_functions ~__context
				(fun rpc session_id -> Client.PBD.unplug ~rpc ~session_id ~self:pbd)
		) pbds;
		(* Forget the SR. *)
		Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.SR.forget ~rpc ~session_id ~sr)
	) introduced_SRs;
	Db.DR_task.destroy ~__context ~self
