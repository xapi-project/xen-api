let rec split ?limit:(limit=(-1)) c s =
	let i = try String.index s c with Not_found -> -1 in
	let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
	if i = -1 || nlimit = 0
	then [s]
	else
		let a = String.sub s 0 i
		and b = String.sub s (i + 1) (String.length s - i - 1) in
		a :: (split ~limit: nlimit c b)

exception Invalid_header_string
exception Invalid_length
exception Invalid_checksum
exception Invalid_payload
exception No_update
exception Payload_too_large
exception Read_error

type payload = {
	timestamp: int64;
	datasources : (Ds.ds * Rrd.ds_owner) list;
}

module type PROTOCOL = sig
	val read_payload : Cstruct.t -> payload
	val write_payload : (int -> Cstruct.t) -> payload -> unit
end

module MakeProtocol = functor (P: PROTOCOL) -> struct
	module Page = struct
		open Gnt

		let read mapping =
			let cs = Cstruct.of_bigarray (Gnttab.Local_mapping.to_buf mapping) in
			P.read_payload cs

		let write share payload =
			let alloc_cstruct size =
				if size > Bigarray.Array1.dim share.Gntshr.mapping then
					raise Payload_too_large;
				Cstruct.of_bigarray share.Gntshr.mapping
			in
			P.write_payload alloc_cstruct payload
	end

	module File = struct
		let read fd =
			let buf = Bigarray.(Array1.map_file fd char c_layout false (-1)) in
			let cs = Cstruct.of_bigarray buf in
			P.read_payload cs

		let write fd payload =
			let alloc_cstruct size =
				let buf = Bigarray.(Array1.map_file fd char c_layout true size) in
				Cstruct.of_bigarray buf
			in
			P.write_payload alloc_cstruct payload
	end
end

module V1 = struct
	module Rrdp = Rrdp_common.Common(struct let name = "test_rrd_writer" end)

	let default_header = "DATASOURCES\n"
	let header_bytes = String.length default_header
	let length_start = header_bytes
	let length_bytes = 8 (* hex length of payload *)
	let checksum_start = header_bytes + length_bytes + 1 (* newline *)
	let checksum_bytes = 32 (* hex length of checksum *)
	let payload_start = header_bytes + length_bytes + checksum_bytes + 2 (* 2 newlines *)

	(* A helper function for extracting the dictionary out of the RPC type. *)
	let dict_of_rpc ~(rpc : Rpc.t) : (string * Rpc.t) list =
		match rpc with Rpc.Dict d -> d | _ -> raise Invalid_payload

	(* A helper function for extracting the enum/list out of the RPC type. *)
	let list_of_rpc ~(rpc : Rpc.t) : Rpc.t list =
		match rpc with Rpc.Enum l -> l | _ -> raise Invalid_payload

	(* [assoc_opt ~key ~default l] gets string value associated with [key] in
	 * [l], returning [default] if no mapping is found. *)
	let assoc_opt ~(key : string) ~(default : string)
			(l : (string * Rpc.t) list) : string =
		try Rpc.string_of_rpc (List.assoc key l) with
		| Not_found -> default
		| e -> raise e

	(* Converts string to the corresponding datasource type. *)
	let ds_ty_of_string (s : string) : Rrd.ds_type =
		match String.lowercase s with
		| "absolute" -> Rrd.Gauge
		| "rate" -> Rrd.Absolute
		| "absolute_to_rate" -> Rrd.Derive
		| _ -> raise Invalid_payload

	(* Possible types for values in datasources. *)
	type value_type = Float | Int64

	(* Converts string to datasource value type. *)
	let val_ty_of_string (s : string) : value_type =
		match String.lowercase s with
		| "float" -> Float
		| "int64" -> Int64
		| _ -> raise Invalid_payload

	(* Converts an RPC value to a typed datasource value. *)
	let ds_value_of_rpc ~(ty : value_type) ~(rpc : Rpc.t) : Rrd.ds_value_type =
		match ty with
		| Float -> Rrd.VT_Float (Rpc.float_of_rpc rpc)
		| Int64 -> Rrd.VT_Int64 (Rpc.int64_of_rpc rpc)

	(* Converts a string to value of datasource owner type. *)
	let owner_of_string (s : string) : Rrd.ds_owner =
		match split ' ' (String.lowercase s) with
		| ["host"] -> Rrd.Host
		| ["vm"; uuid] -> Rrd.VM uuid
		| ["sr"; uuid] -> Rrd.SR uuid
		| _ -> raise Invalid_payload

	(* A function that converts a JSON type into a datasource type, assigning
	 * default values appropriately. *)
	let ds_of_rpc ((name, rpc) : (string * Rpc.t)) : (Ds.ds * Rrd.ds_owner) =
		try
			let open Rpc in
			let kvs = dict_of_rpc ~rpc in
			let description = assoc_opt ~key:"description" ~default:"" kvs in
			let units = assoc_opt ~key:"units" ~default:"" kvs in
			let ty =
				ds_ty_of_string (assoc_opt ~key:"type" ~default:"absolute" kvs) in
			let val_ty =
				val_ty_of_string (assoc_opt ~key:"value_type" ~default:"float" kvs) in
			let value =
				let value_rpc = List.assoc "value" kvs in
				ds_value_of_rpc ~ty:val_ty ~rpc:value_rpc
			in
			let min =
				float_of_string (assoc_opt ~key:"min" ~default:"-infinity" kvs) in
			let max =
				float_of_string (assoc_opt ~key:"max" ~default:"infinity" kvs) in
			let owner =
				owner_of_string (assoc_opt ~key:"owner" ~default:"host" kvs) in
			let ds = Ds.ds_make ~name ~description ~units ~ty ~value ~min ~max
				~default:true () in
			ds, owner
		with e -> raise e

	(* A function that parses the payload written by a plugin into the payload
	 * type. *)
	let parse_payload ~(json : string) : payload =
		try
			let open Rpc in
			let rpc = Jsonrpc.of_string json in
			let kvs = dict_of_rpc ~rpc in
			let timestamp = int64_of_rpc (List.assoc "timestamp" kvs) in
			let datasource_rpcs = dict_of_rpc (List.assoc "datasources" kvs) in
			{timestamp; datasources = List.map ds_of_rpc datasource_rpcs}
		with _ -> raise Invalid_payload

	let of_dss dss =
		Rrdp.json_of_dss ~hdr:default_header (Rrdp.now()) dss

	let to_dss text =
		let length_str = "0x" ^ (String.sub text length_start length_bytes) in
		let length = int_of_string length_str in
		let checksum = String.sub text checksum_start checksum_bytes in
		let payload = String.sub text payload_start length in
		length, checksum, (parse_payload payload)

	let read_payload cs =
		let header = Cstruct.copy cs 0 header_bytes in
		if header <> default_header then
			raise Invalid_header_string;
		let length_str = "0x" ^ (Cstruct.copy cs length_start length_bytes) in
		let length = int_of_string length_str in
		let checksum = Cstruct.copy cs checksum_start checksum_bytes in
		let payload = Cstruct.copy cs payload_start length in
		parse_payload payload

	let write_payload alloc_cstruct payload =
		let json =
			Rrdp.json_of_dss ~hdr:default_header payload.timestamp payload.datasources
		in
		let length = String.length json in
		let cs = alloc_cstruct length in
		Cstruct.blit_from_string json 0 cs 0 length
end
