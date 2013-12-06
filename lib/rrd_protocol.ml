let rec split ?limit:(limit=(-1)) c s =
	let i = try String.index s c with Not_found -> -1 in
	let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
	if i = -1 || nlimit = 0
	then [s]
	else
		let a = String.sub s 0 i
		and b = String.sub s (i + 1) (String.length s - i - 1) in
		a :: (split ~limit: nlimit c b)

let now () = Int64.of_float (Unix.gettimeofday ())

exception Invalid_header_string
exception Invalid_length
exception Invalid_checksum
exception Invalid_payload
exception No_update
exception Payload_too_large
exception Read_error

module Json = struct
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

	(* Converts a string to value of datasource owner type. *)
	let owner_of_string (s : string) : Rrd.ds_owner =
		match split ' ' (String.lowercase s) with
		| ["host"] -> Rrd.Host
		| ["vm"; uuid] -> Rrd.VM uuid
		| ["sr"; uuid] -> Rrd.SR uuid
		| _ -> raise Invalid_payload
end

type payload = {
	timestamp: int64;
	datasources : (Ds.ds * Rrd.ds_owner) list;
}

module type PROTOCOL = sig
	val read_payload : Cstruct.t -> payload
	val write_payload : (int -> Cstruct.t) -> payload -> unit
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

	(* A function that converts a JSON type into a datasource type, assigning
	 * default values appropriately. *)
	let ds_of_rpc ((name, rpc) : (string * Rpc.t)) : (Ds.ds * Rrd.ds_owner) =
		try
			let open Rpc in
			let kvs = Json.dict_of_rpc ~rpc in
			let description = Json.assoc_opt ~key:"description" ~default:"" kvs in
			let units = Json.assoc_opt ~key:"units" ~default:"" kvs in
			let ty =
				Json.ds_ty_of_string
					(Json.assoc_opt ~key:"type" ~default:"absolute" kvs)
			in
			let val_ty =
				val_ty_of_string
					(Json.assoc_opt ~key:"value_type" ~default:"float" kvs)
			in
			let value =
				let value_rpc = List.assoc "value" kvs in
				ds_value_of_rpc ~ty:val_ty ~rpc:value_rpc
			in
			let min =
				float_of_string (Json.assoc_opt ~key:"min" ~default:"-infinity" kvs) in
			let max =
				float_of_string (Json.assoc_opt ~key:"max" ~default:"infinity" kvs) in
			let owner =
				Json.owner_of_string (Json.assoc_opt ~key:"owner" ~default:"host" kvs)
			in
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
			let kvs = Json.dict_of_rpc ~rpc in
			let timestamp = int64_of_rpc (List.assoc "timestamp" kvs) in
			let datasource_rpcs = Json.dict_of_rpc (List.assoc "datasources" kvs) in
			{timestamp; datasources = List.map ds_of_rpc datasource_rpcs}
		with _ -> raise Invalid_payload

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
		let payload_string = Cstruct.copy cs payload_start length in
		if payload_string |> Digest.string |> Digest.to_hex <> checksum then
			raise Invalid_checksum;
		parse_payload payload_string

	let write_payload alloc_cstruct payload =
		let json =
			Rrdp.json_of_dss ~hdr:default_header payload.timestamp payload.datasources
		in
		let length = String.length json in
		let cs = alloc_cstruct length in
		Cstruct.blit_from_string json 0 cs 0 length
end

module V2 = struct
	module Rrdp = Rrdp_common.Common(struct let name = "test_rrd_writer" end)

	(* Field sizes. *)
	let default_header = "DATASOURCES"

	let header_bytes = String.length default_header

	let data_checksum_bytes = 16

	let metadata_checksum_bytes = 16

	let datasource_count_bytes = 4

	let timestamp_bytes = 8

	let datasource_value_bytes = 8

	let metadata_length_bytes = 4

	let get_total_bytes datasource_count metadata_length =
		header_bytes +
		data_checksum_bytes +
		metadata_checksum_bytes +
		datasource_count_bytes +
		timestamp_bytes +
		(datasource_value_bytes * datasource_count) +
		(metadata_length * 8)

	(* Field start points. *)
	let header_start = 0

	let data_checksum_start = header_start + header_bytes

	let metadata_checksum_start = data_checksum_start + data_checksum_bytes

	let datasource_count_start = metadata_checksum_start + metadata_checksum_bytes

	let timestamp_start = datasource_count_start + datasource_count_bytes

	let datasource_value_start = timestamp_start + timestamp_bytes

	let get_metadata_length_start datasource_count =
		datasource_value_start + (datasource_count * datasource_value_bytes)

	let get_metadata_start datasource_count =
		(get_metadata_length_start datasource_count) + metadata_length_bytes

	(* Reading fields from cstructs. *)
	module Read = struct
		let header cs =
			let header = String.create header_bytes in
			Cstruct.blit_to_string cs header_start header 0 header_bytes;
			header

		let data_checksum cs =
			let data_checksum = String.create data_checksum_bytes in
			Cstruct.blit_to_string cs data_checksum_start
				data_checksum 0 data_checksum_bytes;
			data_checksum

		let metadata_checksum cs =
			let metadata_checksum = String.create metadata_checksum_bytes in
			Cstruct.blit_to_string cs metadata_checksum_start
				metadata_checksum 0 metadata_checksum_bytes;
			metadata_checksum

		let datasource_count cs =
			Int32.to_int (Cstruct.BE.get_uint32 cs datasource_count_start)

		let timestamp cs =
			Cstruct.BE.get_uint64 cs timestamp_start

		let datasource_values cs cached_datasources =
			let rec aux start acc = function
				| [] -> acc
				| (cached_datasource, owner) :: rest -> begin
					(* Replace the cached datasource's value with the value read
					 * from the cstruct. *)
					let value = match cached_datasource.Ds.ds_value with
					| Rrd.VT_Float _ ->
						Rrd.VT_Float (Int64.float_of_bits (Cstruct.BE.get_uint64 cs start))
					| Rrd.VT_Int64 _ ->
						Rrd.VT_Int64 (Cstruct.BE.get_uint64 cs start)
					| Rrd.VT_Unknown -> failwith "unknown datasource type"
					in
					aux (start + datasource_value_bytes)
						(({cached_datasource with Ds.ds_value = value}, owner) :: acc)
						rest
				end
			in
			List.rev (aux datasource_value_start [] cached_datasources)

		let metadata_length cs datasource_count =
			Int32.to_int
				(Cstruct.BE.get_uint32 cs (get_metadata_length_start datasource_count))

		let metadata cs datasource_count metadata_length =
			let metadata = String.create metadata_length in
			Cstruct.blit_to_string
				cs (get_metadata_start datasource_count)
				metadata 0 metadata_length;
			metadata
	end

	(* Writing fields to cstructs. *)
	module Write = struct
		let header cs =
			Cstruct.blit_from_string default_header 0 cs header_start header_bytes

		let data_checksum cs value =
			Cstruct.blit_from_string value 0
				cs data_checksum_start data_checksum_bytes

		let metadata_checksum cs value =
			Cstruct.blit_from_string value 0
				cs metadata_checksum_start metadata_checksum_bytes

		let datasource_count cs value =
			Cstruct.BE.set_uint32 cs datasource_count_start (Int32.of_int value)

		let timestamp cs value =
			Cstruct.BE.set_uint64 cs timestamp_start value

		let datasource_values cs values =
			let rec aux start = function
				| [] -> ()
				| value :: rest -> begin
					let to_write = match value with
					| Rrd.VT_Float f -> Int64.bits_of_float f
					| Rrd.VT_Int64 i -> i
					| Rrd.VT_Unknown -> failwith "unknown datasource type"
					in
					Cstruct.BE.set_uint64 cs start to_write;
					aux (start + datasource_value_bytes) rest
				end
			in
			aux datasource_value_start values

		let metadata_length cs value datasource_count =
			Cstruct.BE.set_uint32 cs
				(get_metadata_length_start datasource_count) (Int32.of_int value)

		let metadata cs value datasource_count =
			let metadata_length = String.length value in
			Cstruct.blit_from_string value 0 cs
				(get_metadata_start datasource_count)
				metadata_length
	end

	let last_data_checksum = ref ""
	let last_metadata_checksum = ref ""
	let cached_datasources : (Ds.ds * Rrd.ds_owner) list ref = ref []

	let value_of_string (s : string) : Rrd.ds_value_type =
		match s with
		| "float" -> Rrd.VT_Float 0.0
		| "int64" -> Rrd.VT_Int64 0L
		| _ -> raise Invalid_payload

	(* WARNING! This creates datasources from datasource metadata, hence the
	 * values will be meaningless. The types however, will be correct. *)
	let uninitialised_ds_of_rpc ((name, rpc) : (string * Rpc.t))
			: (Ds.ds * Rrd.ds_owner) =
		let open Rpc in
		let kvs = Json.dict_of_rpc ~rpc in
		let description = Json.assoc_opt ~key:"description" ~default:"" kvs in
		let units = Json.assoc_opt ~key:"units" ~default:"" kvs in
		let ty =
			Json.ds_ty_of_string (Json.assoc_opt ~key:"type" ~default:"absolute" kvs)
		in
		let value =
			value_of_string (Rpc.string_of_rpc (List.assoc "value_type" kvs)) in
		let min =
			float_of_string (Json.assoc_opt ~key:"min" ~default:"-infinity" kvs) in
		let max =
			float_of_string (Json.assoc_opt ~key:"max" ~default:"infinity" kvs) in
		let owner =
			Json.owner_of_string (Json.assoc_opt ~key:"owner" ~default:"host" kvs) in
		let ds = Ds.ds_make ~name ~description ~units
			~ty ~value ~min ~max ~default:true () in
		ds, owner

	let parse_metadata metadata =
		try
			let open Rpc in
			let rpc = Jsonrpc.of_string metadata in
			let kvs = Json.dict_of_rpc ~rpc in
			let datasource_rpcs = Json.dict_of_rpc (List.assoc "datasources" kvs) in
			List.map uninitialised_ds_of_rpc datasource_rpcs
		with _ -> raise Invalid_payload

	let read_payload cs =
		(* Check the header string is present and correct. *)
		let header = Read.header cs in
		if not (header = default_header) then
			raise Invalid_header_string;
		(* Check that the data checksum has changed. Since the checksummed data
		 * includes the timestamp, this should change with every update. *)
		let data_checksum = Read.data_checksum cs in
		if data_checksum = !last_data_checksum then raise No_update;
		let metadata_checksum = Read.metadata_checksum cs in
		let datasource_count = Read.datasource_count cs in
		let timestamp = Read.timestamp cs in
		(* Check the data checksum is correct. *)
		let data_checksum_calculated =
			Cstruct_hash.md5sum cs
				timestamp_start
				(timestamp_bytes + datasource_count * datasource_value_bytes)
		in
		if not (data_checksum = data_checksum_calculated)
		then raise Invalid_checksum
		else last_data_checksum := data_checksum;
		(* Read the datasource values. *)
		let datasources =
			if metadata_checksum = !last_metadata_checksum then begin
				(* Metadata hasn't changed, so just read the datasources values. *)
				Read.datasource_values cs !cached_datasources
			end else begin
				(* Metadata has changed - we need to read it to find the types of the
				 * datasources, then go back and read the values themselves. *)
				let metadata_length = Read.metadata_length cs datasource_count in
				let metadata = Read.metadata cs datasource_count metadata_length in
				(* Check the metadata checksum is correct. *)
				if not (metadata_checksum = (Digest.string metadata))
				then raise Invalid_checksum;
				(* If all is OK, cache the metadata checksum and read the values
				 * based on this new metadata. *)
				last_metadata_checksum := metadata_checksum;
				Read.datasource_values cs (parse_metadata metadata)
			end
		in
		cached_datasources := datasources;
		{
			timestamp = timestamp;
			datasources = datasources;
		}

	let write_payload alloc_cstruct payload =
		let metadata = Rrdp.json_metadata_of_dss payload.datasources in
		let datasource_count = List.length payload.datasources in
		let metadata_length = String.length metadata in
		let total_bytes = get_total_bytes datasource_count metadata_length in
		let cs = alloc_cstruct total_bytes in
		(* Write header. *)
		Write.header cs;
		(* Write metadata checksum. *)
		Write.metadata_checksum cs (Digest.string metadata);
		(* Write number of datasources. *)
		Write.datasource_count cs datasource_count;
		(* Write timestamp. *)
		Write.timestamp cs (now ());
		(* Write datasource values. *)
		Write.datasource_values cs
			(List.map (fun (ds, _) -> ds.Ds.ds_value) payload.datasources);
		(* Write the metadata. *)
		Write.metadata cs metadata datasource_count;
		(* Write the metadata length. *)
		Write.metadata_length cs metadata_length datasource_count;
		(* Write the data checksum. *)
		let data_checksum =
			Cstruct_hash.md5sum cs timestamp_start
				(timestamp_bytes + datasource_count * datasource_value_bytes) in
		Write.data_checksum cs data_checksum
end

let of_string = function
	| "v1" -> (module V1 : PROTOCOL)
	| "v2" -> (module V2 : PROTOCOL)
	| _ -> failwith "Unknown protocol"
