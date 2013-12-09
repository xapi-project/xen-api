let rec split ?limit:(limit=(-1)) c s =
	let i = try String.index s c with Not_found -> -1 in
	let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
	if i = -1 || nlimit = 0
	then [s]
	else
		let a = String.sub s 0 i
		and b = String.sub s (i + 1) (String.length s - i - 1) in
		a :: (split ~limit: nlimit c b)

(* A helper function for extracting the dictionary out of the RPC type. *)
let dict_of_rpc ~(rpc : Rpc.t) : (string * Rpc.t) list =
	match rpc with Rpc.Dict d -> d | _ -> raise Rrd_protocol.Invalid_payload

(* A helper function for extracting the enum/list out of the RPC type. *)
let list_of_rpc ~(rpc : Rpc.t) : Rpc.t list =
	match rpc with Rpc.Enum l -> l | _ -> raise Rrd_protocol.Invalid_payload

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
	| _ -> raise Rrd_protocol.Invalid_payload

(* Converts a string to value of datasource owner type. *)
let owner_of_string (s : string) : Rrd.ds_owner =
	match split ' ' (String.lowercase s) with
	| ["host"] -> Rrd.Host
	| ["vm"; uuid] -> Rrd.VM uuid
	| ["sr"; uuid] -> Rrd.SR uuid
	| _ -> raise Rrd_protocol.Invalid_payload
