(* Represents a key-value pair and its allowed values. *)
type requirement = {
	key : string;
	default_value : string option;
	is_valid_value : string -> bool;
}

(* Check that a key-value pair is present for each requirement. *)
(* If any are not, add the default value. *)
let add_defaults requirements kvpairs =
	let key_is_present requirement = List.exists
		(fun (key, _) -> key = requirement.key)
		kvpairs
	in
	List.fold_left
		(fun acc requirement ->
			if key_is_present requirement
			then acc
			else match requirement.default_value with
				| None -> acc
				| Some default_value ->
					(requirement.key, default_value)::acc)
		kvpairs requirements

(* Validate a key-value pair against a list of requirements. *)
let validate_kvpair field_name requirements (key, value) =
	let fail () = raise Api_errors.(Server_error
		(invalid_value, [field_name; Printf.sprintf "%s = %s" key value]))
	in
	(* Try to find a required property requirement with this name. *)
	let requirement =
		try
			List.find
				(fun requirement -> requirement.key = key)
				requirements
		with Not_found -> fail ()
	in
	(* Check whether the proposed value for this property is allowed. *)
	if not (requirement.is_valid_value value) then fail ()
