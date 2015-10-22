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

(** Combinators for validated map access *)
type 'a pickler = (string -> 'a) * ('a -> string)
let pickler : (string -> 'a) -> ('a -> string) -> 'a pickler = 
	fun of_string to_string -> (of_string, to_string)

let string  : string pickler = (fun x -> x), (fun x -> x)
let int     : int pickler = int_of_string, string_of_int

let cons x xs = x :: xs

type assoc_list = (string * string) list
type 'a field = (assoc_list -> 'a) * ('a -> assoc_list -> assoc_list)

let field : string -> 'a pickler -> 'a field = 
	fun name (of_string, to_string) ->
        	(fun assoc_list -> assoc_list
                	|> List.assoc name
                	|> of_string),
        	(fun value assoc_list -> assoc_list
                	|> List.remove_assoc name
                	|> cons (name, to_string value))

let getf : ?default:'a -> 'a field -> assoc_list -> 'a =
	fun ?default (of_string, _) record ->
		try of_string record
		with Not_found as e ->
			Backtrace.is_important e;
			match default with
			| None ->
				raise e
			| Some d -> d

let setf : 'a field -> 'a -> assoc_list -> assoc_list = 
	fun (_, to_string) value record -> to_string value record

