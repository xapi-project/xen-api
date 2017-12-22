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

type key_type = Enum of string list | EnumSet of string list | IntRange of int*int | String | ReqValue of string

let err field key value =
	let msg = if key="" then field else field^":"^key in
	raise (Api_errors.Server_error (Api_errors.invalid_value, [msg;value]))

let mem value range =
	try Some
	(List.find (fun r->(String.lowercase_ascii value)=(String.lowercase_ascii r)) range)
	with Not_found -> None

let assert_value ~field ~key ~attr ~value =
	let err v = err field key v in
	let (ty,default) = attr in
	match ty with
	| Enum range -> (match (mem value range) with None->err value|Some v->v)
	| EnumSet range -> (* enumset is a comma-separated string *)
                let vs = Stdext.Xstringext.String.split ',' value in
		List.fold_right
			(fun v acc->match (mem v range) with
				|None->err v
				|Some v->
					if acc="" then v
					else begin
						if (Stdext.Xstringext.String.has_substr acc v) then err value
						else (v^","^acc)
					end;
			)
			vs ""
	| IntRange (min,max) ->
		let v=try int_of_string value with _->err value in
		if (v<min || v>max) then err value else value
	| ReqValue required_value -> if value <> required_value then err value else value
	| String -> value

let with_ks ~kss ~fn =
	let field,kss=kss in
	let corrected_values = List.filter (fun cv->cv<>None) (List.map (fun ks-> fn field ks) kss) in
	if List.length corrected_values < 1 then []
	else (match List.hd corrected_values with None->[]|Some cv->cv)

let assert_req_values ~field ~ks ~vs =
	(* each required values in this ks must match the one in the vs map this key/value belongs to *)
	let req_values = List.fold_right
	(fun (k,attr) acc->match attr with(ReqValue rv),_->(k,rv)::acc|_->acc) ks []
	in
	(if vs<>[] then
		List.iter (fun (k,rv)->
			if (List.mem_assoc k vs) then (if rv<>(List.assoc k vs) then err field k rv)
		) req_values
	)

(* uses xs elements to overwrite ys elements *)
let merge xs ys =
	let nys = List.map (fun (ky,vy)->if List.mem_assoc ky xs then (ky,(List.assoc ky xs)) else (ky,vy)) ys in
	let nxs = List.filter (fun (kx,_)->not(List.mem_assoc kx nys)) xs in
	nxs@nys

let assert_key ~field ~ks ~key ~value =
	(* check if the key and value conform to this ks *)
	(if not (List.mem_assoc key ks)
		then err field key value
	else
		assert_value ~field ~key ~attr:(List.assoc key ks) ~value
	)

let assert_keys ~ty ~ks ~value ~db =
	let value = merge value db in
	with_ks ~kss:ks ~fn:
		(fun field (xt,ks) ->
			if (xt=ty) then Some
			(
				assert_req_values ~field ~ks ~vs:value;
				(* for this ks, each key value must be valid *)
				List.map (fun (k,v)-> k,(assert_key ~field ~ks ~key:k ~value:v)) value
			)
			else None
		)

let assert_all_keys ~ty ~ks ~value ~db =
	let value = merge value db in
	with_ks ~kss:ks ~fn:
		(fun field (xt,ks)->
			if (xt=ty) then Some
			(
				assert_req_values ~field ~ks ~vs:value;
				(* add missing keys with default values *)
				let value = List.map (fun (k,(kt,default))->if List.mem_assoc k value then (k,(List.assoc k value)) else (k,default)) ks in
				(* remove extra unexpected keys *)
				let value = List.fold_right (fun (k,v) acc->if List.mem_assoc k ks then (k,v)::acc else acc) value [] in
				(* for this ks, each key value must be valid *)
				List.map (fun (k,v)-> k,(assert_key ~field ~ks ~key:k ~value:v)) value
			)
			else None
		)

