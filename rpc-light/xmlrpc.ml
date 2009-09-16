open Printf

let debug = ref false
let debug (fmt: ('a, unit, string, unit) format4) : 'a =
	kprintf (fun s -> if !debug then begin print_string s; print_newline (); flush stdout end) fmt

(* marshalling/unmarshalling code *)
let rec to_string = function
	| Rpc.Int i  -> sprintf "<value><int>%i</int></value>" i
	| Rpc.Bool b -> sprintf "<value><bool>%b</bool></value>" b
	| Rpc.String s -> sprintf "<value><string>%s</string></value>" s
	| Rpc.Double d -> sprintf "<value><double>%f</double></value>" d
	| Rpc.Array a -> sprintf "<value><array><data>%s</data></array></value>" (String.concat "" (List.map to_string a))
	| Rpc.Struct f ->
		let members =
			List.map (fun (name, value) -> sprintf "<member><name>%s</name>%s</member>" name (to_string value)) f in
		sprintf "<value><struct>%s</struct></value>" (String.concat "" members)

exception Parse_error of string * string

let get_child xml =
	match Xml.children xml with
	| [x] -> x
	| _   -> raise (Parse_error ("get_child", Xml.to_string xml))

let get_content xml =
	debug "Value.get_content(%s)" (Xml.to_string xml);
	Xml.pcdata (get_child xml)

let rec of_xml xml =
	match Xml.tag xml with
	| "value" -> value_of_xml (get_child xml)
	| x -> raise (Parse_error (x, Xml.to_string xml))

and value_of_xml xml =
	match Xml.tag xml with
	| "int" -> Rpc.Int (int_of_string (get_content xml))
	| "bool" -> Rpc.Bool (bool_of_string (get_content xml))
	| "string" -> Rpc.String (get_content xml)
	| "double" -> Rpc.Double (float_of_string (get_content xml))
	| "struct" -> Rpc.Struct (List.map member_of_xml (Xml.children xml))
	| "array"  -> data_of_xml (get_child xml)
	| x -> raise (Parse_error (x, Xml.to_string xml))

and data_of_xml xml =
	match Xml.tag xml with
	| "data" -> Rpc.Array  (List.map of_xml (Xml.children xml))
	| x  -> raise (Parse_error (x, Xml.to_string xml))

and member_of_xml xml =
	match Xml.tag xml with
	| "member" ->
		begin match Xml.children xml with
		| [name; value] -> name_of_xml name, of_xml value
		| _   -> raise (Parse_error ("member_of_xml",Xml.to_string xml))
		end
	| x -> raise (Parse_error (x, Xml.to_string xml))

and name_of_xml xml =
	match Xml.tag xml with
	| "name" ->
		let data = get_child xml in
		debug "Value.name_of_xml.data(%s)" (Xml.to_string data);
		Xml.pcdata data
	| x -> raise (Parse_error (x, Xml.to_string xml))

let of_string str = of_xml (Xml.parse_string str)
