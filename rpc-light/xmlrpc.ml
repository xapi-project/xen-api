open Printf

let debug = ref false
let debug (fmt: ('a, unit, string, unit) format4) : 'a =
	kprintf (fun s -> if !debug then begin print_string s; print_newline (); flush stdout end) fmt

(* marshalling/unmarshalling code *)
let rec buffer_add_value buf = function
	| Rpc.Int i  ->
		Buffer.add_string buf "<value><int>";
		Buffer.add_string buf (string_of_int i);
		Buffer.add_string buf "</int></value>"

	| Rpc.Bool b ->
		Buffer.add_string buf "<value><bool>";
		Buffer.add_string buf (string_of_bool b);
		Buffer.add_string buf "</bool></value>"

	| Rpc.String s ->
		Buffer.add_string buf "<value><string>";
		Buffer.add_string buf s;
		Buffer.add_string buf "</string></value>"

	| Rpc.Double d ->
		Buffer.add_string buf "<value><double>";
		Buffer.add_string buf (string_of_float d);
		Buffer.add_string buf "</double></value>"

	| Rpc.Array a ->
		Buffer.add_string buf "<value><array><data>";
		List.iter (buffer_add_value buf) a;
		Buffer.add_string buf "</data></array></value>"

	| Rpc.Struct f ->
		let buffer_add_member (name, value) =
			Buffer.add_string buf "<member><name>";
			Buffer.add_string buf name;
			Buffer.add_string buf "</name>";
			buffer_add_value buf value;
			Buffer.add_string buf "</member>"
		in
		Buffer.add_string buf "<value><struct>";
		List.iter buffer_add_member f;
		Buffer.add_string buf "</struct></value>"

let to_string x =
	let buf = Buffer.create 128 in
	buffer_add_value buf x;
	Buffer.contents buf

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
