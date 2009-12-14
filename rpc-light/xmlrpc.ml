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

open Printf

let debug = ref false
let debug (fmt: ('a, unit, string, unit) format4) : 'a =
	kprintf (fun s -> if !debug then begin print_string s; print_newline (); flush stdout end) fmt

(* marshalling/unmarshalling code *)

(* The XML-RPC is not very clear about what characters can be in a string value ... *)
let check s =
	let aux c =
		let code = int_of_char c in
		if code <= 31 then
			failwith (sprintf "%s is not a valid string (it contains char '\\%i')" s code) 
	in
	for i = 0 to String.length s - 1 do aux s.[i] done;
	s

let rec add_value f = function
	| `Int i  ->
		f "<value><i4>";
		f (Int64.to_string i);
		f "</i4></value>"

	| `Bool b ->
		f "<value><bool>";
		f (string_of_bool b);
		f "</bool></value>"

	| `Float d ->
		f "<value><double>";
		f (string_of_float d);
		f "</double></value>"

	| `String s ->
		f "<value><string>";
		f (check s);
		f "</string></value>"

	| `List a ->
		f "<value><array><data>";
		List.iter (add_value f) a;
		f "</data></array></value>"

	| `Dict s ->
		let add_member (name, value) =
			f "<member><name>";
			f name;
			f "</name>";
			add_value f value;
			f "</member>"
		in
		f "<value><struct>";
		List.iter add_member s;
		f "</struct></value>"

	| `None ->
		  f "<value><string>nil</string></value>"

let to_string x =
	let buf = Buffer.create 128 in
	add_value (Buffer.add_string buf) x;
	Buffer.contents buf

let string_of_call call =
	let module B = Buffer in
	let buf = B.create 1024 in
	let add = B.add_string buf in
	add "<?xml version=\"1.0\"?>";
	add "<methodCall><methodName>";
	add (check call.Rpc.name);
	add "</methodName><params>";
	List.iter (fun p ->
		add "<param>";
		add (to_string p);
		add "</param>"
		) call.Rpc.params;
	add "</params></methodCall>";
	B.contents buf

let string_of_response response =
	let module B = Buffer in
	let buf = B.create 256 in
	let add = B.add_string buf in
	begin match response with
	| Rpc.Success v   ->
		add "<?xml version=\"1.0\"?><methodResponse><params><param>";
		add (to_string v);
		add "</param></params></methodResponse>";
	| Rpc.Fault (i,s) ->
		add "<?xml version=\"1.0\"?><methodResponse><fault><value><struct><member><name>faultCode</name><value><int>";
		add (Int64.to_string i);
		add "</int></value></member><member><name>faultString</name><value><string>";
		add s;
		add "</string></value></member></struct></value></fault></methodResponse>";
	end;
	B.contents buf

exception Parse_error of string * Xmlm.signal * Xmlm.input

let debug_signal = function
	| `El_start ((_,tag),_) -> Printf.sprintf "<%s>" tag
	| `El_end               -> "</...>"
	| `Data d               -> Printf.sprintf "%s" d
	| `Dtd _                -> "<?dtd?>"

let debug_input input =
	let buf = Buffer.create 1024 in
	let rec aux tags =
		if not (Xmlm.eoi input) then begin
			match Xmlm.input input with
			| `El_start ((_,tag),_) ->
				Buffer.add_string buf "<";
				Buffer.add_string buf tag;
				Buffer.add_string buf ">";
				aux (tag :: tags)
			| `El_end ->
				begin match tags with
				| []     ->
					Buffer.add_string buf "</>";
					aux tags
				| h :: t ->
					Buffer.add_string buf "</";
					Buffer.add_string buf h;
					Buffer.add_string buf ">";
					aux t
				end
			| `Data d ->
				Buffer.add_string buf d;
				aux tags
			| `Dtd _ ->
				aux tags end
	in
	aux [];
	Buffer.contents buf

let parse_error n s i =
	Printf.eprintf "Error: got '%s' while '%s' was expected when processing '%s'\n" (debug_signal s) n (debug_input i);
	raise (Parse_error (n,s,i))

module Parser = struct

	(* Specific helpers *)
	let get_data input =
		match Xmlm.input input with
		| `Data d -> d
		| e       -> parse_error "..." e input

	let open_tag input =
		match Xmlm.input input with
		| `El_start ((_,tag),_) -> tag
		| e                     -> parse_error "<...>" e input

	let close_tag input =
		match Xmlm.input input with
		| `El_end -> ()
		| e       -> parse_error "</...>" e input

	let map_tags f input =
		let tag = open_tag input in
		let r = f input tag in
		close_tag input;
		r

	let map_tag tag f input =
		let t = open_tag input in
		if t = tag then begin
			let r = f input in
			close_tag input;
			r
		end else
			parse_error (Printf.sprintf "<%s>" tag) (`El_start (("",t),[])) input

	let name   input   = map_tag "name" get_data input
	let data   f input = map_tag "data" f input
	let value  f input = map_tag "value" f input
	let members f input =
		let g input =
			let name  = name input in
			let value = f name input in
			(name, value) in
		let r = ref [] in
		while Xmlm.peek input <> `El_end do
			r := map_tag "member" g input :: !r
		done;
		List.rev !r


	(* Basic constructors *)
	let make_int ?callback accu data : Rpc.Val.t =
		let r = `Int (Int64.of_string data) in
		match callback with
		| Some f -> f (List.rev accu) r; r
		| None   -> r

	let make_bool ?callback accu data : Rpc.Val.t =
		let r = `Bool (bool_of_string data) in
		match callback with
		| Some f -> f (List.rev accu) r; r
		| None   -> r

	let make_double ?callback accu data : Rpc.Val.t =
		let r = `Float (float_of_string data) in
		match callback with
		| Some f -> f (List.rev accu) r; r
		| None   -> r

	let make_string ?callback accu data : Rpc.Val.t =
		let r = match data with
			| "nil" -> `None
			| s     -> `String s in
		match callback with
		| Some f -> f (List.rev accu) r; r
		| None   -> r

	let make_array ?callback accu data : Rpc.Val.t =
		let r = `List data in
		match callback with
		| Some f -> f (List.rev accu) r; r
		| None   -> r

	let make_struct ?callback accu data : Rpc.Val.t =
		let r = `Dict data in
		match callback with
		| Some f -> f (List.rev accu) r; r
		| None   -> r

	(* General parser functions *)
	let rec of_xml ?callback accu input =
		try value (map_tags (basic_types ?callback accu)) input
		with Xmlm.Error ((a,b), e) ->
			Printf.eprintf "Characters %i--%i: %s\n%!" a b (Xmlm.error_message e);
			exit (-1)
			| e -> Printf.eprintf "%s\n%!" (Printexc.to_string e); exit (-1)

	and basic_types ?callback accu input = function
		| "int" | "i4" -> make_int    ?callback accu (get_data input)
		| "bool"       -> make_bool   ?callback accu (get_data input)
		| "double"     -> make_double ?callback accu (get_data input)
		| "string"     -> make_string ?callback accu (get_data input)
		| "array"      -> make_array  ?callback accu (data (of_xmls ?callback accu) input)
		| "struct"     -> make_struct ?callback accu (members (fun name -> of_xml ?callback (name::accu)) input)
		| e            -> make_string ?callback accu e

	and of_xmls ?callback accu input =
		let r = ref [] in
		while Xmlm.peek input <> `El_end do
			r := of_xml ?callback accu input :: !r
		done;
		List.rev !r
end

let of_string ?callback str : Rpc.Val.t =
	let input = Xmlm.make_input (`String (0, str)) in
	begin match Xmlm.peek input with
	| `Dtd _ -> ignore (Xmlm.input input)
	| _      -> () end;
	Parser.of_xml ?callback [] input
	
let call_of_string ?callback str : Rpc.call =
	let input = Xmlm.make_input (`String (0, str)) in
	begin match Xmlm.peek input with
	| `Dtd _ -> ignore (Xmlm.input input)
	| _      -> () end;
	let name = ref "" in
	let params = ref [] in
	Parser.map_tag "methodCall" (fun input ->
		name := Parser.map_tag "methodName" Parser.get_data input;
		Parser.map_tag "params" (fun input ->
			while Xmlm.peek input <> `El_end do
				Parser.map_tag "param" (fun input -> params := (Parser.of_xml ?callback [] input) :: !params) input
			done;
			) input
		) input;
	{ Rpc.name = !name; Rpc.params = !params }
	
let response_of_string ?callback str : Rpc.response =
	let input = Xmlm.make_input (`String (0, str)) in
	begin match Xmlm.peek input with
	| `Dtd _ -> ignore (Xmlm.input input)
	| _      -> () end;
	Parser.map_tag "methodResponse" (fun input ->
		match Xmlm.peek input with
		| `El_start ((_,"fault"),_) ->
			Parser.map_tag "fault" (fun input ->
				let signal = Xmlm.peek input in
				match Parser.of_xml ?callback [] input with
					| `Dict [ "faultCode", `Int i; "faultString", `String s ] -> Rpc.Fault (i, s)
					| s -> parse_error (to_string s) signal input
				) input
		| `El_start ((_,"params"),_) ->
			Parser.map_tag "params" (fun input ->
				Parser.map_tag "param" (fun input -> Rpc.Success (Parser.of_xml ?callback [] input)) input
				) input
		| s -> parse_error "response" s input
		) input 

	
