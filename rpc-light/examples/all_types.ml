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

type t = Foo of int | Bar of (int * float) with rpc

module M = struct
	type m = t with rpc
end

type 'a x = {
	foo: M.m;
	bar: string;
	gna: float list;
	f1: (int option * bool list * float list list) option;
	f2: (string * string list) array;
	f3: int32;
	f4: int64;
	f5: int;
	f6: (unit * char) list;
	f7: 'a list;
	f8: (string, t) Hashtbl.t;
	progress: int array;
 } with rpc

let _ =
	let x = {
		foo= Foo 3;
		bar= "ha          ha";
		gna=[1.; 2.; 3.; 4. ];
		f2 = [| "hi",["hi"]; "hou",["hou";"hou"]; "foo", ["b";"a";"r"] |];
		f1 = Some (None, [true], [[1.]; [2.;3.]]);
		f3 = Int32.max_int;
		f4 = Int64.max_int;
		f5 = max_int;
		f6 = [ (),'a' ; (),'b' ; (),'c'; (),'d' ; (),'e' ];
		f7 = [ Foo 1; Foo 2; Foo 3 ];
		f8 = Hashtbl.create 0;
		progress = [| 0; 1; 2; 3; 4; 5 |];
	} in

	Hashtbl.add x.f8 "hello" (Foo 5);
	Hashtbl.add x.f8 "there" (Bar (5,0.5));

	(* Testing basic marshalling/unmarshalling *)
	
	let rpc = rpc_of_x M.rpc_of_m x in

	let rpc_xml = Xmlrpc.to_string rpc in
	let rpc_json = Jsonrpc.to_string rpc in

	Printf.printf "\n==rpc_xml==\n%s\n" rpc_xml;
	Printf.printf "\n==json==\n%s\n" rpc_json;

	let callback fields value = match (fields, value) with
		| ["progress"], Rpc.Int i -> Printf.printf "Progress: %Ld\n" i
		| _                       -> ()
	in
	let x_xml = x_of_rpc M.m_of_rpc (Xmlrpc.of_string ~callback rpc_xml) in
	let x_json = x_of_rpc M.m_of_rpc (Jsonrpc.of_string rpc_json) in

	Printf.printf "\n==Sanity check 1==\nx=x_xml: %b\nx=x_json: %b\n" (x = x_xml) (x = x_json);
	(*assert (x = x_xml && x = x_json);*)
	
	(* Testing calls and responses *)
	
	let call = Rpc.call "foo" [ rpc; Rpc.String "Mouhahahaaaaa" ] in
	let success = Rpc.success rpc in
	let failure = Rpc.failure rpc in

	let c_xml_str = Xmlrpc.string_of_call call in
	let s_xml_str = Xmlrpc.string_of_response success in
	let f_xml_str = Xmlrpc.string_of_response failure in

	let c_json_str = Jsonrpc.string_of_call call in
	let s_json_str = Jsonrpc.string_of_response success in
	let f_json_str = Jsonrpc.string_of_response failure in

	Printf.printf "\n==call==\n %s\n%s\n" c_xml_str c_json_str;
	Printf.printf "\n==success==\n %s\n%s\n" s_xml_str s_json_str;
	Printf.printf "\n==failure==\n %s\n%s\n" f_xml_str f_json_str;

	let c_xml = Xmlrpc.call_of_string c_xml_str in
	let s_xml = Xmlrpc.response_of_string s_xml_str in
	let f_xml = Xmlrpc.response_of_string f_xml_str in

	(* Printf.printf "\n==Sanity check 2==\ncall=c_xml: %b\nsuccess=s_xml: %b\nfailure=f_xml: %b\n"
		(call = c_xml) (success = s_xml) (failure = f_xml);
	assert (call = c_xml && success = s_xml && failure = f_xml); *)

	let c_json = Jsonrpc.call_of_string c_json_str in
	let s_json = Jsonrpc.response_of_string s_json_str in
	let f_json = Jsonrpc.response_of_string f_json_str in

	Printf.printf "\n==Sanity check 3==\ncall=c_json': %b\nsuccess=s_json': %b\nfailure=f_json': %b\n"
		(call = c_json) (success = s_json) (failure = f_json);
	assert (call = c_json && success = s_json && failure = f_json);

	let print_hashtbl h =
	  Hashtbl.iter (fun k v -> Printf.printf "key=%s v=%s\n" k (match v with | Foo x -> Printf.sprintf "Foo (%d)" x | Bar (x,y) -> Printf.sprintf "Bar (%d,%f)" x y)) h
	in

	Printf.printf "Original hashtbl:\n";
	print_hashtbl x.f8;
	Printf.printf "Testing xml Hashtbl representation:\n";
	print_hashtbl x_xml.f8;
	Printf.printf "Testing json Hashtbl representation:\n";
	print_hashtbl x_json.f8

