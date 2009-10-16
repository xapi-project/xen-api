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

type x = {
	foo: t;
	bar: string;
	gna: float list;
	f1: (int option * bool list * float list list) option;
	f2: (string * string list) array;
	f3: int32;
	f4: int64;
	f5: int;
	f6: (unit * char) list;
	progress: int array;
 } with rpc

let _ =
	let x1 = {
		foo= Foo 3;
		bar= "ha          ha";
		gna=[1.; 2.; 3.; 4. ];
		f2 = [| "hi",["hi"]; "hou",["hou";"hou"]; "foo", ["b";"a";"r"] |];
		f1 = None;
		f3 = Int32.max_int;
		f4 = Int64.max_int;
		f5 = max_int;
		f6 = [ (),'a' ; (),'b' ; (),'c'; (),'d' ; (),'e' ];
		progress = [| 0; 1; 2; 3; 4; 5 |];
	} in

	let rpc = rpc_of_x x1 in
	let xml = Xmlrpc.to_string rpc in
	let json = Jsonrpc.to_string rpc in

	Printf.printf "xmlrpc: %s\n\n" xml;
	Printf.printf "jsonrpc: %s\n\n" json;

	let callback fields value = match (fields, value) with
		| ["progress"], `Int i -> Printf.printf "Progress: %Ld\n" i
		| _                       -> ()
	in
	let x2 = x_of_rpc (Xmlrpc.of_string ~callback xml) in
	let x3 = x_of_rpc (Jsonrpc.of_string json) in

	Printf.printf "\nSanity check:\nx1=x2: %b\nx2=x3: %b\nx1=x3: %b\n\n" (x1 = x2) (x2 = x3) (x1 = x3)
	
