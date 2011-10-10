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
open Common
open Printf
open Xenbus

module Perm =
struct
	type access = NONE | READ | WRITE | RDWR 
			
	let string_of_access = function
		| NONE -> "n"
		| READ -> "r"
		| WRITE -> "w"
		| RDWR -> "b"
			  
	let make_access () = [|NONE; READ; WRITE; RDWR|].(Random.int 4)
		
	type t =
			{ owner : int; 
			  other : access;
			  ack : (int * access) list }
				
	let string_of_ack = function
		| [] -> ""
		| ack -> String.concat "\000" (List.map (fun (i,p) -> sprintf "%s%i" (string_of_access p) i) ack) ^ "\000"
		
	let pretty_string_of_ack ack =
		String.concat "," (List.map (fun (i,p) -> sprintf "%s%i" (string_of_access p) i) ack)
			
	let make_ack n =
		let rec aux accu i =
			let accu = 
				if  Random.int 3 = 0
				then (i, make_access ()) :: accu
				else accu
			in
			if i >= n
			then accu
			else aux accu (i+1)
		in
		aux [] 0
			
	let to_string p =
		sprintf "%s%i\000%s" (string_of_access p.other) p.owner	(string_of_ack p.ack)

	let to_pretty_string p =
		sprintf "%s%i,%s" (string_of_access p.other) p.owner (pretty_string_of_ack p.ack)
			
	let make n =
		let o = Random.int (n+1) in
		{ owner = o; 
		  other = make_access ();
		  ack = make_ack (n+1) }

	let write a = 
		a = WRITE || a = RDWR

	let read a =
		a = READ || a = RDWR

	let can fn domain perm =
		perm.owner = domain ||
		if List.mem_assoc domain perm.ack
		then List.exists (fun (d,a) -> d=domain && fn a) perm.ack
		else fn perm.other


	let can_write = can write
	let can_read = can read
end
			
module Tree =
struct
	type node = 
		{ name : string;
		  perm : Perm.t }

	let node_ref = ref 0
		
	let make_name () = 
		incr node_ref;
		sprintf "n%i" (!node_ref)
			
	let make_node nb_domains = 
		{ name = make_name ();
		  perm = Perm.make nb_domains }

	type t = Node of node * t list | Leaf
			
	let rec make nb_domains nb_nodes =
		let rec make_children nb_nodes_to_create =
			match nb_nodes_to_create with
				| 0 -> []
				| 1 -> [make nb_domains 1]
				| _ ->
					  let nb_new_nodes = Random.int nb_nodes_to_create in
					  let nb_nodes_to_create = nb_nodes_to_create - nb_new_nodes in
					  make nb_domains nb_new_nodes :: make_children nb_nodes_to_create
		in
		if nb_nodes = 0
		then Leaf
		else Node (make_node nb_domains, make_children (nb_nodes-1))
			
	let iter fn tree =
		let rec aux path = function
			| Node (node, children) -> 
				  let path = sprintf "%s/%s" path node.name in
				  fn path node;
				  List.iter (aux path) children
			| Leaf -> ()
		in
		aux "" tree
end

module Xenstore =
struct
	open Xb
	open Tree

	type connection =
			{ domain: int; xb: Xb.t; mutable req_id: int }

	let make_connection n =
		{ domain = n; xb = open_xb (); req_id = 0 }

	let close_connection con =
		close_xb con.xb

	let rec read_packet con =
		let packet = pkt_recv con.xb in
		if Packet.get_rid packet <> con.req_id
		then read_packet con
		else packet

	let write_packet con =
		send_packet con.xb 0 con.req_id

	let ok = "OK"
	let invalid_perms = "EACCES"

	let process op con data =
		write_packet con op data;
		invalid_perms <> Packet.get_data (read_packet con)

	let write = process Op.Write
	let setperms = process Op.Setperms
	let rm = process Op.Rm
	let restrict = process Op.Restrict
	let read = process Op.Read

	let ignore_bool (b:bool) = ignore b

	let with_connection n f =
		let con = make_connection n in
		if n <> 0 
		then ignore_bool (restrict con (sprintf "%i\000" n));
		Pervasiveext.finally 
			(fun () -> f con)
			(fun () -> close_connection con)
		  
	let make tree =
		with_connection 0 
			(fun con -> iter
				 (fun path node -> 
					  (* printf "create node '%s' with permissions {%s}\n" path (Perm.to_pretty_string node.perm); *)
					  ignore_bool (write con (sprintf "%s\000%s" path node.name));
 					  ignore_bool (setperms con (sprintf "%s\000%s" path (Perm.to_string node.perm))))
				 tree)
			
	let clean = function
		| Leaf -> ()
		| Node (node,_) -> with_connection 0 (fun con -> ignore_bool (rm con (sprintf "/%s\000" node.name)))
end

module Test =
struct
	open Xb
	open Tree
	open Xenstore

	let align tab s =
		let n = String.length s in
		let t = String.make tab ' ' in
		String.blit s 0 t 0 n;
		t

	let print_result node dom perm wanted succeed =
		let aux b = 
			if b
			then "success"
			else "failure"
		in
		if wanted <> succeed
		then begin
			let s = 
				sprintf "<%s>, dom%i, %s: #%s (should be #%s)" node dom (Perm.to_pretty_string perm) (aux succeed) (aux wanted)
			in
			printf "%s[ERROR]\n%!" (align 80 s)
		end

	let test test_fn apply_fn n tree =
		with_connection n 
			(fun con ->
				 Tree.iter
					 (fun path node -> 
						  con.req_id <- con.req_id + 1;
						  let result = apply_fn con path in
						  print_result node.name con.domain node.perm (test_fn con.domain node.perm) result)
					 tree)

	let write = test Perm.can_write (fun con path -> Xenstore.write con (sprintf "%s\000%i" path con.req_id))
	let read = test Perm.can_read (fun con path -> Xenstore.read con (sprintf "%s\000" path))
end
	


let make nb_connections nb_nodes =
	let test_write = ref true in
	let test_read = ref true in
	let clean = ref true in

	printf "=== creating the internal tree===\n%!";
	let tree = Tree.make nb_connections nb_nodes in

	printf "\n=== populating xenstore ===\n%!";
	Xenstore.make tree;

	if !test_write
	then begin
		printf "\n=== testing write permisssions ===\n%!";
		for i = 1 to nb_connections do
			printf "testing connection %i\n%!" i;
			Test.write i tree;
		done;
	end;

	if !test_read
	then begin
		printf "\n=== testing read permisssions ===\n%!";
		for i = 1 to nb_connections do
			printf "testing connection %i\n%!" i;
			Test.read i tree;
		done;
	end;

	if !clean
	then begin
		printf "\n=== cleaning xenstore ===\n%!";
		Xenstore.clean tree
	end

let _ =
	Random.self_init ();
	make 10 500


