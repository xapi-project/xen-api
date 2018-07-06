(*
 * Copyright (C) Citrix Systems Inc.
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

open OUnit

let ( |> ) a b = b a
let id x = x

let unbox = function
	| None -> failwith "unbox"
	| Some x -> x

let op_ids _ =
  let open Xs_protocol.Op in
  for i = 0 to 100 do (* higher than the highest ID *)
    let i' = Int32.of_int i in
    match of_int32 i' with
      | None -> ()
      | Some x -> assert (to_int32 x = i')
  done

let example_acl =
	let open Xs_protocol.ACL in
    { owner = 5; other = READ; acl = [ 2, WRITE; 3, RDWR ] }

let acl_parser _ =
  let open Xs_protocol.ACL in
  let ts = [
    { owner = 5; other = READ; acl = [ 2, WRITE; 3, RDWR ] };
    { owner = 1; other = WRITE; acl = [] };
  ] in
  let ss = List.map to_string ts in
  let ts' = List.map of_string ss in
  let printer = function
    | None -> "None"
    | Some x -> "Some " ^ to_string x in
  List.iter
    (fun (x, y) -> assert_equal ~msg:"acl" ~printer x y)
    (List.combine (List.map (fun x -> Some x) ts) ts')

let test_packet_parser choose pkt () =
    let open Xs_protocol in
    let p = ref (Parser.start ()) in
    let s = Bytes.to_string @@ to_bytes pkt in
    let i = ref 0 in
    let finished = ref false in
    while not !finished do
      match Parser.state !p with
	| Parser.Need_more_data x ->
	  let n = choose x in
	  p := Parser.input !p (String.sub s !i n);
	  i := !i + n
	| Parser.Packet pkt' ->
	  assert(get_tid pkt = (get_tid pkt'));
	  assert(get_ty pkt = (get_ty pkt'));
	  assert(get_data pkt = (get_data pkt'));
	  assert(get_rid pkt = (get_rid pkt'));
	  finished := true
	| _ ->
	  failwith (Printf.sprintf "parser failed for %s" (pkt |> get_ty |> Op.to_string))
    done


open Lwt


let test _ =
  let t = return () in
  Lwt_main.run t

type example_packet = {
	op: Xs_protocol.Op.t;
	packet: Xs_protocol.t;
	wire_fmt: string;
}
let make_example_request op payload tid wire_fmt = {
		op = op;
		packet = Xs_protocol.Request.print payload tid 0l;
		wire_fmt = wire_fmt;
	}

let example_request_packets =
	let open Xs_protocol in
	let open Xs_protocol.Request in [
		make_example_request Op.Directory (PathOp("/whatever/whenever", Directory)) 5l
			"\x01\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x13\x00\x00\x00\x2f\x77\x68\x61\x74\x65\x76\x65\x72\x2f\x77\x68\x65\x6e\x65\x76\x65\x72\x00";
		make_example_request Op.Read (PathOp("/a/b/c", Read)) 6l
			"\x02\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2f\x61\x2f\x62\x2f\x63\x00";
		make_example_request Op.Getperms (PathOp("/a/b", Getperms)) 7l
			"\x03\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x2f\x61\x2f\x62\x00";
		make_example_request Op.Rm (PathOp("/", Rm)) 0l
			"\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x2f\x00";
		make_example_request Op.Setperms (PathOp("/", Setperms example_acl)) 1l
			"\x0e\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x2f\x00\x72\x35\x00\x77\x32\x00\x62\x33\x00";
		make_example_request Op.Write (PathOp("/key", Write "value")) 1l
			"\x0b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x2f\x6b\x65\x79\x00\x76\x61\x6c\x75\x65";
		make_example_request Op.Mkdir (PathOp("/", Mkdir)) 1024l
			"\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x02\x00\x00\x00\x2f\x00";
		make_example_request Op.Transaction_start Transaction_start 0l
			"\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00";
		make_example_request Op.Transaction_end (Transaction_end true) 1l
			"\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x54\x00";
		make_example_request Op.Introduce (Introduce(4, 5n, 1)) 0l
			"\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x34\x00\x35\x00\x31\x00";
		make_example_request Op.Release (Release 2) 0l
			"\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x32\x00";
		make_example_request Op.Resume (Resume 3) 0l
			"\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
		make_example_request Op.Getdomainpath (Getdomainpath 3) 0l
			"\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
		make_example_request Op.Watch (Watch("/foo/bar", (Xs_protocol.Token.(to_string(of_string "something"))))) 0l
			"\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x2f\x66\x6f\x6f\x2f\x62\x61\x72\x00\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x00";
		make_example_request Op.Unwatch (Unwatch("/foo/bar", (Xs_protocol.Token.(to_string(of_string "somethinglse"))))) 0l
			"\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x2f\x66\x6f\x6f\x2f\x62\x61\x72\x00\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x6c\x73\x65\x00";
		make_example_request Op.Debug (Debug [ "a"; "b"; "something" ]) 0l
			"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x61\x00\x62\x00\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x00"
	]

let make_example_response op response wire_fmt =
	let request = List.find (fun x -> x.op = op) example_request_packets in
	let tid = Xs_protocol.get_tid request.packet in
	let rid = Xs_protocol.get_rid request.packet in {
		op = op;
		packet = Xs_protocol.Response.print response tid rid;
		wire_fmt = wire_fmt;
	}

(* We use the example requests to generate example responses *)
let example_response_packets =
	let open Xs_protocol in
	let open Xs_protocol.Response in [
		make_example_response Op.Read (Read "theresult")
			"\x02\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x74\x68\x65\x72\x65\x73\x75\x6c\x74";
		make_example_response Op.Read (Read "")
			"\x02\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00";
		make_example_response Op.Getperms (Getperms (Xs_protocol.ACL.( { owner = 2; other = READ; acl = [ 4, NONE ] } )))
			"\x03\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x06\x00\x00\x00\x72\x32\x00\x6e\x34\x00";
		make_example_response Op.Getdomainpath (Getdomainpath "/local/domain/4")
			"\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x2f\x6c\x6f\x63\x61\x6c\x2f\x64\x6f\x6d\x61\x69\x6e\x2f\x34\x00";
		make_example_response Op.Transaction_start (Transaction_start 3l)
			"\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
		make_example_response Op.Directory (Directory [ "a"; "b"; "c"; "aseasyas"; "1"; "2"; "3" ])
			"\x01\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x15\x00\x00\x00\x61\x00\x62\x00\x63\x00\x61\x73\x65\x61\x73\x79\x61\x73\x00\x31\x00\x32\x00\x33\x00";
		make_example_response Op.Write Write
			"\x0b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
		make_example_response Op.Mkdir Mkdir
			"\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
		make_example_response Op.Rm Rm
			"\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
		make_example_response Op.Setperms Setperms
			"\x0e\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
		make_example_response Op.Watch Watch
			"\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
		make_example_response Op.Unwatch Unwatch
			"\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
		make_example_response Op.Transaction_end Transaction_end
			"\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
		{
			op = Op.Error;
			packet = print (Error "whatyoutalkingabout") 2l 0x10l;
			wire_fmt =
				"\x10\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x77\x68\x61\x74\x79\x6f\x75\x74\x61\x6c\x6b\x69\x6e\x67\x61\x62\x6f\x75\x74\x00"
		}
	]

let example_packets = example_request_packets @ example_response_packets

let rec ints first last =
	if first > last then [] else first :: (ints (first + 1) last)

let hexstring x =
	String.concat "" ([
		"\"";
	] @ (
		List.map (fun i -> Printf.sprintf "\\x%02x" (int_of_char x.[i])) (ints 0 (String.length x - 1))
	) @ [
		"\"";
	])

(*
let error_unmarshal _ =
  let open Xs_protocol.Response in
  let enoent = 
*)
let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore protocol code";

  let packet_parsing choose =
    let f = test_packet_parser choose in
    "packet_parsing" >:::
		(List.map (fun example ->
			let description = Xs_protocol.Op.to_string example.op in
			description >:: f example.packet
		) example_packets) in
  let packet_printing =
	  "packet_printing" >:::
		  (List.map (fun example ->
			  let description = Xs_protocol.Op.to_string example.op in
			  description >:: (fun () -> assert_equal ~msg:description ~printer:hexstring example.wire_fmt (Bytes.to_string @@ Xs_protocol.to_bytes example.packet))
		  ) example_packets) in
  let suite = "xenstore" >:::
    [
      "op_ids" >:: op_ids;
      "acl_parser" >:: acl_parser;
      packet_parsing id;
      packet_parsing (fun _ -> 1);
	  packet_printing;
      "test" >:: test;
    ] in
  run_test_tt ~verbose:!verbose suite
