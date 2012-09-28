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

open Storage

module S = (SR_test(Lwt): SR with type 'a t = 'a Lwt.t)
module S_d = SR_server_dispatcher(S)
module SR = SR_client(S_d)


module V = (VDI_test(Lwt): VDI with type 'a t = 'a Lwt.t)
module V_d = VDI_server_dispatcher(V)
module VDI = VDI_client(V_d)

let base_path = "../../rpc-light/"

let readfile filename =
	let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o0 in
	let buffer = String.make (1024 * 1024) '\000' in
	let length = Unix.read fd buffer 0 (String.length buffer) in
	let () = Unix.close fd in
	String.sub buffer 0 length

let sr_attach_request _ =
	let xml = readfile (base_path ^ "sr.attach/request") in
	let req = Xmlrpc.call_of_string xml in
	match Storage.Types.SR.In.of_call req with
	| Xcp.Result.Ok _ -> ()
	| Xcp.Result.Error e -> raise e

let sr_attach_response _ =
	let xml = readfile (base_path ^ "sr.attach/response") in
	let resp = Xmlrpc.response_of_string xml in
	match Storage.result_of_response resp with
	| Xcp.Result.Ok x -> let (_: Storage.Types.SR.Attach.Out.t) = Storage.Types.SR.Attach.Out.t_of_rpc x in ()
	| Xcp.Result.Error e -> raise e

let _ =
	let verbose = ref false in
	Arg.parse [
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
		"Test xcp-api protocol code";

	let suite = "xen-api" >:::
		[
			"sr_attach_request" >:: sr_attach_request;
			"sr_attach_response" >:: sr_attach_response;
		] in
	run_test_tt ~verbose:!verbose suite
