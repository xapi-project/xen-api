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

let _ =
	let verbose = ref false in
	Arg.parse [
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
		"Test xcp-api protocol code";

	let suite = "xen-api" >:::
		[
		] in
	run_test_tt ~verbose:!verbose suite
