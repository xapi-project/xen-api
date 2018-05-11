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

type t = {
	verbose: bool;
	debug: bool;
	socket: string;
} [@@deriving rpc]

let make verbose debug socket queue =
        Xenops_interface.default_path := socket;
	begin match queue with
	| None -> ();
	| Some name ->
		Xenops_interface.queue_name := name;
		Xcp_client.use_switch := true
	end;
	{ verbose; debug; socket }

let to_string x = Jsonrpc.to_string (rpc_of_t x)

let print oc x = output_string oc (to_string x)
