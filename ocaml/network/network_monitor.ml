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

open Network_interface

open Fun
open Stringext
open Threadext

let stats_file = "/dev/shm/network_stats"
let interval = 5. (* seconds *)
let num_retries = 2
let retry_delay = 0.5

type iface_stats = {
	tx_bytes: int64;  (* bytes emitted *)
	tx_pkts: int64;   (* packets emitted *)
	tx_errors: int64; (* error emitted *)
	rx_bytes: int64;  (* bytes received *)
	rx_pkts: int64;   (* packets received *)
	rx_errors: int64; (* error received *)
	carrier: bool;
	speed: int;
	duplex: duplex;
	pci_bus_path: string;
	vendor_id: string;
	device_id: string;
} with rpc

let default_stats = {
	tx_bytes = 0L;
	tx_pkts = 0L;
	tx_errors = 0L;
	rx_bytes = 0L;
	rx_pkts = 0L;
	rx_errors = 0L;
	carrier = false;
	speed = 0;
	duplex = Duplex_unknown;
	pci_bus_path = "";
	vendor_id = "";
	device_id = "";
}

type stats_t = (iface * iface_stats) list with rpc

exception Read_failed

let write_stats stats =
	let json = stats |> rpc_of_stats_t |> Jsonrpc.to_string in
	let checksum = json |> Digest.string |> Digest.to_hex in
	Unixext.write_string_to_file stats_file (checksum ^ "\n" ^ json)

let read_stats () =
	let rec retry n =
		try
			let file = Unixext.string_of_file stats_file in
			let index =
				try String.index file '\n'
				with Not_found -> raise Read_failed
			in
			let checksum = String.sub file 0 index in
			let json = String.sub_to_end file (index + 1) in
			if json |> Digest.string |> Digest.to_hex <> checksum then
				raise Read_failed
			else
				json |> Jsonrpc.of_string |> stats_t_of_rpc
		with Read_failed ->
			if n > 0 then begin
				Thread.delay retry_delay;
				retry (n - 1)
			end else
				raise Read_failed
	in
	retry num_retries

