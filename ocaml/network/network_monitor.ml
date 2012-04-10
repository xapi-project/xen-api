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

let magic = "xapistat"
let magic_bytes = 8
let checksum_bytes = 32
let length_bytes = 8

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
	nb_links: int;
	links_up: int;
	interfaces: iface list;
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
	nb_links = 0;
	links_up = 0;
	interfaces = [];
}

type stats_t = (iface * iface_stats) list with rpc

exception Read_error
exception Invalid_magic_string
exception Invalid_checksum
exception Invalid_length

let write_stats stats =
	let payload = stats |> rpc_of_stats_t |> Jsonrpc.to_string in
	let checksum = payload |> Digest.string |> Digest.to_hex in
	let length = String.length payload in
	let data = Printf.sprintf "%s%s%08x%s" magic checksum length payload in
	Unixext.write_string_to_file stats_file (data)

let read_stats () =
	let rec retry n =
		try
			let data = Unixext.string_of_file stats_file in
			if String.sub data 0 magic_bytes <> magic then
				raise Invalid_magic_string;
			let checksum = String.sub data magic_bytes checksum_bytes in
			let length =
				try int_of_string ("0x" ^ (String.sub data (magic_bytes + checksum_bytes) length_bytes))
				with _ -> raise Invalid_length
			in
			let payload = String.sub data (magic_bytes + checksum_bytes + length_bytes) length in
			if payload |> Digest.string |> Digest.to_hex <> checksum then
				raise Invalid_checksum
			else
				payload |> Jsonrpc.of_string |> stats_t_of_rpc
		with e ->
			if n > 0 then begin
				Thread.delay retry_delay;
				retry (n - 1)
			end else
				match e with
				| Invalid_magic_string | Invalid_length | Invalid_checksum -> raise e
				| _ -> raise Read_error
	in
	retry num_retries

