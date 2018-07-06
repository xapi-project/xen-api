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

(* This a temporary measure to allow rrdd to read networkd's stats;
 * ultimately networkd should export its stats to rrdd via rrdd's
 * plugin interface. *)

open Network_interface

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
} [@@deriving rpcty]

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

type stats_t = (iface * iface_stats) list [@@deriving rpcty]

exception Read_error
exception Invalid_magic_string
exception Invalid_checksum
exception Invalid_length

(* Shamelessly stolen from Unixext. *)
module File_helpers = struct
  (** open a file, and make sure the close is always done *)
  let with_file file mode perms f =
    let fd = Unix.openfile file mode perms in
    let r =
      try f fd
      with exn -> Unix.close fd; raise exn
    in
    Unix.close fd;
    r

  (** [fd_blocks_fold block_size f start fd] folds [f] over blocks (strings)
      from the fd [fd] with initial value [start] *)
  let fd_blocks_fold block_size f start fd = 
    let block = Bytes.create block_size in
    let rec fold acc = 
      let n = Unix.read fd block 0 block_size in
      (* Consider making the interface explicitly use Substrings *)
      let s = if n = block_size then (Bytes.to_string block) else Bytes.sub_string block 0 n in
      if n = 0 then acc else fold (f acc s) in
    fold start

  let buffer_of_fd fd = 
    fd_blocks_fold 1024 (fun b s -> Buffer.add_string b s; b) (Buffer.create 1024) fd

  let buffer_of_file file_path = with_file file_path [ Unix.O_RDONLY ] 0 buffer_of_fd

  let string_of_file file_path = Buffer.contents (buffer_of_file file_path)
end

let read_stats () =
  let rec retry n =
    try
      let data = File_helpers.string_of_file stats_file in
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
        match payload |> Jsonrpc.of_string |> Rpcmarshal.unmarshal typ_of_stats_t with
        | Result.Ok v -> v
        | Result.Error _ -> raise Read_error
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

