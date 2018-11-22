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

exception Invalid_Mac of string

let check_mac mac =
  try
    if String.length mac <> 17 then failwith "mac length";
    Scanf.sscanf mac "%2x:%2x:%2x:%2x:%2x:%2x" (fun _ _ _ _ _ _ -> ());
    mac
  with _ ->
    raise (Invalid_Mac mac)

let xensource_oui = [ 0x00; 0x16; 0x3e ]

let xensource_mac () =
  let bytes = [0x00; 0x16; 0x3e] @ (List.map Random.int [0x80; 0x100; 0x100
                                                        ]) in
  String.concat ":" (List.map (Printf.sprintf "%02x") bytes)

let make_local_mac bytes =
  (* make sure bit 1 (local) is set and bit 0 (unicast) is clear *)
  bytes.(0) <- ((bytes.(0) lor 0x2) land (lnot 0x1));
  (* libvirt:virnetdevtap.c rejects MAC addresses starting with
     	   reserved value 0xfe *)
  if bytes.(0) = 0xfe then bytes.(0) <- 0xfd;
  Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
    bytes.(0) bytes.(1) bytes.(2) bytes.(3) bytes.(4) bytes.(5)

(* Generate a completely random local MAC *)
let random_local_mac () =
  make_local_mac (Array.init 6 (fun _ -> Random.int 0x100))

let hashchain_local_mac dev seed =
  let hash x = Digest.string x in
  let rec chain n f acc =
    if n = 0 then Digest.string acc
    else chain (n-1) f (f acc) in
  let hashed_seed = chain (dev*2) hash seed in
  let mac_data_1 = hashed_seed in
  let mac_data_2 = Digest.string hashed_seed in
  let take_byte n s = Char.code (String.get s n) in
  make_local_mac
    [| take_byte 0 mac_data_1;
       take_byte 1 mac_data_1;
       take_byte 2 mac_data_1;
       take_byte 3 mac_data_1;
       take_byte 1 mac_data_2;
       take_byte 2 mac_data_2; |]

