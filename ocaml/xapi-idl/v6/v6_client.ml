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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU Lesser General Public License for more details.
 *)

open V6_interface
open Xcp_client

let json_url () = "file:" ^ json_path

let xml_url () = "file:" ^ xml_path

let known_present = ref false

let ensure_queue_present () =
  if !known_present then ()
  else
  let t = get_ok @@ Message_switch_unix.Protocol_unix.Client.connect ~switch:!Xcp_client.switch_path () in
  match Message_switch_unix.Protocol_unix.Client.list ~t ~prefix:!queue_name ~filter:`Alive () with
  | Ok lst when List.mem !queue_name lst -> known_present := true
  | _ ->
      D.warn "Cannot find v6 queue %s in message_switch" !queue_name;
      raise (V6_error V6d_failure)

let rpc call =
  if !use_switch then begin
    ensure_queue_present ();
    json_switch_rpc !queue_name call
  end
  else
    xml_http_rpc ~srcstr:"xapi" ~dststr:"v6d" xml_url call

module Client = V6_interface.RPC_API (Idl.Exn.GenClient (struct
  let rpc = rpc
end))

include Client
