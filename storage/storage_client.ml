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

let rec retry_econnrefused f =
  try
    f ()
  with
  | Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
      (* debug "Caught ECONNREFUSED; retrying in 5s"; *)
      Thread.delay 5.;
      retry_econnrefused f
  | e ->
      (* error "Caught %s: does the storage service need restarting?" (Printexc.to_string e); *)
      raise e

module Client = Storage_interface.Client(struct
  let rpc =
    retry_econnrefused
      (fun () ->
        Xcp_client.xml_http_rpc
          ~srcstr:(Xcp_client.get_user_agent ())
          ~dststr:"storage"
        Storage_interface.uri
      )
end)
