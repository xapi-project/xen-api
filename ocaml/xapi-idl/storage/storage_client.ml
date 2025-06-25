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

open Storage_interface

module Client = StorageAPI (Idl.Exn.GenClient (struct
  let rpc call =
    Xcp_client.(
      retry_and_switch_rpc call ~use_switch:!use_switch ~queue_name:!queue_name
        ~dststr:"storage" ~uri
    )
end))
