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

(** Varstored is deprivileged and should not have full access to XAPI.
 ** This interface provides a way to spawn a new listening socket
 ** restricted to a small number of API calls targeting only 1 VM.
 ** Xenopsd is a client of this interface and calls it through message-switch.
 ** A new privileged daemon (varstored-socket-deprivd) implements the interface
 ** and spawns the listening sockets.
 ***)

open Rpc
open Idl

let service_name = "xapi_depriv"
let queue_name = Xcp_service.common_prefix ^ service_name

type error = InternalError of string [@@deriving rpcty]

module E = Error.Make (struct
  type t = error [@@deriving rpcty]

  let internal_error_of e = Some (InternalError (Printexc.to_string e))
end)

module Uuidm = struct
  include Uuidm

  (** Validate UUIDs by converting them to Uuidm.t in the API *)
  let typ_of =
    Rpc.Types.Abstract
      { aname = "uuid"
      ; test_data = [Uuidm.v `V4]
      ; rpc_of = (fun t -> Rpc.String (Uuidm.to_string t))
      ; of_rpc =
          (function
          | Rpc.String s ->
            (match Uuidm.of_string s with
            | Some uuid -> Ok uuid
            | None ->
              Error (`Msg (Printf.sprintf "typ_of_vm_uuid: not a valid UUID: %s" s)))
          | r ->
            Error
              (`Msg
                (Printf.sprintf
                   "typ_of_vm_uuid: expected rpc string but got %s"
                   (Rpc.to_string r)))) }
end

type vm_uuid = Uuidm.t [@@deriving rpcty]

module RPC_API (R : RPC) = struct
  open R

  let description =
    Interface.
      { name = "Depriv"
      ; namespace = None
      ; description =
          ["Interface for creating a deprivileged XAPI socket for a specific VM."]
      ; version = 1, 0, 0 }

  let implementation = implement description

  type debug_info = string
  [@@doc ["An uninterpreted string associated with the operation."]] [@@deriving rpcty]

  let debug_info_p =
    Param.mk
      ~name:"dbg"
      ~description:["An uninterpreted string to associate with the operation."]
      debug_info

  let err = E.error
  let unit_p = Param.mk Types.unit

  let path_p =
    Param.mk ~name:"path" ~description:["Unix domain socket path"] Types.string

  (** each VM gets its own group id = qemu_base + domid *)
  let gid_p = Param.mk ~name:"gid" ~description:["socket group id"] Types.int

  let create =
    let vm_uuid_p = Param.mk ~name:"vm_uuid" ~description:["VM UUID"] vm_uuid in
    declare
      "create"
      [ "Create a deprivileged socket that only accepts API calls for a"
      ; "specific VM. The socket will be writable only to the specified group." ]
      (debug_info_p @-> vm_uuid_p @-> gid_p @-> path_p @-> returning unit_p err)

  let destroy =
    declare
      "destroy"
      ["Stop listening on sockets for the specified group"]
      (debug_info_p @-> gid_p @-> path_p @-> returning unit_p err)
end
