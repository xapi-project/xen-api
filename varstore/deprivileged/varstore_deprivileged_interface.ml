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

open Rpc
open Idl

let err = Xenops_interface.err

type nvram = (string * string) list [@@deriving rpcty]

module RPC_API (R : RPC) = struct
  open R

  (* A restricted interface to VM's NVRAM, varstored is sandboxed and has no direct access to XAPI *)
  let description =
    let open Interface in
    { name = "Xenopsd_varstored"
    ; namespace = None
    ; description =
        ["This interface is used by varstored to set VM NVRAM and send alerts"]
    ; version = 0, 1, 0 }

  let implementation = implement description
  let unit_p = Param.mk ~name:"unit" Types.unit
  let nvram_p = Param.mk ~name:"NVRAM" nvram
  let string_p = Param.mk Types.string
  let int64_p = Param.mk Types.int64

  (* The APIs here should be wire-compatible with the ones in XAPI,
   * but ignore the parameters that varstored is not allowed to override,
   * e.g. the VM ref or session id.
   * The parameters cannot be named *)

  let session_login =
    declare
      "session.login_with_password"
      ["Dummy, for wire compatibility with XAPI"]
      (string_p @-> string_p @-> string_p @-> string_p @-> returning string_p err)

  let session_logout =
    declare
      "session.logout"
      ["Dummy, for wire compatibility with XAPI"]
      (string_p @-> returning unit_p err)

  let get_by_uuid =
    declare
      "VM.get_by_uuid"
      ["Dummy, for wire compatibility with XAPI"]
      (string_p @-> string_p @-> returning string_p err)

  let get_NVRAM =
    declare
      "VM.get_NVRAM"
      ["Get the current VM's NVRAM contents"]
      (string_p @-> string_p @-> returning nvram_p err)

  let set_NVRAM =
    declare
      "VM.set_NVRAM_EFI_variables"
      ["Set the current VM's NVRAM contents"]
      (string_p @-> string_p @-> string_p @-> returning unit_p err)

  let message_create =
    declare
      "message.create"
      ["Send an alert when booting a UEFI guest fails"]
      ( string_p
      @-> string_p
      @-> int64_p
      @-> string_p
      @-> string_p
      @-> string_p
      @-> returning unit_p err )
end
