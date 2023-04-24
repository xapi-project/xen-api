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


let nvram_uefi_of_vm vm =
  let open Xenops_types.Nvram_uefi_variables in
  let on_field name f t =
    match List.assoc name vm.API.vM_NVRAM with
    | v ->
        f v t
    | exception Not_found ->
        t
  in
  let add_on_boot =
    on_field "EFI-variables-on-boot" (fun str t ->
        match str with
        | "persist" ->
            {t with on_boot= Persist}
        | "reset" ->
            {t with on_boot= Reset}
        | bad ->
            raise
              Api_errors.(
                Server_error
                  (invalid_value, ["NVRAM['EFI-variables-on-boot']"; bad])
              )
    )
  in
  let add_backend =
    on_field "EFI-variables-backend" (fun backend t -> {t with backend})
  in
  default_t |> add_on_boot |> add_backend

let firmware_of_vm vm =
  let open Xenops_types.Vm in
  match List.assoc "firmware" vm.API.vM_HVM_boot_params with
  | "bios" ->
      Bios
  | "uefi" ->
      Uefi (nvram_uefi_of_vm vm)
  | bad ->
      raise
        Api_errors.(
          Server_error (invalid_value, ["HVM-boot-params['firmware']"; bad])
        )
  | exception Not_found ->
      default_firmware

