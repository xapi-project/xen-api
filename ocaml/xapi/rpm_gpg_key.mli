(*
 * Copyright (C) 2022 Citrix Systems Inc.
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

val install_rpmgpgkey :
  name:string -> pubkey:string -> fingerprint:string -> unit

val uninstall_rpmgpgkey : name:string -> fingerprint:string -> unit

val get_pubkey : name:string -> string option

val get_rpm_pubkey_string :
  __context:Context.t -> self:API.ref_Gpg_key -> string

val extract_pubkey_from_rpm : name:string -> string -> string option
