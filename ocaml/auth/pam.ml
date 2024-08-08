(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

external authenticate : string -> string -> unit = "stub_XA_mh_authorize"

external change_password : string -> string -> unit = "stub_XA_mh_chpasswd"

include (
  struct
    external unsafe_crypt_r : key:string -> setting:string -> string option
      = "stub_XA_crypt_r"
  end :
    sig
      val unsafe_crypt_r : key:string -> setting:string -> string option
      [@@alert unsafe "Direct usage of this function is not recommended."]
    end
)

type crypt_algorithm = SHA256 | SHA512

type crypt_err = SaltTooLong | HashFailure

let crypt ~algo ~key ~salt =
  if String.length salt > 16 then
    Error SaltTooLong
  else
    let crypt_r = unsafe_crypt_r [@@alert "-unsafe"] in
    let algo_id = match algo with SHA256 -> 5 | SHA512 -> 6 in
    let setting = Printf.sprintf "$%d$%s$" algo_id salt in
    match crypt_r ~key ~setting with Some h -> Ok h | _ -> Error HashFailure
