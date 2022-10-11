(*
 * Copyright (C) 2006-2022 Citrix Systems Inc.
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

exception InvalidSignature

val with_signed_cleartext :
  string -> (string option -> Unix.file_descr -> 'a) -> 'a

val with_detached_signature :
  string -> string -> Int64.t -> (string option -> Unix.file_descr -> 'a) -> 'a

val with_verified_signature :
  string -> string -> (string option -> Unix.file_descr -> 'a) -> 'a

val simple_checksum : string -> string

val assert_name_is_valid : name:string -> unit

module PubKeyMetaData : sig
  type t = {created: float; fingerprint: string}
end

val parse_pubkey : pubkey:string -> PubKeyMetaData.t

val create_db_record :
     __context:Context.t
  -> name:string
  -> created:float
  -> fingerprint:string
  -> _type:[< `rpm_pubkey]
  -> API.ref_Gpg_key

val parse_pubkey_metadata : string -> PubKeyMetaData.t
