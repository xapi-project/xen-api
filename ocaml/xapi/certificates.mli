(*
   Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

type t_trusted =
  | Root_legacy
  | CRL
  | Root of API.certificate_purpose list
  | Pinned of API.certificate_purpose list

type category = [`Root_legacy | `CRL | `Root | `Pinned]

(* Information extraction *)

val pem_of_string : string -> X509.Certificate.t

val pp_hash : string -> string

val pp_fingerprint : hash_type:Digestif.hash' -> X509.Certificate.t -> string

val validate_name : t_trusted -> string -> unit

val hostnames_of_pem_cert :
  string -> (X509.Host.Set.t, [> Rresult.R.msg]) result

val local_list : t_trusted -> string list

val get_server_certificate : unit -> string

val get_internal_server_certificate : unit -> string

(* Keeping CA roots updated in the filesystem *)

val update_all_bundles : unit -> unit

val local_sync : unit -> unit

(* Certificate installation to filesystem *)

val install_server_certificate :
     pem_chain:string option
  -> pem_leaf:string
  -> pkcs8_private_key:string
  -> path:string
  -> X509.Certificate.t

val host_install : t_trusted -> name:string -> cert:string -> unit

val host_uninstall : t_trusted -> name:string -> force:bool -> unit

val name_of_uuid : string -> string

val all_purposes : API.certificate_purpose list list

type trusted_store = {cert_dir: string; bundle: (string * string) option}

val trusted_store_locations : t_trusted -> trusted_store list

val sync_all_hosts : __context:Context.t -> API.ref_host list -> unit

val db_type_of_category : [`Root | `Pinned] -> [`ca | `pinned]

val cleanup_all_trusted : unit -> unit

(* Database manipulation *)

module Db_util : sig
  module PurposeSet : Set.S with type elt = API.certificate_purpose

  val add_cert :
       __context:Context.t
    -> type':
         [< `ca of string
         | `host of API.ref_host
         | `host_internal of API.ref_host
         | `pinned ]
    -> purpose:API.certificate_purpose list
    -> X509.Certificate.t
    -> API.ref_Certificate * string

  val remove_cert_by_ref : __context:Context.t -> API.ref_Certificate -> unit

  val remove_ca_cert_by_name : __context:Context.t -> string -> unit

  val get_host_certs :
       __context:Context.t
    -> type':[< `host | `host_internal]
    -> host:API.ref_host
    -> API.ref_Certificate list

  val get_ca_certs : __context:Context.t -> API.ref_Certificate list

  val get_trusted_certs :
       __context:Context.t
    -> [`ca | `pinned]
    -> (API.ref_Certificate * API.certificate_t) list
end
