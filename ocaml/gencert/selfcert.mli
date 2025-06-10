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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

val fist_path : string
(** Path to a file; if this file exists, newly created certificates will
be backdated to 2008-07-01 and any [valid_from] ignored *)

val write_certs : string -> int -> string -> (unit, [> Rresult.R.msg]) result
(** [write_certs path pkcs12] writes [pkcs12] to [path] atomically.
[pkcs12] should contain a components of a PKCS12 Certificate *)

val host :
     name:string
  -> dns_names:string list
  -> ips:string list
  -> ?valid_from:Ptime.t (* default: now *)
  -> valid_for_days:int
  -> string
  -> int
  -> X509.Certificate.t
(** [host name dns_names ip path] creates (atomically) a PEM file at
    [path] with [name] as CN, and the following SANs: [dns_names] + [ip] *)

val xapi_pool :
     ?valid_from:Ptime.t (* default: now *)
  -> valid_for_days:int
  -> uuid:string
  -> string
  -> int
  -> X509.Certificate.t
(** [xapi_pool uuid path] creates (atomically) a PEM file at [path] with
    [uuid] as CN *)
