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

(* This module provides an interface to certificates stored on disk.
 * Certificates are read upon [init], potentially manipulated, and
 * written back on [sync]. No other code should write these files.
 * Writing back on [sync] provides a primitive form of transactions: in
 * case of a fatal error, we could avoid writing a bad state to disk.
 * This is currently not actively used, though *)

type uuid = Uuidm.t

type paths = {host_cert_path: string; ca_certs_dir: string; ca_crls_dir: string}

type host = {
    private_key: X509.Private_key.t
  ; cert: X509.Certificate.t
  ; intermediates: X509.Certificate.t list
}

type ca = {cert: X509.Certificate.t; uuid: uuid; mutable deleted: bool}

type crl = {name: string; cert: X509.Certificate.t; mutable deleted: bool}

(* [path] value to be used normally but other paths could be used for
   testing *)
val default_paths : unit -> paths

(* initialise this module and read satte from disk *)
val init : paths -> (unit, [> Rresult.R.msg]) result

(* write back state from memory to disk *)
val sync : unit -> (unit, [> Rresult.R.msg]) result

(* remove CA cert *)
val rm_ca : uuid -> unit -> (unit, [> Rresult.R.msg]) result

(* remove crl cert *)
val rm_crl : string -> unit -> (unit, [> Rresult.R.msg]) result

(* access crl cert *)
val get_crl :
  unit -> ((string * X509.Certificate.t) list, [> Rresult.R.msg]) result

(* access CA cert *)
val get_ca :
  unit -> ((uuid * X509.Certificate.t) list, [> Rresult.R.msg]) result

(* access host cert *)
val get_host :
     unit
  -> ( (X509.Private_key.t * X509.Certificate.t * X509.Certificate.t list) option
     , [> Rresult.R.msg] )
     result
