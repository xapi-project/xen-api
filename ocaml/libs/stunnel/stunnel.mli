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

(** Thrown if we can't find the stunnel binary in the prescribed location *)
exception Stunnel_binary_missing

exception Stunnel_error of string

exception Stunnel_verify_error of string

val crl_path : string

val timeoutidle : int option ref

type pid =
  | StdFork of int  (** we forked and exec'ed. This is the pid *)
  | FEFork of Forkhelpers.pidty  (** the forkhelpers module did it for us. *)
  | Nopid

val getpid : pid -> int

val debug_conf_of_bool : bool -> string

val debug_conf_of_env : unit -> string

type verify = VerifyPeer | CheckHost

type verification_config = {
    sni: string option
  ; verify: verify
  ; cert_bundle_path: string
}

(** Represents an active stunnel connection *)
type t = {
    mutable pid: pid
  ; fd: Safe_resources.Unixfd.t
  ; host: string
  ; port: int
  ; connected_time: float
        (** time when the connection opened, for 'early retirement' *)
  ; unique_id: int option
  ; mutable logfile: string
  ; verified: verification_config option
}

val appliance : verification_config

val pool : verification_config

val external_host : string -> verification_config

val with_connect :
     ?unique_id:int
  -> ?use_fork_exec_helper:bool
  -> ?write_to_log:(string -> unit)
  -> verify_cert:verification_config option
  -> ?extended_diagnosis:bool
  -> string
  -> int
  -> (t -> 'b)
  -> 'b
(** Connects via stunnel (optionally via an external 'fork/exec' helper) to
    a host and port.
    NOTE: this does not guarantee the connection to the remote server actually works.
    For server-side connections, use Xmlrpcclient.get_reusable_stunnel instead.
*)

val disconnect : ?wait:bool -> ?force:bool -> t -> unit
(** Disconnects from stunnel and cleans up *)

val diagnose_failure : t -> unit

val test : string -> int -> unit

val move_out_exn : t -> t

val with_moved_exn : t -> (t -> 'd) -> 'd

val safe_release : t -> unit

val with_client_proxy :
     verify_cert:verification_config option
  -> remote_host:string
  -> remote_port:int
  -> local_host:string
  -> local_port:int
  -> (unit -> 'a)
  -> 'a
