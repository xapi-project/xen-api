(*
 * Copyright (c) Cloud Software Group, Inc.
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

(** TLS transport for the VM-migration data connections. See the module
    implementation for the full description of the kTLS helper flow and the
    transparent stunnel fallback. *)

module Migration_tls : sig
  type t = Stunnel | Ktls

  val to_string : t -> string

  val of_string : string -> t
  (** Parse a [xenopsd.conf] [migration-tls] value; empty or unknown values
      default to [Stunnel] and never raise. *)
end

val migration_tls : Migration_tls.t ref
(** The transport selected by the [migration-tls] option in xenopsd.conf. *)

val helper_path : string ref
(** Path to the migration TLS helper binary, registered as an Xcp_service
    resource and overridable in xenopsd.conf. *)

val with_open_uri :
     ?verify_cert:Stunnel.verification_config option
  -> Uri.t
  -> (Unix.file_descr -> 'a)
  -> 'a
(** Drop-in replacement for [Open_uri.with_open_uri] on the migration paths.
    With [migration-tls = "ktls"] and an https URI, spawn the helper, receive
    the kTLS-enabled fd via SCM_RIGHTS, and pass it to [f]; on any failure to
    produce the fd, log a warning and fall back to [Open_uri.with_open_uri].
    Otherwise behaves exactly as [Open_uri.with_open_uri]. *)
