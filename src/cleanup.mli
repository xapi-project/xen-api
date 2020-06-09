(*
 * Copyright (C) Citrix Inc
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

(** This module provides two independent mechanisms for cleaning up leaked
    VBDs. They both work on their own, but they can also be combined for extra
    safety. *)

val ignore_exn_log_error : string -> (unit -> unit Lwt.t) -> unit Lwt.t

module VBD : sig
  val with_vbd :
       vDI:[`VDI] API.Ref.t
    -> vM:[`VM] API.Ref.t
    -> mode:[`RO | `RW]
    -> rpc:(Rpc.call -> Rpc.response Lwt.t)
    -> session_id:[`session] API.Ref.t
    -> ([`VBD] API.Ref.t -> unit Lwt.t)
    -> unit Lwt.t
  (** This function takes care of cleaning up a VBD, and keeps a record of
      it in case something goes wrong before the cleanups can finish (for
      example the program is terminated with SIGTREM or it crashes), so
      that we have a chance of cleaning it up in the signal handler
      registered by {!Runtime.register_signal_handler}, or in the
      {!Persistent.cleanup} function.  This function will fail with an
      exception if the cleanups, the function passed to it, or
      manipulating the record of VDIs to be cleaned up fails.
  *)
end

module Block : sig
  val with_block : string -> (Block.t -> 'a Lwt.t) -> 'a Lwt.t
  (** Takes care of closing the block device, and keeps a record of it in
      case something goes wrong before the cleanups can finish (for
      example the program is terminated with SIGTREM or it crashes), so
      that we have a chance of cleaning it up in the signal handler
      registered by {!Runtime.register_signal_handler}.
  *)
end

module Runtime : sig
  val register_signal_handler : unit -> unit
  (** Register a signal handler for SIGTERM and SIGINT, to clean up the
      leftoveer VBDs that we've created but haven't yet cleaned up during
      the runtime of the program.
      If an exception happens while cleaning up a VBD, it is just logged, but
      not propagated to the caller, and will not interrupt the cleanup of the
      rest of the VBDs.
      When the cleanup is finished, the handler raises an exception to
      terminate the program.
  *)
end

module Persistent : sig
  val cleanup : unit -> unit Lwt.t
  (** Cleans up the VBDs that are recorded in the
      {!Consts.vbd_list_file_name} in the {!Consts.xapi_nbd_persistent_dir}
      directory. This function should be called at program startup, to
      ensure that the VBDs that leaked during the last run of the program
      that abnormally terminated do get cleaned up.
      If an exception happens while cleaning up a VBD, it is just logged, but
      not propagated to the caller, and will not interrupt the cleanup of the
      rest of the VBDs. However, unexpected errors that occur while
      reading the persistent list of VBDs to clean up, other than the file
      not existing, will cause this function to fail and will be
      propagated to the caller.
  *)
end
