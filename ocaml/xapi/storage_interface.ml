(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(**
 * @group Storage
 *)

open Vdi_automaton

(** Primary key identifying the SR *)
type sr = string

(** Primary key identifying a VDI within an SR *)
type vdi = string

(** Opaque identifier used by the client to identify a particular operation *)
type task = string

(** The result of a successful VDI.attach: this information (eg) can be used to
	connect a VBD backend to a VBD frontend *)
type params = string

(** Each VDI is associated with one or more "attached" or "activated" "datapaths". *)
type dp = string

type stat_t = {
	superstate: Vdi_automaton.state;
	dps: (string * Vdi_automaton.state) list;
}

let string_of_stat_t x = Printf.sprintf "{ superstate = %s; dps = [ %s ] }"
	(Vdi_automaton.string_of_state x.superstate)
	(String.concat "; " (List.map (fun (name, state) -> Printf.sprintf "%s, %s" name (Vdi_automaton.string_of_state state)) x.dps))

type success_t =
	| Vdi of params                  (** success (from VDI.attach) *)
	| Unit                                    (** success *)
	| Stat of stat_t                          (** success (from VDI.stat) *)

type failure_t =
	| Sr_not_attached                         (** error: SR must be attached to access VDIs *)
	| Illegal_transition of Vdi_automaton.state * Vdi_automaton.state (** This operation implies an illegal state transition *)
	| Backend_error of string * (string list) (** error: of the form SR_BACKEND_FAILURE *)
	| Internal_error of string		          (** error: some unexpected internal error *)

(* Represents a common "result" type. Note this is only here as a way to wrap exceptions. *)
type result =
	| Success of success_t
	| Failure of failure_t

let string_of_success = function
	| Vdi x -> "VDI " ^ x
	| Unit -> "()"
	| Stat x -> string_of_stat_t x

let string_of_failure = function
	| Sr_not_attached -> "Sr_not_attached"
	| Illegal_transition(a, b) -> Printf.sprintf "Illegal VDI transition: %s -> %s" (Vdi_automaton.string_of_state a) (Vdi_automaton.string_of_state b)
	| Backend_error (code, params) -> Printf.sprintf "Backend_error (%s; [ %s ])" code (String.concat ";" params)
	| Internal_error x -> "Internal_error " ^ x

let string_of_result = function
	| Success s -> "Success: " ^ (string_of_success s)
	| Failure f -> "Failure: " ^ (string_of_failure f)

let success = function
	| Success _ -> true
	| Failure _ -> false

module DP = struct
	(** Functions which create/destroy (or register/unregister) dps *)

	(** [create task id]: creates and returns a dp *)
	external create: task:task -> id:string -> dp = ""

	(** [destroy task id]: frees any resources associated with [id] and destroys it.
		This will typically do any needed VDI.detach, VDI.deactivate cleanup. *)
	external destroy: task:task -> dp:dp -> allow_leak:bool -> result = ""

	(** [diagnostics ()]: returns a printable set of diagnostic information,
		typically including lists of all registered datapaths and their allocated
		resources. *)
	external diagnostics: unit -> string = ""
end

module SR = struct
	(** Functions which attach/detach SRs *)

	(** [attach task sr]: attaches the SR *)
    external attach : task:task -> sr:sr -> result = ""

	(** [detach task sr]: detaches the SR, first detaching and/or deactivating any
		active VDIs. This may fail with Sr_not_attached, or any error from VDI.detach
		or VDI.deactivate. *)
    external detach : task:task -> sr:sr -> result = ""

	(** [destroy sr]: destroys (i.e. makes unattachable and unprobeable) the [sr],
		first detaching and/or deactivating any active VDIs. This may fail with 
		Sr_not_attached, or any error from VDI.detach or VDI.deactivate. *)
	external destroy : task:task -> sr:sr -> result = ""

	(** [list task] returns the list of currently attached SRs *)
	external list: task:task -> sr list = ""
end

module VDI = struct
	(** Functions which operate on particular VDIs.
		These functions are all idempotent from the point of view of a given [dp]. *)

	(** [attach task dp sr vdi read_write] returns the [params] for a given
		[vdi] in [sr] which can be written to if (but not necessarily only if) [read_write]
		is true *)
	external attach : task:task -> dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> result = ""

	(** [activate task dp sr vdi] signals the desire to immediately use [vdi].
		This client must have called [attach] on the [vdi] first. *)
    external activate : task:task -> dp:dp -> sr:sr -> vdi:vdi -> result = ""

	(** [stat task sr vdi ()] returns the state of the given VDI from the point of view of
        each dp as well as the overall superstate. *)
	external stat: task:task -> sr:sr -> vdi:vdi -> unit -> result = ""

	(** [deactivate task dp sr vdi] signals that this client has stopped reading (and writing)
		[vdi]. *)
    external deactivate : task:task -> dp:dp -> sr:sr -> vdi:vdi -> result = ""

	(** [detach task dp sr vdi] signals that this client no-longer needs the [params]
		to be valid. *)
    external detach : task:task -> dp:dp -> sr:sr -> vdi:vdi -> result = ""
end
