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

type query_result = {
    name: string;
    vendor: string;
    version: string;
    features: string list;
}

(** Primary key identifying the SR *)
type sr = string

(** Primary key identifying a VDI within an SR *)
type vdi = string

(** Opaque identifier used by the client to identify a particular operation *)
type task = string

(** The result of a successful VDI.attach: this information (eg) can be used to
	connect a VBD backend to a VBD frontend *)
type attach_info = {
	params : string;
	xenstore_data : (string * string) list;
}

(** Uniquely identifies the contents of a VDI *)
type content_id = string

(** The result of an operation which creates or examines a VDI *)
type vdi_info = {
    vdi: vdi;
	sr: sr;
	content_id: content_id;
    name_label: string;
    name_description: string;
    ty: string;
    (* sm_config: workaround via XenAPI *)
    metadata_of_pool: string;
    is_a_snapshot: bool;
    snapshot_time: string;
    snapshot_of: vdi;
    (* managed: workaround via XenAPI *)
    read_only: bool;
    (* missing: workaround via XenAPI *)
    virtual_size: int64;
    physical_utilisation: int64;
    (* xenstore_data: workaround via XenAPI *)
}

let string_of_vdi_info (x: vdi_info) = Jsonrpc.to_string (rpc_of_vdi_info x)

(** Each VDI is associated with one or more "attached" or "activated" "datapaths". *)
type dp = string

type stat_t = {
	superstate: Vdi_automaton.state;
	dps: (string * Vdi_automaton.state) list;
}

let string_of_stat_t (x: stat_t) = Jsonrpc.to_string (rpc_of_stat_t x)

type success_t =
	| Vdis of vdi_info list                    (** success (from SR.scan) *)
	| Vdi of vdi_info                         (** success (from VDI.create) *)
	| Attach_info of attach_info              (** success (from VDI.attach) *)
	| String of string                        (** success (from DP.diagnostics, VDI.get_url) *)
	| Unit                                    (** success *)
	| Stat of stat_t                          (** success (from VDI.stat) *)

let string_of_success (x: success_t) = Jsonrpc.to_string (rpc_of_success_t x)

type failure_t =
	| Sr_not_attached                         (** error: SR must be attached to access VDIs *)
	| Vdi_does_not_exist                      (** error: the VDI is unknown *)
	| Illegal_transition of Vdi_automaton.state * Vdi_automaton.state (** This operation implies an illegal state transition *)
	| Backend_error of string * (string list) (** error: of the form SR_BACKEND_FAILURE *)
	| Internal_err of string		          (** error: some unexpected internal error *)
	| Unimplemented                           (** error: not implemented by backend *)

let string_of_failure (x: failure_t) = Jsonrpc.to_string (rpc_of_failure_t x)

(* Represents a common "result" type. Note this is only here as a way to wrap exceptions. *)
type result =
	| Success of success_t
	| Failure of failure_t

let string_of_result (x: result) = Jsonrpc.to_string (rpc_of_result x)

let success = function
	| Success _ -> true
	| Failure _ -> false

module Driver_info = struct
    type t = {
        uri: string;
        name: string;
        description: string;
        vendor: string;
        copyright: string;
        version: string;
        required_api_version: string;
        capabilities: string list;
        configuration: (string * string) list;
    }

    type ts = t list
end

(** [query ()] returns information about this storage driver *)
external query: unit -> query_result = ""

module DP = struct
	(** Functions which create/destroy (or register/unregister) dps *)

	(** [create task id]: creates and returns a dp *)
	external create: task:task -> id:string -> dp = ""

	(** [destroy task id]: frees any resources associated with [id] and destroys it.
		This will typically do any needed VDI.detach, VDI.deactivate cleanup. *)
	external destroy: task:task -> dp:dp -> allow_leak:bool -> result = ""

		
	(** [params task id]: returns the params of the dp (the return value of VDI.attach) *)
	external attach_info: task:task -> sr:sr -> vdi:vdi -> dp:dp -> result = ""

	(** [diagnostics ()]: returns a printable set of diagnostic information,
		typically including lists of all registered datapaths and their allocated
		resources. *)
	external diagnostics: unit -> result = ""
end

module SR = struct
	(** Functions which attach/detach SRs *)

	(** [attach task sr]: attaches the SR *)
    external attach : task:task -> sr:sr -> device_config:(string * string) list -> result = ""

	(** [detach task sr]: detaches the SR, first detaching and/or deactivating any
		active VDIs. This may fail with Sr_not_attached, or any error from VDI.detach
		or VDI.deactivate. *)
    external detach : task:task -> sr:sr -> result = ""

	(** [reset task sr]: declares that the SR has been completely reset, e.g. by
		rebooting the VM hosting the SR backend. *)
	external reset : task:task -> sr:sr ->  result = ""

	(** [destroy sr]: destroys (i.e. makes unattachable and unprobeable) the [sr],
		first detaching and/or deactivating any active VDIs. This may fail with 
		Sr_not_attached, or any error from VDI.detach or VDI.deactivate. *)
	external destroy : task:task -> sr:sr -> result = ""

	(** [scan task sr] returns a list of VDIs contained within an attached SR *)
	external scan: task:task -> sr:sr -> result = ""

	(** [list task] returns the list of currently attached SRs *)
	external list: task:task -> sr list = ""
end

module VDI = struct
	(** Functions which operate on particular VDIs.
		These functions are all idempotent from the point of view of a given [dp]. *)

	(** [create task sr vdi_info params] creates a new VDI in [sr] using [vdi_info]. Some
        fields in the [vdi_info] may be modified (e.g. rounded up), so the function
        returns the vdi_info which was used. *)
	external create : task:task -> sr:sr -> vdi_info:vdi_info -> params:(string*string) list -> result = ""

	(** [snapshot task sr vdi vdi_info params] creates a new VDI which is a snapshot of [vdi] in [sr] *)
	external snapshot : task:task -> sr:sr -> vdi:vdi -> vdi_info:vdi_info -> params:(string*string) list -> result = ""

	(** [clone task sr vdi vdi_info params] creates a new VDI which is a clone of [vdi] in [sr] *)
	external clone : task:task -> sr:sr -> vdi:vdi -> vdi_info:vdi_info -> params:(string*string) list -> result = ""

    (** [destroy task sr vdi] removes [vdi] from [sr] *)
    external destroy : task:task -> sr:sr -> vdi:vdi -> result = ""

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

	(** [copy task sr vdi url sr2] copies the data from [vdi] into a remote system [url]'s [sr2] *)
	external copy : task:task -> sr:sr -> vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi -> result = ""

    (** [get_url task sr vdi] returns a URL suitable for accessing disk data directly. *)
    external get_url : task:task -> sr:sr -> vdi:vdi -> result = ""

	(** [similar_content task sr vdi] returns a list of VDIs which have similar content to [vdi] *)
	external similar_content : task:task -> sr:sr -> vdi:vdi -> result = ""

	(** [get_by_name task sr name] returns the vdi within [sr] with [name] *)
	external get_by_name : task:task -> sr:sr -> name:string -> result = ""

	(** [set_content_id task sr vdi content_id] tells the storage backend that a VDI has an updated [content_id] *)
	external set_content_id : task:task -> sr:sr -> vdi:vdi -> content_id:content_id -> result = ""

    (** [compose task sr vdi1 vdi2] layers the updates from [vdi2] onto [vdi1], modifying [vdi2] *)
    external compose : task:task -> sr:sr -> vdi1:vdi -> vdi2:vdi -> result = ""
end

(** [get_by_name task name] returns a vdi with [name] (which may be in any SR) *)
external get_by_name : task:task -> name:string -> result = ""

type mirror_receive_result_vhd_t = {
	mirror_vdi : vdi_info;
	mirror_datapath : dp;
	copy_diffs_from : content_id option;
	copy_diffs_to : vdi;
}
		
type mirror_receive_result = 
	| Vhd_mirror of mirror_receive_result_vhd_t

type similars = content_id list 

module Mirror = struct

	(** [start task sr vdi url sr2] creates a VDI in remote [url]'s [sr2] and writes
		data synchronously. It returns the id of the VDI.*)
	external start : task:task -> sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> result = ""

	(** [stop task sr vdi] stops mirroring local [vdi] *)
	external stop : task:task -> sr:sr -> vdi:vdi -> result = ""

	external active : task:task -> sr:sr -> content_id list = ""

	(** Called on the receiving end *)
	external receive_start : task:task -> sr:sr -> vdi_info:vdi_info -> content_id:content_id -> similar:similars -> mirror_receive_result = ""

	external receive_finalize : task:task -> sr:sr -> content_id:content_id -> unit = ""

	external receive_cancel : task:task -> sr:sr -> content_id:content_id -> unit = ""

end
