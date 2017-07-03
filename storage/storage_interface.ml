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

let service_name="storage"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)

let set_sockets_dir x =
        default_path := Filename.concat x service_name

let uri () = "file:" ^ !default_path


(** Primary key identifying the SR *)
type sr = string

(** Primary key identifying a VDI within an SR *)
type vdi = string

(** Opaque identifier used by the client to identify a particular operation *)
type debug_info = string

(** The result of a successful VDI.attach: this information (eg) can be used to
	connect a VBD backend to a VBD frontend *)
type attach_info = {
	params : string;
	o_direct: bool;
	o_direct_reason : string;
	xenstore_data : (string * string) list;
}

(** Uniquely identifies the contents of a VDI *)
type content_id = string

(** The result of an operation which creates or examines a VDI *)
type vdi_info = {
    vdi: vdi;
    uuid: string option;
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
    cbt_enabled: bool;
    (* missing: workaround via XenAPI *)
    virtual_size: int64;
    physical_utilisation: int64;
    (* xenstore_data: workaround via XenAPI *)
	persistent: bool;
    sm_config: (string * string) list;
}

let default_vdi_info = {
    vdi = "";
    uuid = None;
    content_id = "";
    name_label = "";
    name_description = "";
    ty = "user";
    metadata_of_pool = "";
    is_a_snapshot = false;
    snapshot_time = Xapi_stdext_date.Date.to_string Xapi_stdext_date.Date.never;
    snapshot_of = "";
    read_only = false;
    cbt_enabled = false;
    virtual_size = 0L;
    physical_utilisation = 0L;
    persistent = true;
    sm_config = [];
}

let vdi_info_of_rpc rpc = Rpc.struct_extend rpc (rpc_of_vdi_info default_vdi_info) |> vdi_info_of_rpc

type sr_health = Healthy | Recovering

type sr_info = {
    name_label: string;
    name_description: string;
    total_space: int64;        (** total number of bytes on the storage substrate *)
    free_space: int64;         (** current free space on the storage substrate *)
    clustered: bool;
    health: sr_health;
}

let string_of_vdi_info (x: vdi_info) = Jsonrpc.to_string (rpc_of_vdi_info x)

(** Each VDI is associated with one or more "attached" or "activated" "datapaths". *)
type dp = string

type dp_stat_t = {
	superstate: Vdi_automaton.state;
	dps: (string * Vdi_automaton.state) list;
}

let string_of_dp_stat_t (x: dp_stat_t) = Jsonrpc.to_string (rpc_of_dp_stat_t x)

type probe = {
  srs: (string * sr_info) list; (* SRs we found *)
  uris: string list; (* other uris we found which could be probed recursively *)
}

type probe_result =
  | Raw of string (* SMAPIv1 adapters return arbitrary data *)
  | Probe of probe

module Mirror = struct
	type id = string

	type state =
		| Receiving
		| Sending
		| Copying

	type t = {
		source_vdi : vdi;
		dest_vdi : vdi;
		state : state list;
		failed : bool;
	}

	type mirror_receive_result_vhd_t = {
		mirror_vdi : vdi_info;
		mirror_datapath : dp;
		copy_diffs_from : content_id option;
		copy_diffs_to : vdi;
		dummy_vdi : vdi;
	}

	type mirror_receive_result =
		| Vhd_mirror of mirror_receive_result_vhd_t

	type similars = content_id list
end

type async_result_t =
	| Vdi_info of vdi_info
	| Mirror_id of Mirror.id



module Task = struct
	type id = string

	type async_result = async_result_t

	type completion_t = {
		duration : float;
		result : async_result option
	}

	type state =
		| Pending of float
		| Completed of completion_t
		| Failed of Rpc.t

	type t = {
		id: id;
		dbg: string;
		ctime: float;
		state: state;
		subtasks: (string * state) list;
		debug_info: (string * string) list;
		backtrace: string;
	}
end

module Dynamic = struct
	type id =
		| Task of Task.id
		| Vdi of vdi
		| Dp of dp
		| Mirror of Mirror.id

	type t =
		| Task_t of Task.id * Task.t
		| Vdi_t of vdi * vdi_info
		| Dp_t of dp * dp_stat_t
		| Mirror_t of Mirror.id * Mirror.t

end

exception Backend_error_with_backtrace of (string * (string list)) (** name * params *)

exception Sr_not_attached of string               (** error: SR must be attached to access VDIs *)
exception Vdi_does_not_exist of string            (** error: the VDI is unknown *)
exception Illegal_transition of (Vdi_automaton.state * Vdi_automaton.state) (** This operation implies an illegal state transition *)
exception Backend_error of (string * (string list)) (** error: of the form SR_BACKEND_FAILURE *)
exception Does_not_exist of (string * string)
exception Cancelled of string
exception Redirect of string option
exception Sr_attached of string
exception Unimplemented of string
exception Duplicated_key of string
exception No_storage_plugin_for_sr of string
exception Content_ids_do_not_match of (string * string)
exception Missing_configuration_parameter of string

type query_result = {
	driver: string;
	name: string;
	description: string;
	vendor: string;
	copyright: string;
	version: string;
	required_api_version: string;
	features: string list;
	configuration: (string * string) list;
	required_cluster_stack: string list;
}

module Query = struct
	(** [query ()] returns information about this storage driver *)
	external query: dbg:string -> query_result = ""

	(** [diagnostics ()] returns diagnostic information about this storage driver *)
	external diagnostics: dbg:string -> string = ""
end

module DP = struct
	(** Functions which create/destroy (or register/unregister) dps *)

	(** [create task id]: creates and returns a dp *)
	external create: dbg:debug_info -> id:string -> dp = ""

	(** [destroy task id]: frees any resources associated with [id] and destroys it.
		This will typically do any needed VDI.detach, VDI.deactivate cleanup. *)
	external destroy: dbg:debug_info -> dp:dp -> allow_leak:bool -> unit = ""


	(** [attach_info task sr vdi dp]: returns the params of the dp (the return value of VDI.attach) *)
	external attach_info: dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> attach_info = ""

	(** [diagnostics ()]: returns a printable set of diagnostic information,
		typically including lists of all registered datapaths and their allocated
		resources. *)
	external diagnostics: unit -> string = ""

	(** [stat_vdi task sr vdi ()] returns the state of the given VDI from the point of view of
        each dp as well as the overall superstate. *)
	external stat_vdi: dbg:debug_info -> sr:sr -> vdi:vdi -> unit -> dp_stat_t = ""
end

module SR = struct
	(** Functions which manipulate SRs *)

	(** [create dbg sr name_label name_description device_config physical_size] creates an sr with id [sr] *)
	external create : dbg:debug_info -> sr:sr -> name_label:string -> name_description:string -> device_config:(string * string) list -> physical_size:int64 -> (string * string) list = ""

	(** [set_name_label sr new_name_label] updates the name_label of SR [sr]. *)
	external set_name_label : dbg:debug_info -> sr:sr -> new_name_label:string -> unit = ""

	(** [set_name_description sr new_name_description] updates the name_description of SR [sr]. *)
	external set_name_description : dbg:debug_info -> sr:sr -> new_name_description:string -> unit = ""

	(** [probe dbg queue device_config sm_config] searches on the storage device for SRs of queue [queue] *)
	external probe : dbg:debug_info -> queue:string -> device_config:(string * string) list -> sm_config:(string * string) list -> probe_result = ""

	(** [attach task sr]: attaches the SR *)
    external attach : dbg:debug_info -> sr:sr -> device_config:(string * string) list -> unit = ""

	(** [detach task sr]: detaches the SR, first detaching and/or deactivating any
		active VDIs. This may fail with Sr_not_attached, or any error from VDI.detach
		or VDI.deactivate. *)
    external detach : dbg:debug_info -> sr:sr -> unit = ""

	(** [reset task sr]: declares that the SR has been completely reset, e.g. by
		rebooting the VM hosting the SR backend. *)
	external reset : dbg:debug_info -> sr:sr ->  unit = ""

	(** [destroy sr]: destroys (i.e. makes unattachable and unprobeable) the [sr],
		first detaching and/or deactivating any active VDIs. This may fail with
		Sr_not_attached, or any error from VDI.detach or VDI.deactivate. *)
	external destroy : dbg:debug_info -> sr:sr -> unit = ""

	(** [scan task sr] returns a list of VDIs contained within an attached SR *)
	external scan: dbg:debug_info -> sr:sr -> vdi_info list = ""

	(** [update_snapshot_info_src sr vdi url dest dest_vdi snapshot_pairs]
	 *  updates the fields is_a_snapshot, snapshot_time and snapshot_of for a
	 *  list of snapshots on a remote SR. *)
	external update_snapshot_info_src: dbg:debug_info ->
		sr:sr -> vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi ->
		snapshot_pairs:(vdi * vdi) list -> unit = ""

	(** [update_snapshot_info_dest sr vdi dest src_vdi snapshot_pairs]
	 *  updates the fields is_a_snapshot, snapshot_time and snapshot_of for a
	 *  list of snapshots on a local SR. Typically, vdi will be a mirror of
	 *  src_vdi, and for each item in snapshot_pairs the first will be a copy
	 *  of the second. *)
	external update_snapshot_info_dest: dbg:debug_info ->
		sr:sr -> vdi:vdi -> src_vdi:vdi_info ->
		snapshot_pairs:(vdi * vdi_info) list -> unit = ""

	(** [stat task sr] returns instantaneous SR-level statistics *)
	external stat: dbg:debug_info -> sr:sr -> sr_info = ""

	(** [list task] returns the list of currently attached SRs *)
	external list: dbg:debug_info -> sr list = ""
end

module VDI = struct
	(** Functions which operate on particular VDIs.
		These functions are all idempotent from the point of view of a given [dp]. *)

	(** [create task sr vdi_info] creates a new VDI in [sr] using [vdi_info]. Some
        fields in the [vdi_info] may be modified (e.g. rounded up), so the function
        returns the vdi_info which was used. *)
	external create : dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info = ""

	(** [set_name_label sr vdi new_name_label] updates the name_label of VDI [vdi] in SR [sr]. *)
	external set_name_label : dbg:debug_info -> sr:sr -> vdi:vdi -> new_name_label:string -> unit = ""

	(** [set_name_description sr vdi new_name_description] updates the name_description of VDI [vdi] in SR [sr]. *)
	external set_name_description : dbg:debug_info -> sr:sr -> vdi:vdi -> new_name_description:string -> unit = ""

	(** [snapshot task sr vdi_info] creates a new VDI which is a snapshot of [vdi_info] in [sr] *)
	external snapshot : dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info = ""

	(** [clone task sr vdi_info] creates a new VDI which is a clone of [vdi_info] in [sr] *)
	external clone : dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info = ""

	(** [resize task sr vdi new_size] makes a VDI's virtual_size  at least [new_size] bytes.
	    The function returns the new virtual_size which may be bigger (but not less than)
	    requested. *)
	external resize : dbg:debug_info -> sr:sr -> vdi:vdi -> new_size:int64 -> int64 = ""

    (** [destroy task sr vdi] removes [vdi] from [sr] *)
    external destroy : dbg:debug_info -> sr:sr -> vdi:vdi -> unit = ""

	(** [stat dbg sr vdi] returns information about VDI [vdi] in SR [sr] *)
	external stat : dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info = ""

	(** [introduce dbg sr uuid sm_config location] checks that a VDI exists and returns info about it *)
	external introduce : dbg:debug_info -> sr:sr -> uuid:string -> sm_config:(string * string) list -> location:string -> vdi_info = ""

	(** [set_persistent dbg sr vdi persistent] sets [vdi]'s persistent flag to [persistent] *)
	external set_persistent : dbg:debug_info -> sr:sr -> vdi:vdi -> persistent:bool -> unit = ""

	(** [epoch_begin sr vdi persistent] declares that [vdi] is about to be added to a starting/rebooting VM.
            This is not called over suspend/resume or migrate.
	    If [persistent] is false, then changes to the disk will be erased when the VM shuts down. *)
	external epoch_begin : dbg:debug_info -> sr:sr -> vdi:vdi -> persistent:bool -> unit = ""

	(** [attach task dp sr vdi read_write] returns the [params] for a given
		[vdi] in [sr] which can be written to if (but not necessarily only if) [read_write]
		is true *)
	external attach : dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> attach_info = ""

	(** [activate task dp sr vdi] signals the desire to immediately use [vdi].
		This client must have called [attach] on the [vdi] first. *)
    external activate : dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit = ""

	(** [deactivate task dp sr vdi] signals that this client has stopped reading (and writing)
		[vdi]. *)
    external deactivate : dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit = ""

	(** [detach task dp sr vdi] signals that this client no-longer needs the [attach_info]
		to be valid. *)
    external detach : dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit = ""

	(** [epoch_end sr vdi] declares that [vdi] is about to be removed from a shutting down/rebooting VM.
            This is not called over suspend/resume or migrate. *)
	external epoch_end : dbg:debug_info -> sr:sr -> vdi:vdi -> unit = ""

    (** [get_url task sr vdi] returns a URL suitable for accessing disk data directly. *)
    external get_url : dbg:debug_info -> sr:sr -> vdi:vdi -> string = ""

	(** [similar_content task sr vdi] returns a list of VDIs which have similar content to [vdi] *)
	external similar_content : dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info list = ""

	(** [get_by_name task sr name] returns the vdi within [sr] with [name] *)
	external get_by_name : dbg:debug_info -> sr:sr -> name:string -> vdi_info = ""

	(** [set_content_id task sr vdi content_id] tells the storage backend that a VDI has an updated [content_id] *)
	external set_content_id : dbg:debug_info -> sr:sr -> vdi:vdi -> content_id:content_id -> unit = ""

    (** [compose task sr vdi1 vdi2] layers the updates from [vdi2] onto [vdi1], modifying [vdi2] *)
    external compose : dbg:debug_info -> sr:sr -> vdi1:vdi -> vdi2:vdi -> unit = ""

	(** [add_to_sm_config dbg sr vdi key value] associates [value] to the [key] in [vdi] sm-config *)
	external add_to_sm_config: dbg:debug_info -> sr:sr -> vdi:vdi -> key:string -> value:string -> unit = ""

	(** [remove_from_sm_config dbg sr vdi key] remove [key] from [vdi] sm-config *)
	external remove_from_sm_config: dbg:debug_info -> sr:sr -> vdi:vdi -> key:string -> unit = ""

  (** [enable_cbt dbg sr vdi] enables changed block tracking for [vdi] *)
  external enable_cbt: dbg:debug_info -> sr:sr -> vdi:vdi -> unit = ""

  (** [disable_cbt dbg sr vdi] disables changed block tracking for [vdi] *)
  external disable_cbt: dbg:debug_info -> sr:sr -> vdi:vdi -> unit = ""

  (** [data_destroy dbg sr vdi] deletes the data of the snapshot [vdi] without deleting its changed block tracking metadata *)
  external data_destroy: dbg:debug_info -> sr:sr -> vdi:vdi -> unit = ""

  (** [list_changed_blocks dbg sr vdi_from vdi_to] returns the blocks that have changed between [vdi_from] and [vdi_to] as a base64-encoded bitmap string *)
  external list_changed_blocks: dbg:debug_info -> sr:sr -> vdi_from:vdi -> vdi_to:vdi -> string = ""

end

(** [get_by_name task name] returns a vdi with [name] (which may be in any SR) *)
external get_by_name : dbg:debug_info -> name:string -> sr * vdi_info = ""

module DATA = struct

	(** [copy_into task sr vdi url sr2] copies the data from [vdi] into a remote system [url]'s [sr2] *)
	external copy_into : dbg:debug_info -> sr:sr -> vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi -> Task.id = ""

	external copy : dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id = ""


	module MIRROR = struct
		(** [start task sr vdi url sr2] creates a VDI in remote [url]'s [sr2] and writes
			data synchronously. It returns the id of the VDI.*)
		external start : dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id = ""

		(** [stop task sr vdi] stops mirroring local [vdi] *)
		external stop : dbg:debug_info -> id:Mirror.id -> unit = ""

		external stat : dbg:debug_info -> id:Mirror.id -> Mirror.t = ""

		(** Called on the receiving end *)
		external receive_start : dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> id:Mirror.id -> similar:Mirror.similars ->
			Mirror.mirror_receive_result = ""

		external receive_finalize : dbg:debug_info -> id:Mirror.id -> unit = ""

		external receive_cancel : dbg:debug_info -> id:Mirror.id -> unit = ""

		external list : dbg:debug_info -> (Mirror.id * Mirror.t) list = ""
	end


end

module Policy = struct
	external get_backend_vm: dbg:debug_info -> vm:string -> sr:sr -> vdi:vdi -> string = ""
end

module TASK = struct
	external stat: dbg:debug_info -> task:Task.id -> Task.t = ""
	external cancel: dbg:debug_info -> task:Task.id -> unit = ""
	external destroy: dbg:debug_info -> task:Task.id -> unit = ""
	external list: dbg:debug_info -> Task.t list = ""
end

module UPDATES = struct
	external get: dbg:debug_info -> from:string -> timeout:int option -> Dynamic.id list * string = ""
end
