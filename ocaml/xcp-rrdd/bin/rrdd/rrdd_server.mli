val has_vm_rrd : string -> bool

val push_rrd_local : string -> int -> unit

val push_rrd_remote : string -> string -> unit

val remove_rrd : string -> unit

val migrate_rrd : string option -> string -> string -> string -> unit

val send_host_rrd_to_master : string -> unit

val backup_rrds : string option -> unit -> unit

val save_rrds : unit -> unit

val archive_rrd : string -> string option -> unit

val archive_sr_rrd : string -> string

val push_sr_rrd : string -> string -> unit

val add_host_ds : string -> unit

val forget_host_ds : string -> unit

val query_possible_host_dss : unit -> Data_source.t list

val query_host_ds : string -> float

val dump_host_dss_to_file : string -> unit

val dump_vm_dss_to_file : string -> unit

val add_vm_ds : string -> int -> string -> unit

val forget_vm_ds : string -> string -> unit

val query_possible_vm_dss : string -> Data_source.t list

val query_vm_ds : string -> string -> float

val add_sr_ds : string -> string -> unit

val forget_sr_ds : string -> string -> unit

val query_possible_sr_dss : string -> Data_source.t list

val query_sr_ds : string -> string -> float

val update_use_min_max : bool -> unit

val update_vm_memory_target : int -> int64 -> unit

val set_cache_sr : string -> unit

val unset_cache_sr : unit -> unit

module Plugin : sig
  val base_path : string

  val get_header : unit -> string

  val get_path : string -> string

  val register : string -> Rrd.sampling_frequency -> float

  val deregister : string -> unit

  val next_reading : string -> float

  val read_stats : unit -> (Rrd.ds_owner * Ds.ds) Seq.t

  module Local : sig
    val register :
      string -> Rrd.sampling_frequency -> Rrd_interface.plugin_protocol -> float

    val deregister : string -> unit

    val next_reading : string -> float
  end
end

module HA : sig
  val enable_and_update :
    Rrd_interface.statefile_latency list -> float -> float -> unit

  val disable : unit -> unit
end

module Deprecated : sig
  val load_rrd : string -> int -> string option -> unit
end
