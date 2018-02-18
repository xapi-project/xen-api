module type RRDD_IMPLEMENTATION = sig

  val has_vm_rrd              : string -> bool

  val push_rrd_local          : string        -> int           -> unit
  val push_rrd_remote         : string        -> string        -> unit
  val remove_rrd              : string        -> unit
  val migrate_rrd             : string option -> string        -> string -> string -> unit
  val send_host_rrd_to_master : string        -> unit
  val backup_rrds             : string option -> unit          -> unit
  val archive_rrd             : string        -> string option -> unit

  val archive_sr_rrd          : string -> string
  val push_sr_rrd             : string -> string -> unit

  val add_host_ds             : string -> unit
  val forget_host_ds          : string -> unit
  val query_possible_host_dss : unit   -> Data_source.t list
  val query_host_ds           : string -> float

  val add_vm_ds               : string -> int                -> string -> unit
  val forget_vm_ds            : string -> string             -> unit
  val query_possible_vm_dss   : string -> Data_source.t list
  val query_vm_ds             : string -> string             -> float

  val add_sr_ds               : string -> string             -> unit
  val forget_sr_ds            : string -> string             -> unit
  val query_possible_sr_dss   : string -> Data_source.t list
  val query_sr_ds             : string -> string             -> float

  val update_use_min_max      : bool   -> unit

  val update_vm_memory_target : int    -> int64 -> unit

  val set_cache_sr            : string -> unit
  val unset_cache_sr          : unit   -> unit

   module Plugin : sig
    val get_header   : unit   -> string
    val get_path     : string -> string
    val register     : string -> Rrd.sampling_frequency -> float
    val deregister   : string -> unit
    val next_reading : string -> float

     module Local : sig
      val register     : string -> Rrd.sampling_frequency -> Rrd_interface.plugin_protocol -> float
      val deregister   : string -> unit
      val next_reading : string -> float
    end

     module Interdomain : sig
       open Rrd_interface
      val register     : interdomain_uid -> interdomain_info -> plugin_protocol -> float
      val deregister   : interdomain_uid -> unit
      val next_reading : interdomain_uid -> float
    end

  end

   module HA : sig
    val enable_and_update : Rrd.Statefile_latency.t list -> float -> float -> unit
    val disable           : unit                         -> unit
  end

   module Deprecated : sig
    val load_rrd : string -> int -> string option -> unit
   end
end

(* Generate empty server before binding API calls *)
module Server = Rrd_interface.RPC_API(Idl.GenServerExn ())

(* -- Server-implementation RPC bindings -- *)
module Make(Impl : RRDD_IMPLEMENTATION) = struct

  let bind () =

    Server.has_vm_rrd              Impl.has_vm_rrd;

    Server.push_rrd_local          Impl.push_rrd_local;
    Server.push_rrd_remote         Impl.push_rrd_remote;
    Server.remove_rrd              Impl.remove_rrd;
    Server.migrate_rrd             Impl.migrate_rrd;
    Server.send_host_rrd_to_master Impl.send_host_rrd_to_master;
    Server.backup_rrds             Impl.backup_rrds;
    Server.archive_rrd             Impl.archive_rrd;

    Server.archive_sr_rrd          Impl.archive_sr_rrd;
    Server.push_sr_rrd             Impl.push_sr_rrd;

    Server.add_host_ds             Impl.add_host_ds;
    Server.forget_host_ds          Impl.forget_host_ds;
    Server.query_possible_host_dss Impl.query_possible_host_dss;
    Server.query_host_ds           Impl.query_host_ds;

    Server.add_vm_ds               Impl.add_vm_ds;
    Server.forget_vm_ds            Impl.forget_vm_ds;
    Server.query_possible_vm_dss   Impl.query_possible_vm_dss;
    Server.query_vm_ds             Impl.query_vm_ds;

    Server.add_sr_ds               Impl.add_sr_ds;
    Server.forget_sr_ds            Impl.forget_sr_ds;
    Server.query_possible_sr_dss   Impl.query_possible_sr_dss;
    Server.query_sr_ds             Impl.query_sr_ds;

    Server.update_use_min_max      Impl.update_use_min_max;
    Server.update_vm_memory_target Impl.update_vm_memory_target;

    Server.set_cache_sr            Impl.set_cache_sr;
    Server.unset_cache_sr          Impl.unset_cache_sr;

    (* module Plugin*)

    Server.Plugin.get_header Impl.Plugin.get_header;
    Server.Plugin.get_path   Impl.Plugin.get_path;

    (* Plugin submodules *)
    Server.Plugin.Local.register     Impl.Plugin.Local.register;
    Server.Plugin.Local.deregister   Impl.Plugin.Local.deregister;
    Server.Plugin.Local.next_reading Impl.Plugin.Local.next_reading;

    Server.Plugin.Interdomain.register     Impl.Plugin.Interdomain.register;
    Server.Plugin.Interdomain.deregister   Impl.Plugin.Interdomain.deregister;
    Server.Plugin.Interdomain.next_reading Impl.Plugin.Interdomain.next_reading;

    Server.Plugin.register     Impl.Plugin.register;
    Server.Plugin.deregister   Impl.Plugin.deregister;
    Server.Plugin.next_reading Impl.Plugin.next_reading;

    (* end Plugin *)

    Server.HA.enable_and_update Impl.HA.enable_and_update;
    Server.HA.disable           Impl.HA.disable;

    Server.Deprecated.load_rrd Impl.Deprecated.load_rrd
end


(* create module to allow server-implementation RPC bindings *)
module Rrd_daemon = Make(Rrdd_server)

