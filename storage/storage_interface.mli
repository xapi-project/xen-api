val service_name : string
type query_result = {
  name : string;
  vendor : string;
  version : string;
  features : string list;
}
val rpc_of_query_result : query_result -> Rpc.t
val query_result_of_rpc : Rpc.t -> query_result
type sr = string
val rpc_of_sr : string -> Rpc.t
val sr_of_rpc : Rpc.t -> string
type vdi = string
val rpc_of_vdi : string -> Rpc.t
val vdi_of_rpc : Rpc.t -> string
type debug_info = string
val rpc_of_debug_info : string -> Rpc.t
val debug_info_of_rpc : Rpc.t -> string
type attach_info = {
  params : string;
  xenstore_data : (string * string) list;
}
val rpc_of_attach_info : attach_info -> Rpc.t
val attach_info_of_rpc : Rpc.t -> attach_info
type content_id = string
val rpc_of_content_id : string -> Rpc.t
val content_id_of_rpc : Rpc.t -> string
type vdi_info = {
  vdi : vdi;
  sr : sr;
  content_id : content_id;
  name_label : string;
  name_description : string;
  ty : string;
  metadata_of_pool : string;
  is_a_snapshot : bool;
  snapshot_time : string;
  snapshot_of : vdi;
  read_only : bool;
  virtual_size : int64;
  physical_utilisation : int64;
}
val rpc_of_vdi_info : vdi_info -> Rpc.t
val vdi_info_of_rpc : Rpc.t -> vdi_info
val string_of_vdi_info : vdi_info -> string
type dp = string
val rpc_of_dp : string -> Rpc.t
val dp_of_rpc : Rpc.t -> string
type stat_t = {
  superstate : Vdi_automaton.state;
  dps : (string * Vdi_automaton.state) list;
}
val rpc_of_stat_t : stat_t -> Rpc.t
val stat_t_of_rpc : Rpc.t -> stat_t
val string_of_stat_t : stat_t -> string
module Mirror :
  sig
    type id = string
    val rpc_of_id : string -> Rpc.t
    val id_of_rpc : Rpc.t -> string
    type state = Receiving | Sending
    val rpc_of_state : state -> Rpc.t
    val state_of_rpc : Rpc.t -> state
    type t = {
      source_vdi : vdi;
      dest_vdi : vdi;
      state : state list;
      failed : bool;
    }
    val rpc_of_t : t -> Rpc.t
    val t_of_rpc : Rpc.t -> t
    type mirror_receive_result_vhd_t = {
      mirror_vdi : vdi_info;
      mirror_datapath : dp;
      copy_diffs_from : content_id option;
      copy_diffs_to : vdi;
    }
    val rpc_of_mirror_receive_result_vhd_t :
      mirror_receive_result_vhd_t -> Rpc.t
    val mirror_receive_result_vhd_t_of_rpc :
      Rpc.t -> mirror_receive_result_vhd_t
    type mirror_receive_result = Vhd_mirror of mirror_receive_result_vhd_t
    val rpc_of_mirror_receive_result : mirror_receive_result -> Rpc.t
    val mirror_receive_result_of_rpc : Rpc.t -> mirror_receive_result
    type similars = content_id list
    val rpc_of_similars : string list -> Rpc.t
    val similars_of_rpc : Rpc.t -> string list
  end
type async_result_t = Vdi_info of vdi_info | Mirror_id of Mirror.id
val rpc_of_async_result_t : async_result_t -> Rpc.t
val async_result_t_of_rpc : Rpc.t -> async_result_t
module Task :
  sig
    type id = string
    val rpc_of_id : string -> Rpc.t
    val id_of_rpc : Rpc.t -> string
    type async_result = async_result_t
    val rpc_of_async_result : async_result_t -> Rpc.t
    val async_result_of_rpc : Rpc.t -> async_result_t
    type completion_t = { duration : float; result : async_result option; }
    val rpc_of_completion_t : completion_t -> Rpc.t
    val completion_t_of_rpc : Rpc.t -> completion_t
    type state =
        Pending of float
      | Completed of completion_t
      | Failed of Rpc.t
    val rpc_of_state : state -> Rpc.t
    val state_of_rpc : Rpc.t -> state
    type t = {
      id : id;
      debug_info : string;
      ctime : float;
      state : state;
      subtasks : (string * state) list;
    }
    val rpc_of_t : t -> Rpc.t
    val t_of_rpc : Rpc.t -> t
  end
module Dynamic :
  sig
    type id = Task of Task.id | Vdi of vdi | Dp of dp | Mirror of Mirror.id
    val rpc_of_id : id -> Rpc.t
    val id_of_rpc : Rpc.t -> id
    type t =
        Task_t of Task.id * Task.t
      | Vdi_t of vdi * vdi_info
      | Dp_t of dp * stat_t
      | Mirror_t of Mirror.id * Mirror.t
    val rpc_of_t : t -> Rpc.t
    val t_of_rpc : Rpc.t -> t
  end
module Driver_info :
  sig
    type t = {
      uri : string;
      name : string;
      description : string;
      vendor : string;
      copyright : string;
      version : string;
      required_api_version : string;
      capabilities : string list;
      configuration : (string * string) list;
    }
    val rpc_of_t : t -> Rpc.t
    val t_of_rpc : Rpc.t -> t
    type ts = t list
    val rpc_of_ts : t list -> Rpc.t
    val ts_of_rpc : Rpc.t -> t list
  end
module DP : sig  end
module SR : sig  end
module VDI : sig  end
module DATA : sig module MIRROR : sig  end end
module TASK : sig  end
module UPDATES : sig  end
type __exn_ty8 = Vdi_automaton.state * Vdi_automaton.state
type __exn_ty7 = string * string list
type __exn_ty5 = string * string
type __exn_ty4 = string
type __exn_ty3 = string option
type __exn_ty2 = string
type __exn_ty1 = string * int * int
type __exn_ty0 = string
exception Sr_not_attached
exception Vdi_does_not_exist
exception Illegal_transition of __exn_ty8
exception Backend_error of __exn_ty7
exception Unimplemented
exception Does_not_exist of __exn_ty5
exception Cancelled of __exn_ty4
exception Redirect of __exn_ty3
exception Unknown_RPC of __exn_ty2
exception Message_param_count_mismatch of __exn_ty1
exception Internal_error of __exn_ty0
module Exception :
  sig
    type exnty =
        Internal_error of string
      | Message_param_count_mismatch of (string * int * int)
      | Unknown_RPC of string
      | Redirect of string option
      | Cancelled of string
      | Does_not_exist of (string * string)
      | Unimplemented
      | Backend_error of (string * string list)
      | Illegal_transition of (Vdi_automaton.state * Vdi_automaton.state)
      | Vdi_does_not_exist
      | Sr_not_attached
    val rpc_of_exnty : exnty -> Rpc.t
    val exnty_of_rpc : Rpc.t -> exnty
  end
val exnty_of_exn : exn -> Exception.exnty
val exn_of_exnty : Exception.exnty -> exn
module Args :
  sig
    module Query :
      sig
        val rpc_of___x1__ : 'a -> Rpc.t
        val __x1___of_rpc : Rpc.t -> unit
        type response = query_result
        val rpc_of_response : query_result -> Rpc.t
        val response_of_rpc : Rpc.t -> query_result
        val call_of_query : 'a -> Rpc.call
      end
    module DP :
      sig
        module Create :
          sig
            type request = { dbg : debug_info; id : string; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = dp
            val rpc_of_response : string -> Rpc.t
            val response_of_rpc : Rpc.t -> string
            val call_of_create : dbg:debug_info -> id:string -> Rpc.call
          end
        module Destroy :
          sig
            type request = { dbg : debug_info; dp : dp; allow_leak : bool; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_destroy :
              dbg:debug_info -> dp:dp -> allow_leak:bool -> Rpc.call
          end
        module Attach_info :
          sig
            type request = { dbg : debug_info; sr : sr; vdi : vdi; dp : dp; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = attach_info
            val rpc_of_response : attach_info -> Rpc.t
            val response_of_rpc : Rpc.t -> attach_info
            val call_of_attach_info :
              dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> Rpc.call
          end
        module Diagnostics :
          sig
            val rpc_of___x1__ : 'a -> Rpc.t
            val __x1___of_rpc : Rpc.t -> unit
            type response = string
            val rpc_of_response : string -> Rpc.t
            val response_of_rpc : Rpc.t -> string
            val call_of_diagnostics : 'a -> Rpc.call
          end
      end
    module SR :
      sig
        module Attach :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              device_config : (string * string) list;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_attach :
              dbg:debug_info ->
              sr:sr -> device_config:(string * string) list -> Rpc.call
          end
        module Detach :
          sig
            type request = { dbg : debug_info; sr : sr; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_detach : dbg:debug_info -> sr:sr -> Rpc.call
          end
        module Reset :
          sig
            type request = { dbg : debug_info; sr : sr; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_reset : dbg:debug_info -> sr:sr -> Rpc.call
          end
        module Destroy :
          sig
            type request = { dbg : debug_info; sr : sr; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_destroy : dbg:debug_info -> sr:sr -> Rpc.call
          end
        module Scan :
          sig
            type request = { dbg : debug_info; sr : sr; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = vdi_info list
            val rpc_of_response : vdi_info list -> Rpc.t
            val response_of_rpc : Rpc.t -> vdi_info list
            val call_of_scan : dbg:debug_info -> sr:sr -> Rpc.call
          end
        module List :
          sig
            type request = { dbg : debug_info; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = sr list
            val rpc_of_response : string list -> Rpc.t
            val response_of_rpc : Rpc.t -> string list
            val call_of_list : dbg:debug_info -> Rpc.call
          end
      end
    module VDI :
      sig
        module Create :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              vdi_info : vdi_info;
              params : (string * string) list;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = vdi_info
            val rpc_of_response : vdi_info -> Rpc.t
            val response_of_rpc : Rpc.t -> vdi_info
            val call_of_create :
              dbg:debug_info ->
              sr:sr ->
              vdi_info:vdi_info -> params:(string * string) list -> Rpc.call
          end
        module Snapshot :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              vdi : vdi;
              vdi_info : vdi_info;
              params : (string * string) list;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = vdi_info
            val rpc_of_response : vdi_info -> Rpc.t
            val response_of_rpc : Rpc.t -> vdi_info
            val call_of_snapshot :
              dbg:debug_info ->
              sr:sr ->
              vdi:vdi ->
              vdi_info:vdi_info -> params:(string * string) list -> Rpc.call
          end
        module Clone :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              vdi : vdi;
              vdi_info : vdi_info;
              params : (string * string) list;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = vdi_info
            val rpc_of_response : vdi_info -> Rpc.t
            val response_of_rpc : Rpc.t -> vdi_info
            val call_of_clone :
              dbg:debug_info ->
              sr:sr ->
              vdi:vdi ->
              vdi_info:vdi_info -> params:(string * string) list -> Rpc.call
          end
        module Destroy :
          sig
            type request = { dbg : debug_info; sr : sr; vdi : vdi; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_destroy :
              dbg:debug_info -> sr:sr -> vdi:vdi -> Rpc.call
          end
        module Attach :
          sig
            type request = {
              dbg : debug_info;
              dp : dp;
              sr : sr;
              vdi : vdi;
              read_write : bool;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = attach_info
            val rpc_of_response : attach_info -> Rpc.t
            val response_of_rpc : Rpc.t -> attach_info
            val call_of_attach :
              dbg:debug_info ->
              dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> Rpc.call
          end
        module Activate :
          sig
            type request = { dbg : debug_info; dp : dp; sr : sr; vdi : vdi; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_activate :
              dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> Rpc.call
          end
        module Stat :
          sig
            type request = { dbg : debug_info; sr : sr; vdi : vdi; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            val rpc_of___x4__ : 'a -> Rpc.t
            val __x4___of_rpc : Rpc.t -> unit
            type response = stat_t
            val rpc_of_response : stat_t -> Rpc.t
            val response_of_rpc : Rpc.t -> stat_t
            val call_of_stat :
              dbg:debug_info -> sr:sr -> vdi:vdi -> 'a -> Rpc.call
          end
        module Deactivate :
          sig
            type request = { dbg : debug_info; dp : dp; sr : sr; vdi : vdi; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_deactivate :
              dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> Rpc.call
          end
        module Detach :
          sig
            type request = { dbg : debug_info; dp : dp; sr : sr; vdi : vdi; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_detach :
              dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> Rpc.call
          end
        module Get_url :
          sig
            type request = { dbg : debug_info; sr : sr; vdi : vdi; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = string
            val rpc_of_response : string -> Rpc.t
            val response_of_rpc : Rpc.t -> string
            val call_of_get_url :
              dbg:debug_info -> sr:sr -> vdi:vdi -> Rpc.call
          end
        module Similar_content :
          sig
            type request = { dbg : debug_info; sr : sr; vdi : vdi; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = vdi_info list
            val rpc_of_response : vdi_info list -> Rpc.t
            val response_of_rpc : Rpc.t -> vdi_info list
            val call_of_similar_content :
              dbg:debug_info -> sr:sr -> vdi:vdi -> Rpc.call
          end
        module Get_by_name :
          sig
            type request = { dbg : debug_info; sr : sr; name : string; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = vdi_info
            val rpc_of_response : vdi_info -> Rpc.t
            val response_of_rpc : Rpc.t -> vdi_info
            val call_of_get_by_name :
              dbg:debug_info -> sr:sr -> name:string -> Rpc.call
          end
        module Set_content_id :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              vdi : vdi;
              content_id : content_id;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_set_content_id :
              dbg:debug_info ->
              sr:sr -> vdi:vdi -> content_id:content_id -> Rpc.call
          end
        module Compose :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              vdi1 : vdi;
              vdi2 : vdi;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_compose :
              dbg:debug_info -> sr:sr -> vdi1:vdi -> vdi2:vdi -> Rpc.call
          end
      end
    module Get_by_name :
      sig
        type request = { dbg : debug_info; name : string; }
        val rpc_of_request : request -> Rpc.t
        val request_of_rpc : Rpc.t -> request
        type response = vdi_info
        val rpc_of_response : vdi_info -> Rpc.t
        val response_of_rpc : Rpc.t -> vdi_info
        val call_of_get_by_name : dbg:debug_info -> name:string -> Rpc.call
      end
    module DATA :
      sig
        module Copy_into :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              vdi : vdi;
              url : string;
              dest : sr;
              dest_vdi : vdi;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = Task.id
            val rpc_of_response : string -> Rpc.t
            val response_of_rpc : Rpc.t -> string
            val call_of_copy_into :
              dbg:debug_info ->
              sr:sr ->
              vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi -> Rpc.call
          end
        module Copy :
          sig
            type request = {
              dbg : debug_info;
              sr : sr;
              vdi : vdi;
              dp : dp;
              url : string;
              dest : sr;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = Task.id
            val rpc_of_response : string -> Rpc.t
            val response_of_rpc : Rpc.t -> string
            val call_of_copy :
              dbg:debug_info ->
              sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Rpc.call
          end
        module MIRROR :
          sig
            module Start :
              sig
                type request = {
                  dbg : debug_info;
                  sr : sr;
                  vdi : vdi;
                  dp : dp;
                  url : string;
                  dest : sr;
                }
                val rpc_of_request : request -> Rpc.t
                val request_of_rpc : Rpc.t -> request
                type response = Task.id
                val rpc_of_response : string -> Rpc.t
                val response_of_rpc : Rpc.t -> string
                val call_of_start :
                  dbg:debug_info ->
                  sr:sr ->
                  vdi:vdi -> dp:dp -> url:string -> dest:sr -> Rpc.call
              end
            module Stop :
              sig
                type request = { dbg : debug_info; id : Mirror.id; }
                val rpc_of_request : request -> Rpc.t
                val request_of_rpc : Rpc.t -> request
                type response = unit
                val rpc_of_response : 'a -> Rpc.t
                val response_of_rpc : Rpc.t -> unit
                val call_of_stop : dbg:debug_info -> id:Mirror.id -> Rpc.call
              end
            module Stat :
              sig
                type request = { dbg : debug_info; id : Mirror.id; }
                val rpc_of_request : request -> Rpc.t
                val request_of_rpc : Rpc.t -> request
                type response = Mirror.t
                val rpc_of_response : Mirror.t -> Rpc.t
                val response_of_rpc : Rpc.t -> Mirror.t
                val call_of_stat : dbg:debug_info -> id:Mirror.id -> Rpc.call
              end
            module Receive_start :
              sig
                type request = {
                  dbg : debug_info;
                  sr : sr;
                  vdi_info : vdi_info;
                  id : Mirror.id;
                  similar : Mirror.similars;
                }
                val rpc_of_request : request -> Rpc.t
                val request_of_rpc : Rpc.t -> request
                type response = Mirror.mirror_receive_result
                val rpc_of_response : Mirror.mirror_receive_result -> Rpc.t
                val response_of_rpc : Rpc.t -> Mirror.mirror_receive_result
                val call_of_receive_start :
                  dbg:debug_info ->
                  sr:sr ->
                  vdi_info:vdi_info ->
                  id:Mirror.id -> similar:Mirror.similars -> Rpc.call
              end
            module Receive_finalize :
              sig
                type request = { dbg : debug_info; id : Mirror.id; }
                val rpc_of_request : request -> Rpc.t
                val request_of_rpc : Rpc.t -> request
                type response = unit
                val rpc_of_response : 'a -> Rpc.t
                val response_of_rpc : Rpc.t -> unit
                val call_of_receive_finalize :
                  dbg:debug_info -> id:Mirror.id -> Rpc.call
              end
            module Receive_cancel :
              sig
                type request = { dbg : debug_info; id : Mirror.id; }
                val rpc_of_request : request -> Rpc.t
                val request_of_rpc : Rpc.t -> request
                type response = unit
                val rpc_of_response : 'a -> Rpc.t
                val response_of_rpc : Rpc.t -> unit
                val call_of_receive_cancel :
                  dbg:debug_info -> id:Mirror.id -> Rpc.call
              end
            module List :
              sig
                type request = { dbg : debug_info; }
                val rpc_of_request : request -> Rpc.t
                val request_of_rpc : Rpc.t -> request
                type response = (Mirror.id * Mirror.t) list
                val rpc_of_response : (string * Mirror.t) list -> Rpc.t
                val response_of_rpc : Rpc.t -> (string * Mirror.t) list
                val call_of_list : dbg:debug_info -> Rpc.call
              end
          end
      end
    module TASK :
      sig
        module Stat :
          sig
            type request = { dbg : debug_info; task : Task.id; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = Task.t
            val rpc_of_response : Task.t -> Rpc.t
            val response_of_rpc : Rpc.t -> Task.t
            val call_of_stat : dbg:debug_info -> task:Task.id -> Rpc.call
          end
        module Cancel :
          sig
            type request = { dbg : debug_info; task : Task.id; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_cancel : dbg:debug_info -> task:Task.id -> Rpc.call
          end
        module Destroy :
          sig
            type request = { dbg : debug_info; task : Task.id; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = unit
            val rpc_of_response : 'a -> Rpc.t
            val response_of_rpc : Rpc.t -> unit
            val call_of_destroy : dbg:debug_info -> task:Task.id -> Rpc.call
          end
        module List :
          sig
            type request = { dbg : debug_info; }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = Task.t list
            val rpc_of_response : Task.t list -> Rpc.t
            val response_of_rpc : Rpc.t -> Task.t list
            val call_of_list : dbg:debug_info -> Rpc.call
          end
      end
    module UPDATES :
      sig
        module Get :
          sig
            type request = {
              dbg : debug_info;
              from : string;
              timeout : int option;
            }
            val rpc_of_request : request -> Rpc.t
            val request_of_rpc : Rpc.t -> request
            type response = Dynamic.id list * string
            val rpc_of_response : Dynamic.id list * string -> Rpc.t
            val response_of_rpc : Rpc.t -> Dynamic.id list * string
            val call_of_get :
              dbg:debug_info -> from:string -> timeout:int option -> Rpc.call
          end
      end
  end
module type RPC = sig val rpc : Rpc.call -> Rpc.response end
module Client :
  functor (R : RPC) ->
    sig
      val query : 'a -> query_result
      module DP :
        sig
          val create : dbg:debug_info -> id:string -> string
          val destroy : dbg:debug_info -> dp:dp -> allow_leak:bool -> unit
          val attach_info :
            dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> attach_info
          val diagnostics : 'a -> string
        end
      module SR :
        sig
          val attach :
            dbg:debug_info ->
            sr:sr -> device_config:(string * string) list -> unit
          val detach : dbg:debug_info -> sr:sr -> unit
          val reset : dbg:debug_info -> sr:sr -> unit
          val destroy : dbg:debug_info -> sr:sr -> unit
          val scan : dbg:debug_info -> sr:sr -> vdi_info list
          val list : dbg:debug_info -> string list
        end
      module VDI :
        sig
          val create :
            dbg:debug_info ->
            sr:sr ->
            vdi_info:vdi_info -> params:(string * string) list -> vdi_info
          val snapshot :
            dbg:debug_info ->
            sr:sr ->
            vdi:vdi ->
            vdi_info:vdi_info -> params:(string * string) list -> vdi_info
          val clone :
            dbg:debug_info ->
            sr:sr ->
            vdi:vdi ->
            vdi_info:vdi_info -> params:(string * string) list -> vdi_info
          val destroy : dbg:debug_info -> sr:sr -> vdi:vdi -> unit
          val attach :
            dbg:debug_info ->
            dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> attach_info
          val activate : dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
          val stat : dbg:debug_info -> sr:sr -> vdi:vdi -> 'a -> stat_t
          val deactivate :
            dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
          val detach : dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
          val get_url : dbg:debug_info -> sr:sr -> vdi:vdi -> string
          val similar_content :
            dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info list
          val get_by_name :
            dbg:debug_info -> sr:sr -> name:string -> vdi_info
          val set_content_id :
            dbg:debug_info ->
            sr:sr -> vdi:vdi -> content_id:content_id -> unit
          val compose :
            dbg:debug_info -> sr:sr -> vdi1:vdi -> vdi2:vdi -> unit
        end
      val get_by_name : dbg:debug_info -> name:string -> vdi_info
      module DATA :
        sig
          val copy_into :
            dbg:debug_info ->
            sr:sr ->
            vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi -> string
          val copy :
            dbg:debug_info ->
            sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> string
          module MIRROR :
            sig
              val start :
                dbg:debug_info ->
                sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> string
              val stop : dbg:debug_info -> id:Mirror.id -> unit
              val stat : dbg:debug_info -> id:Mirror.id -> Mirror.t
              val receive_start :
                dbg:debug_info ->
                sr:sr ->
                vdi_info:vdi_info ->
                id:Mirror.id ->
                similar:Mirror.similars -> Mirror.mirror_receive_result
              val receive_finalize : dbg:debug_info -> id:Mirror.id -> unit
              val receive_cancel : dbg:debug_info -> id:Mirror.id -> unit
              val list : dbg:debug_info -> (string * Mirror.t) list
            end
        end
      module TASK :
        sig
          val stat : dbg:debug_info -> task:Task.id -> Task.t
          val cancel : dbg:debug_info -> task:Task.id -> unit
          val destroy : dbg:debug_info -> task:Task.id -> unit
          val list : dbg:debug_info -> Task.t list
        end
      module UPDATES :
        sig
          val get :
            dbg:debug_info ->
            from:string -> timeout:int option -> Dynamic.id list * string
        end
    end
module type Server_impl =
  sig
    type context
    val query : context -> unit -> query_result
    module DP :
      sig
        val create : context -> dbg:debug_info -> id:string -> dp
        val destroy :
          context -> dbg:debug_info -> dp:dp -> allow_leak:bool -> unit
        val attach_info :
          context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> attach_info
        val diagnostics : context -> unit -> string
      end
    module SR :
      sig
        val attach :
          context ->
          dbg:debug_info ->
          sr:sr -> device_config:(string * string) list -> unit
        val detach : context -> dbg:debug_info -> sr:sr -> unit
        val reset : context -> dbg:debug_info -> sr:sr -> unit
        val destroy : context -> dbg:debug_info -> sr:sr -> unit
        val scan : context -> dbg:debug_info -> sr:sr -> vdi_info list
        val list : context -> dbg:debug_info -> sr list
      end
    module VDI :
      sig
        val create :
          context ->
          dbg:debug_info ->
          sr:sr ->
          vdi_info:vdi_info -> params:(string * string) list -> vdi_info
        val snapshot :
          context ->
          dbg:debug_info ->
          sr:sr ->
          vdi:vdi ->
          vdi_info:vdi_info -> params:(string * string) list -> vdi_info
        val clone :
          context ->
          dbg:debug_info ->
          sr:sr ->
          vdi:vdi ->
          vdi_info:vdi_info -> params:(string * string) list -> vdi_info
        val destroy : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit
        val attach :
          context ->
          dbg:debug_info ->
          dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> attach_info
        val activate :
          context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
        val stat :
          context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit -> stat_t
        val deactivate :
          context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
        val detach :
          context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
        val get_url : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> string
        val similar_content :
          context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info list
        val get_by_name :
          context -> dbg:debug_info -> sr:sr -> name:string -> vdi_info
        val set_content_id :
          context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> content_id:content_id -> unit
        val compose :
          context -> dbg:debug_info -> sr:sr -> vdi1:vdi -> vdi2:vdi -> unit
      end
    val get_by_name : context -> dbg:debug_info -> name:string -> vdi_info
    module DATA :
      sig
        val copy_into :
          context ->
          dbg:debug_info ->
          sr:sr ->
          vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi -> Task.id
        val copy :
          context ->
          dbg:debug_info ->
          sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id
        module MIRROR :
          sig
            val start :
              context ->
              dbg:debug_info ->
              sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id
            val stop : context -> dbg:debug_info -> id:Mirror.id -> unit
            val stat : context -> dbg:debug_info -> id:Mirror.id -> Mirror.t
            val receive_start :
              context ->
              dbg:debug_info ->
              sr:sr ->
              vdi_info:vdi_info ->
              id:Mirror.id ->
              similar:Mirror.similars -> Mirror.mirror_receive_result
            val receive_finalize :
              context -> dbg:debug_info -> id:Mirror.id -> unit
            val receive_cancel :
              context -> dbg:debug_info -> id:Mirror.id -> unit
            val list :
              context -> dbg:debug_info -> (Mirror.id * Mirror.t) list
          end
      end
    module TASK :
      sig
        val stat : context -> dbg:debug_info -> task:Task.id -> Task.t
        val cancel : context -> dbg:debug_info -> task:Task.id -> unit
        val destroy : context -> dbg:debug_info -> task:Task.id -> unit
        val list : context -> dbg:debug_info -> Task.t list
      end
    module UPDATES :
      sig
        val get :
          context ->
          dbg:debug_info ->
          from:string -> timeout:int option -> Dynamic.id list * string
      end
  end
module Server :
  functor (Impl : Server_impl) ->
    sig val process : Impl.context -> Rpc.call -> Rpc.response end
