val global_id : int64 ref
val global_mutex : Lwt_mutex.t
val get_id : unit -> int64 Lwt.t
val really_read_string : Lwt_unix.file_descr -> int -> string Lwt.t
val bitstring_of_file_descr_max :
  Lwt_unix.file_descr -> int -> Bitstring.bitstring Lwt.t
module NbdRpc :
  sig
    type transport = Lwt_unix.file_descr
    type id = int64
    type request_hdr = Nbd.Request.t
    type request_body = string option
    type response_hdr = Nbd.Reply.t
    type response_body = string option
    val recv_hdr : Lwt_unix.file_descr -> (int64 option * Nbd.Reply.t) Lwt.t
    val recv_body :
      Lwt_unix.file_descr ->
      Nbd.Request.t -> Nbd.Reply.t -> string option Lwt.t
    val send_one :
      Lwt_unix.file_descr -> Nbd.Request.t -> string option -> unit Lwt.t
    val id_of_request : Nbd.Request.t -> int64
    val handle_unrequested_packet : 'a -> 'b -> unit Lwt.t
  end
module Mux :
  sig
    exception Unexpected_id of NbdRpc.id
    exception Shutdown
    type client =
      Lwt_mux.Mux(NbdRpc).client = {
      transport : NbdRpc.transport;
      outgoing_mutex : Lwt_mutex.t;
      id_to_wakeup :
        (NbdRpc.id,
         NbdRpc.request_hdr *
         (NbdRpc.response_hdr * NbdRpc.response_body) Lwt.u)
        Hashtbl.t;
      mutable dispatcher_thread : unit Lwt.t;
      mutable dispatcher_shutting_down : bool;
    }
    val dispatcher : client -> unit Lwt.t
    val rpc :
      NbdRpc.request_hdr ->
      NbdRpc.request_body ->
      client -> (NbdRpc.response_hdr * NbdRpc.response_body) Lwt.t
    val create : NbdRpc.transport -> client Lwt.t
  end
type t = Mux.client
val negotiate :
  NbdRpc.transport -> (Mux.client * int64 * Nbd.flag list) Lwt.t
val connect : string -> int -> (Mux.client * int64 * Nbd.flag list) Lwt.t
val write : Mux.client -> string -> int64 -> unit Lwt.t
val read : Mux.client -> int64 -> int32 -> string Lwt.t
