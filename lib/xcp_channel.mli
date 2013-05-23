type t

val rpc_of_t: t -> Rpc.t
val t_of_rpc: Rpc.t -> t

val file_descr_of_t: t -> Unix.file_descr
val t_of_file_descr: Unix.file_descr -> t

type protocol =
  | TCP_proxy of string * int             (** IP, port *)
  | V4V_proxy of int * int                (** domid, port *)
  | Unix_sendmsg of int * string * string (** domid, path, token *)

val protocol_of_rpc: Rpc.t -> protocol
val rpc_of_protocol: protocol -> Rpc.t

val protocols_of_rpc: Rpc.t -> protocol list
val rpc_of_protocols: protocol list -> Rpc.t
