type t = Unix.file_descr

let file_descr_of_t t = t
let t_of_file_descr t = t

type protocol =
  | TCP_proxy of string * int             (** IP, port *)
  | V4V_proxy of int * int                (** domid, port *)
  | Unix_sendmsg of int * string * string (** domid, path, token *)
with rpc

type protocols = protocol list with rpc

let rpc_of_t fd =
  let protocols = Posix_channel.send fd in
  rpc_of_protocols protocols

let t_of_rpc x =
  let protocols = protocols_of_rpc x in
  Posix_channel.receive protocols

