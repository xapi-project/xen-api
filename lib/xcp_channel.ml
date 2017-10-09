type t = Unix.file_descr

let file_descr_of_t t = t
let t_of_file_descr t = t

[@@@ocaml.warning "-34"]
type protocols = Xcp_channel_protocol.t list [@@deriving rpc]

let rpc_of_t fd =
  let protocols = Posix_channel.send fd in
  rpc_of_protocols protocols

let t_of_rpc x =
  let protocols = protocols_of_rpc x in
  Posix_channel.receive protocols

