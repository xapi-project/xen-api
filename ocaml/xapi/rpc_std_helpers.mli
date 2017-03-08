(** Helpers to marshal and unmarshal Rpc.Dict into Hashtbl *)

val rpc_of_hashtbl : rpc_of:('a -> Rpc.t) -> (string, 'a) Hashtbl.t -> Rpc.t

val hashtbl_of_rpc : of_rpc:(Rpc.t -> 'a) -> Rpc.t -> (string, 'a) Hashtbl.t
