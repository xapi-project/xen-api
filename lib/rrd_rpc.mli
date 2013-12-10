val dict_of_rpc: rpc:Rpc.t -> (string * Rpc.t) list

val list_of_rpc: rpc:Rpc.t -> Rpc.t list

val assoc_opt: key:string -> default:string -> (string * Rpc.t) list -> string

val ds_ty_of_string: string -> Rrd.ds_type

val owner_of_string: string -> Rrd.ds_owner
