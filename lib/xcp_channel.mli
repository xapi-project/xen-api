type t

val rpc_of_t: t -> Rpc.t
val t_of_rpc: Rpc.t -> t

val file_descr_of_t: t -> Unix.file_descr
val t_of_file_descr: Unix.file_descr -> t
