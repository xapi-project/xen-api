val write_block : Tar.Header.t -> (Unix.file_descr -> unit) -> Unix.file_descr -> unit
val write_end : Unix.file_descr -> unit
val skip : Unix.file_descr -> int -> unit
val copy_n : Unix.file_descr -> Unix.file_descr -> int64 -> unit
