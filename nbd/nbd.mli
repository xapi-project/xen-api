type flag =
    NBD_read_only
  | NBD_send_flush
  | NBD_send_fua
  | NBD_rotational
  | NBD_send_trim

val negotiate : Unix.file_descr -> int64 * flag list

val read : Unix.file_descr -> int64 -> int32 -> string option

val write : Unix.file_descr -> string -> int64 -> int32 option

val connect : string -> int -> int64 * flag list
