type flag =
    NBD_read_only
  | NBD_send_flush
  | NBD_send_fua
  | NBD_rotational
  | NBD_send_trim

val negotiate : Unix.file_descr -> int64 * flag list

val read : Unix.file_descr -> int64 -> int32 -> string option

val write : Unix.file_descr -> string -> int64 -> int32 option

val connect : string -> int -> Unix.file_descr * int64 * flag list


module Lwt :
  sig
	type t

    val negotiate : Lwt_unix.file_descr -> (t * int64 * flag list) Lwt.t

    val connect :
      string -> int -> (t * int64 * flag list) Lwt.t

    val write : t -> string -> int64 -> unit Lwt.t

    val read : t -> int64 -> int32 -> string Lwt.t
  end

